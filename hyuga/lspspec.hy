(require hyrule * :readers *)
(import hyrule.misc *)

(import inspect *)
(import toolz.itertoolz *)
(import pygls.lsp.types [CompletionItem
                         CompletionList
                         CompletionOptions
                         CompletionParams
                         CompletionItemKind
                         Hover
                         Range
                         Location
                         Position
                         MarkupContent
                         MarkupKind])
(import re)

(import hyuga.log *)
(import hyuga.api *)
(import hyuga.sym.helper *)

(defn fix-dummy
  [docs [local? True]]
  "test"
  (if local?
    (.replace docs "hyuga.sym.dummy" "local")
    (.replace docs "hyuga.sym.dummy" "")))

(defn decide-kind-by-hyexpr
  [hy-expr]
  (branch (= it (first hy-expr))
          "defn" CompletionItemKind.Keyword
          "defclass" CompletionItemKind.Class
          ;; FIXME: pygls CompletionItemKind.Macro is not defined yet.
          "defmacro" CompletionItemKind.Keyword
          else CompletionItemKind.Variable))

(defn decide-kind-by-type
  [typev]
  (cond
    (isinstance typev Expression)
    (decide-kind-by-hyexpr typev)
    (isbuiltin typev)
    CompletionItemKind.Keyword
    (ismodule typev)
    CompletionItemKind.Module
    (isclass typev)
    CompletionItemKind.Class
    (ismethod typev)
    CompletionItemKind.Method
    (isfunction typev)
    CompletionItemKind.Function
    True
    CompletionItemKind.Variable))

(defn decide-kind
  [ns typev]
  (-> (branch (in it ns)
              "builtin" CompletionItemKind.Keyword
              "hy-special" CompletionItemKind.Keyword
              ;; FIXME: pygls CompletionItemKind.Macro is not defined yet.
              ; "macro" 118115
              ; "macro" CompletionItemKind.Macro
              else (decide-kind-by-type typev))
      int))

(defn create-item
  [word sym/dic]
  "TODO: doc"
  (let [[sym dic] sym/dic]
    (when (isinstance dic dict)
      (let+ [{symkey "sym" docs "docs" typev "type"} dic
             prefix-splitted (.split word ".")
             [scope full-sym] (get-scope/ns symkey)
             [ns sym] (get-ns/sym full-sym)
             insert-text (if (module-or-class? prefix-splitted)
                           sym
                           (fix-dummy full-sym False))]
        (CompletionItem
          :label f"{sym}\t[{(or (fix-dummy ns) (fix-dummy scope))}]"
          :insert_text insert-text
          :detail (fix-dummy docs)
          :kind (decide-kind ns typev))))))

(defn create-items
  [word]
  "TODO: doc"
  (->> (get-candidates word)
       (map #%(create-item word %1))
       (filter #%(is-not None %1))
       list))

(defn create-completion-list
  [items [is-incomplete False]]
  "TODO: doc"
  (logger.debug f"create-completion-list items={(count items)}")
  (CompletionList :is_incomplete is-incomplete
                  :items items))

(defn create-hover
  [docs]
  (Hover
    :contents (MarkupContent
                :kind MarkupKind.PlainText
                :value (fix-dummy docs))))

(defn create-location
  [pos uri]
  "TODO: doc"
  (let [obj-pos (Position :line (-> pos first dec)
                          :character (-> pos second dec))
        obj-range (Range :start obj-pos
                         :end obj-pos)]
    (Location :uri uri
              :range obj-range)))

(defn distinct-locations
  [items]
  "TODO: doc"
  (logger.debug f"distinct-locations: items={items}")
  (setv ret [])
  (for [item items]
    (let [rng item.range
          uri item.uri
          not-exists? (->> ret
                       (filter #%(and (= rng %1.range)
                                      (= uri %1.uri)))
                       tuple
                       count (= 0))]
      (when not-exists?
        (ret.append item))))
  ret)

(defn create-location-list
  [sym/vals]
  "TODO: doc"
  (->> sym/vals
       (map #%(let+ [{pos "pos" uri "uri"} (second %1)]
                (when (and pos uri)
                  (create-location pos uri))))
       (filter #%(is-not %1 None))
       distinct-locations
       list))
