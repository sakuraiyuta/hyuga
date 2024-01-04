(require hyrule * :readers *)
(import hyrule.misc *)

(import inspect *)
(import toolz.itertoolz *)
(import lsprotocol.types [CompletionItem
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

(defn decide-kind-by-summary
  [summary]
  (branch (= it (:type summary))
          "defn" CompletionItemKind.Function
          "defclass" CompletionItemKind.Class
          ;; FIXME: pygls CompletionItemKind.Macro is not defined yet.
          "defmacro" CompletionItemKind.Keyword
          else CompletionItemKind.Variable))

(defn decide-kind-by-type
  [typev]
  (cond
    (and (isinstance typev dict)
         (in "type" (typev.keys)))
    (decide-kind-by-summary typev)
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
  [scope typev]
  "TODO: doc"
  (-> (branch (in it scope)
              ; "builtin" CompletionItemKind.Keyword
              "hy-special" CompletionItemKind.Keyword
              ;; FIXME: pygls CompletionItemKind.Macro is not defined yet.
              ; "macro" 118115
              ; "macro" CompletionItemKind.Macro
              else (decide-kind-by-type typev))
      (CompletionItemKind)))

(defn create-item
  [word full-sym/dic]
  "TODO: doc"
  (let [[full-sym dic] full-sym/dic]
    (when (isinstance dic dict)
      (let+ [{docs "docs" typev "type"} dic
             prefix-splitted (.split word ".")
             [scope ns sym] (get-scope/ns/sym full-sym)
             word-ns (module-or-class? prefix-splitted)
             fixed-prefix (if (and ns
                                   (not (.startswith ns "("))
                                   (not (.endswith ns ")")))
                            (-> ns
                                (+ ".")
                                (.replace word-ns "")
                                (.lstrip "."))
                            "")
             insert-text sym
             label f"[{ns}] {sym}"]
        (CompletionItem
          :label label
          :insert_text insert-text
          :detail (fix-dummy docs)
          :kind (decide-kind scope typev))))))

(defn create-items
  [word root-uri doc-uri]
  "TODO: doc"
  (->> (get-candidates word root-uri doc-uri)
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
