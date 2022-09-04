(require hyrule * :readers *)
(import hyrule.collections [prewalk])
(import hyrule.iterables [butlast drop-last])
(import hyrule.control *)

(import toolz.itertoolz *)

(import sys)
(import types [ModuleType])
(import collections.abc [Iterable])

(import pygls.server [LanguageServer])

(import hyuga.log *)
(import hyuga.global [$GLOBAL])
(import hyuga.importer [DummyImporter])
(import hy.models [Lazy Expression])

(defn filter-add-targets
  [sym-py/vals]
  "TODO: doc"
  (->> sym-py/vals
       (map sym-py/val->sym-hy/val)
       (filter not-in-$SYM?)
       (filter add-sym?)))

(defn add-sym!
  [sym-hy/val scope]
  "TODO: doc"
  ($GLOBAL.add-$SYMS (first sym-hy/val)
                     (second sym-hy/val)
                     scope
                     (create-docs (first sym-hy/val)
                                  (second sym-hy/val)
                                  scope)))

(defn not-in-$SYM?
  [x]
  (not (in (first x) (.keys ($GLOBAL.get-$SYMS)))))

(defn sym-py/val->sym-hy/val
  [sym-py/val]
  (tuple [(-> sym-py/val first sym-py->hy)
          (second sym-py/val)]))

(defn sym-py->hy
  [sym-py]
  (-> sym-py hy.unmangle))

(defn sym-hy->py
  [sym-hy]
  (-> sym-hy hy.mangle))

(defn -get-macro-doc
  [sym-hy symtype]
  "Get macro documents.
  FIXME: So dirty hack!"
  (->> sym-hy (.format
                "(do
                (import io)
                (import contextlib [redirect-stdout])
                (with [buf (io.StringIO)
                _ (redirect-stdout buf)]
                (doc {})
                (buf.getvalue)))")
       hy.read
       (hy.eval :locals {(sym-hy->py sym-hy) symtype})))

(defn -create-fn-docs
  [sym-hy symtype scope orig-docs]
  "TODO: doc"
  (.format "{} {}\n\t{}\n\n{}"
           sym-hy (str symtype) scope orig-docs))

(defn create-docs
  [sym-hy symtype scope]
  "TODO: doc"
  (try
    (-create-fn-docs
      sym-hy symtype scope (or symtype.__doc__ "No docs."))
    (except
      [e BaseException]
      (logger.debug
        (.format "cannot read __doc__. try macro docs. e={}"
                 e))
      (-create-fn-docs sym-hy
                       symtype
                       scope
                       (-get-macro-doc sym-hy symtype)))))

(defn is-eval-target?
  [form]
  (and (isinstance form Expression)
       ;; TODO: get config from server params
       (or (.startswith (first form) "require")
           (.startswith (first form) "def")
           (.startswith (first form) "setv")
           (.startswith (first form) "import"))))

(defn add-sym?
  [sym-val]
  (and (not (.startswith (first sym-val) "_hy-let-evaled-"))
       (!= (first sym-val) "-hyuga-eval-form")
       (!= (first sym-val) "hyuga-dummy")))

(defn -walk-eval!
  [-hyuga-eval-form]
  "TODO: doc"
  (try
    (when (is-eval-target? -hyuga-eval-form)
      (logger.debug (.format "found def/import: ({} {})"
                             (first -hyuga-eval-form)
                             (second -hyuga-eval-form)))
      (let [evaled (hy.eval -hyuga-eval-form
                            :locals (locals))]
        ;; TODO: parse defn/defmacro args and add to dict-item
        ;; TODO: can't import module/class in current sourcetree
        (->> (locals) (.items)
             filter-add-targets
             (map #%(add-sym! %1 "local"))
             tuple)
        (->> __macros__ (.items)
             filter-add-targets
             (map #%(add-sym! %1 "macro"))
             tuple)
        -hyuga-eval-form))
    (except
      [e BaseError]
      (logger.error (.format "-walk-eval! error e={}" e))
      (logger.error
        (.format "-walk-eval! error e.type={}" (type e))))
    (finally
      (return -hyuga-eval-form))))

(defn walk-eval!
  [forms]
  "TODO: doc"
  (try
    (->> forms (prewalk -walk-eval!) tuple)
    (except [e BaseException]
            (logger.warning
              (.format "walk-eval! error={}" e)))))

(defn get-details
  [sym-hy]
  "TODO: doc"
  (logger.debug (.format "get-details sym-hy={}" sym-hy))
  ;; TODO: try get info directly if sym not found
  (-> ($GLOBAL.get-$SYMS) (get sym-hy)))

(defn get-module-attrs
  [splitted-by-dot]
  (let [ns (->> splitted-by-dot butlast (.join "."))
        eval-str (.format "({}.__dict__.items)" ns)]
    (-> eval-str hy.read
        (hy.eval :locals (locals)))))


(defn get-candidates
  [prefix]
  "Get all candidates supposed by prefix from all scopes.
  (globals, locals, builtins, and macros)

  Example:
  ```hy
  (get-candidates \"de\")
  => #({\"scope\" \"builtin\"
  \"type\" <class 'builtin_function_or_method'>
  \"sym\" \"delattr\"})
  ```
  "
  (logger.debug
    (.format "get-candidates: $SYMS.count={}"
             (count ($GLOBAL.get-$SYMS))))
  (let [splitted-by-dot (.split prefix ".")
        module-or-class (if (-> splitted-by-dot count (> 1))
                          (->> splitted-by-dot (drop-last 1) (.join "."))
                          "")
        sym-prefix (if module-or-class
                     (last splitted-by-dot)
                     prefix)]
    (logger.debug (.format "module-or-class={}" module-or-class))
    (when module-or-class
      (->> (get-module-attrs splitted-by-dot)
           filter-add-targets
           (map #%(+ [] [(as-> splitted-by-dot it
                           (drop-last 1 it)
                           (list it)
                           (+ it [(first %1)])
                           (.join "." it))
                         (second %1)]))
           (map #%(add-sym! %1 "module"))
           tuple))
    (->> ($GLOBAL.get-$SYMS) .items
         (filter #%(.startswith (first %1) module-or-class))
         (filter #%(.startswith (-> %1 first (.split ".") last) sym-prefix))
         (map #%(get-details (first %1)))
         (map #%(do (.update %1 {"sym" (-> (get %1 "sym") (.split ".") last)})
                    %1))
         tuple)))

(defn eval-define!
  [src]
  "TODO: doc"
  (logger.debug (.format "eval-define!"))
  ;; FIXME: HyEvalError("module 'hy' has no attribute 'hyx_XampersandXreader'")
  ;; when eval (require hyrule * :readers *).
  ;; @see https://github.com/hylang/hy/issues/2291
  ;; TODO: implement cache mechanism
  (try
    (let [forms (hy.read-many src)]
      (->> forms (map walk-eval!) tuple)
      (logger.debug (.format "eval done. $GLOBAL.$SYMS.count={}"
                             (->> ($GLOBAL.get-$SYMS) count)))
      (->> __builtins__ (.items)
           filter-add-targets
           (map #%(add-sym! %1 "builtin"))
           tuple)
      (logger.debug (.format "builtin loaded.")))
    (except
      [e BaseException]
      (logger.warning (.format "eval-define!: error e={}" e)))
    (else (logger.debug "eval-define! done."))))
