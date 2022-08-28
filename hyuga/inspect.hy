(require hyrule * :readers *)
(import hyrule.collections [prewalk])
(import hyrule.control *)
(import toolz.itertoolz *)
(import sys)
(import re)
(import pygls.server [LanguageServer])
(import collections.abc [Iterable])
(import hyuga.log *)
(import hyuga.global [$GLOBAL])
(import hy.models [Expression])

(setv SCOPE-LISTS #("local" "macro" "global" "builtin"))

(defn testfn
  []
  "Test function
  quot `quot`
  strong **strong**"
  None)

(defn sym-py->hy
  [sym-py]
  (-> sym-py hy.read hy.unmangle))

(defn sym-hy->py
  [sym-hy]
  (-> sym-hy hy.read hy.mangle))

(defn -walk-eval!
  [form]
  "TODO: doc"
  (try
    (when (and (isinstance form Expression)
               (or (.startswith (first form) "require")
                   (.startswith (first form) "def")
                   (.startswith (first form) "import")))
      (logger.debug (.format "found def/import: ({} {})"
                             (first form) (second form)))
      (hy.eval form))
    (except [e BaseException]
            (logger.warning (.format "cannot evaluate: {}" e)))
    (finally (return form)))
  (return form))

(defn walk-eval!
  [forms]
  "TODO: doc"
  (->> forms (prewalk -walk-eval!) tuple))

(defn get-scope
  [scope-str]
  "TODO: doc"
  (branch (= scope-str it)
          "global" (:global ($GLOBAL.get-$SCOPES))
          "local" (:local ($GLOBAL.get-$SCOPES))
          "builtin" (:builtin ($GLOBAL.get-$SCOPES))
          "macro" (:macro ($GLOBAL.get-$SCOPES))
          else None))

(defn -create-docs
  [sym-hy symtype scope-str fixed-docs]
  (.format "{} {} {}\n\n{}"
           scope-str
           (str symtype)
           sym-hy
           fixed-docs))

(defn create-docs
  [sym-hy symtype scope-str]
  "TODO: doc"
  (try
    (-create-docs
      sym-hy symtype scope-str
      (->> sym-hy
           (.format "{}.__doc__")
           hy.read hy.eval))
    (except
      [_ Exception]
      (-create-docs
        sym-hy symtype scope-str
        ;; FIXME: So dirty hack!
        (->> sym-hy (.format
                      "(do
                      (import io)
                      (import contextlib [redirect-stdout])
                      (with [buf (io.StringIO)
                      _ (redirect-stdout buf)]
                      (doc {})
                      (buf.getvalue)))")
             hy.read hy.eval)))))

;(get-details "print")
;(get-details "testfn")
;(get-details "get-details")
;(get-details "get-candidates")
;(get-details "decide-kind")
;(get-details "defmacro!")
;(get-details "walk-eval!")
;(type get-details)
;(->> (globals) (.keys) tuple)
;(->> __macros__ (.keys)
;     (map sym-py->hy)
;     tuple)
(defn get-details
  [sym-hy]
  "TODO: doc"
  (for [scope-str SCOPE-LISTS]
    (try
      (logger.debug (.format "get-details sym-hy={}, scope={}, contains?={}"
                             sym-hy
                             scope-str
                             (contains-in-scope?
                               sym-hy (get-scope scope-str))))
      (when (contains-in-scope?
              sym-hy (get-scope scope-str))
        (let [symtype (->> sym-hy (.format "(type {})")
                           hy.read hy.eval)
              docs (create-docs sym-hy symtype scope-str)]
          (return {"scope" scope-str
                   "type" symtype
                   "docs" docs
                   "sym" sym-hy})))
      (except [e BaseException]
              (logger.warning (.format "get-details eval error sym-hy={} msg={}" sym-hy e))
              (return {"scope" scope-str
                       "type" scope-str
                       "docs" (if (in "macro" scope-str)
                                (create-docs sym-hy "macro" scope-str)
                                "unknown")
                       "sym" sym-hy})))))

;(get-symbols {"global" (globals) "local" (locals) "builtin" __builtins__ "macro" __macros__})
(defn get-symbols
  []
  "Get all symbols from all scopes."
  (->> SCOPE-LISTS
       (map get-scope)
       (map #%(.keys %1))
       concat
       (map sym-py->hy)
       tuple))

(defn contains-in-scope?
  [sym-hy scope]
  "Echo-back if sym-hy in scope."
  (->> scope (.keys)
       (filter #%(= sym-hy (sym-py->hy %1)))
       tuple))

(defn update-$SYMS!
  []
  "TODO: doc"
  ($GLOBAL.set-$SYMS (->> (get-symbols)
                          (map sym-py->hy)
                          ; (map #%(get-details %1 scopes))
                          tuple))
  (logger.debug (.format "update-$SYMS! count={}" (count ($GLOBAL.get-$SYMS)))))

;(get-candidates "de")
(defn get-candidates
  [prefix]
  "Get all candidates supposed by prefix from all scopes(globals, locals, builtins, and macros).

  Example:
  ```hy
  (get-candidates \"de\" {\"global\" (globals)
  \"local\" (locals)
  \"builtin\" __builtins__
  \"macro\" __macros__})
  => #({\"scope\" \"builtin\"  \"type\" <class 'builtin_function_or_method'>  \"sym\" \"delattr\"})
  ```
  "
  (logger.debug (.format "get-candidates: $SYMS.count={}" (count ($GLOBAL.get-$SYMS))))
  (->> ($GLOBAL.get-$SYMS)
       (filter #%(.startswith %1 (sym-hy->py prefix)))
       (map get-details)
       tuple))

(defn eval-define!
  [src]
  "TODO: doc"
  (logger.debug (.format "eval-define!"))
  ;; FIXME: HyEvalError("module 'hy' has no attribute 'hyx_XampersandXreader'")
  ;; when eval (require hyrule * :readers *).
  ;; @see https://github.com/hylang/hy/issues/2291
  ;; TODO: implement cache mechanism
  (let [forms (hy.read-many src :skip_shebang True)]
    (do
      (->> forms
           (map walk-eval!)
           tuple)
      (logger.debug (.format "forms={} form-count={}" forms (count forms)))
      ($GLOBAL.set-$SCOPES {"global" (globals)
                            "local" (locals)
                            "builtin" __builtins__
                            "macro" __macros__})
      (logger.debug (.format "$SCOPES updated."))
      (update-$SYMS!))))

;(->>
;  "
;  (import re)
;
;  (defn testfn
;  []
;  (print 1))
;
;  (defn testfn2 [] 10)
;  "
;  hy.read-many
;  walk-eval!
;  tuple
;  )
