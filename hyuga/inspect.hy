(require hyrule * :readers *)
(import hyrule.collections [prewalk])
(import hyrule.control *)
(import toolz.itertoolz *)
(import sys)
(import pygls.server [LanguageServer])
(import collections.abc [Iterable])
(import hyuga.log *)
(import hyuga.global [$GLOBAL])
(import hyuga.importer [DummyImporter])
(import hy.models [Lazy Expression])

;; TODO: clean imports/defines when boot

(defn not-in-$SYM?
  [x]
  (not (in (first x) (.keys ($GLOBAL.get-$SYMS)))))

(defn sym-py/val->sym-hy/val
  [sym-py/val]
  (tuple [(-> sym-py/val first sym-py->hy) (second sym-py/val)]))

(defn sym-py->hy
  [sym-py]
  (-> sym-py hy.unmangle))

(defn sym-hy->py
  [sym-hy]
  (-> sym-hy hy.mangle))

(defn -create-macro-docs
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
  [sym-hy symtype orig-docs]
  "TODO: doc"
  (.format "{} {}\n\n{}"
           sym-hy
           (str symtype)
           orig-docs))

(defn create-docs
  [sym-hy symtype]
  "TODO: doc"
  (try
    (-create-fn-docs
      sym-hy symtype
      (or symtype.__doc__
          "No docs."))
    (except
      [e BaseException]
      (logger.debug (.format "cannot read __doc__. try macro docs. e={}" e))
      (-create-macro-docs sym-hy symtype))))

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
    (sys.meta_path.append DummyImporter)
    (import hyuga-dummy)
    (when (is-eval-target? -hyuga-eval-form)
      (logger.debug (.format "found def/import: ({} {})"
                             (first -hyuga-eval-form)
                             (second -hyuga-eval-form)))
      (let [evaled (hy.eval -hyuga-eval-form
                            :locals (locals)
                            :module hyuga-dummy)]
        (->> (locals) (.items)
             ;; FIXME: eval-define!: error e=reader macro '#%' is not defined (<string>, line 94)
             ; (map #%(tuple [(-> %1 first sym-py->hy) (second %1)]))
             (map sym-py/val->sym-hy/val)
             ; (filter #%(not (in (first %1) (.keys ($GLOBAL.get-$SYMS)))))
             (filter not-in-$SYM?)
             (filter add-sym?)
             (map (fn [x] ($GLOBAL.add-$SYMS (first x)
                                             (second x)
                                             "local"
                                             (create-docs (first x)
                                                          (second x)))))
             tuple)
        ;; TODO: user-defined macros can't read
        ; (logger.debug (.format "appending macros: __macros__={}" (dir hyuga-dummy.__macros__)))
        ; (->> __macros__ (.items)
        ;      (map #%(tuple [(-> %1 first sym-py->hy) (second %1)]))
        ;      (filter #%(not (in (first %1) (.keys ($GLOBAL.get-$SYMS)))))
        ;      (filter add-sym?)
        ;      (map #%($GLOBAL.add-$SYMS (first %1)
        ;                                (second %1)
        ;                                "macro"
        ;                                (create-docs (first %1)
        ;                                             (second %1))))
        ;      tuple)
        -hyuga-eval-form))
    (except
      [e StopIteration]
      ;; FIXME: eval defmacro causes StopIteration
      (logger.warning (.format "-walk-eval! error e={}" e))
      (logger.warning (.format "-walk-eval! error e.type={}" (type e))))
    (except
      [e BaseError]
      (logger.error (.format "-walk-eval! error e={}" e))
      (logger.error (.format "-walk-eval! error e.type={}" (type e))))
    (finally
      (sys.meta_path.pop -1)
      (return -hyuga-eval-form))))

(defn walk-eval!
  [forms]
  "TODO: doc"
  (try
    (->> forms (prewalk -walk-eval!) tuple)
    (except [e BaseException]
            (logger.warning (.format "walk-eval! error={}" e)))))

(defn get-details
  [sym-hy]
  "TODO: doc"
  (logger.debug (.format "get-details sym-hy={}" sym-hy))
  (-> ($GLOBAL.get-$SYMS) (get sym-hy)))

(defn contains-in-scope?
  [sym-hy scope]
  "Echo-back if sym-hy in scope."
  (->> scope (.keys)
       (filter (fn [x] (= sym-hy (sym-py->hy x))))
       tuple))

(defn get-candidates
  [prefix]
  "Get all candidates supposed by prefix from all scopes(globals, locals, builtins, and macros).

  Example:
  ```hy
  (get-candidates \"de\")
  => #({\"scope\" \"builtin\"  \"type\" <class 'builtin_function_or_method'>  \"sym\" \"delattr\"})
  ```
  "
  (logger.debug (.format "get-candidates: $SYMS.count={}" (count ($GLOBAL.get-$SYMS))))
  (->> ($GLOBAL.get-$SYMS) (.keys)
       (filter (fn [x] (.startswith x prefix)))
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
  (try
    (let [forms (hy.read-many src)]
      (->> forms (map walk-eval!) tuple)
      (logger.debug (.format "eval done. $GLOBAL.$SYMS.count={}"
                             (->> ($GLOBAL.get-$SYMS) count)))
      (->> __builtins__ (.items)
           (map sym-py/val->sym-hy/val)
           (filter not-in-$SYM?)
           (map #%($GLOBAL.add-$SYMS (first %1)
                                     (second %1)
                                     "builtin"
                                     (create-docs (first %1)
                                                  (second %1))))
           tuple)
      (logger.debug (.format "builtin loaded.")))
    (except
      [e BaseException]
      (logger.warning (.format "eval-define!: error e={}" e)))
    (else (logger.debug "eval-define! done."))))

;(->>
;  "
;  (import re)
;
;  (defn testfn
;  []
;  (print 1))
;
;  (defn testfn2 [] 10)
;
;  (defmacro testmacro [form] '())
;  "
;  eval-define!
;  )
;(get-candidates "te")
;(->> ($GLOBAL.get-$SYMS) first)
;(->> (concat #((dir) [])) list )
;(->> (locals) (.items))
;(->> (globals) (.items))
;(get-details "print")
;(get-details "testfn")
;(get-details "get-details")
;(get-details "get-candidates")
;(get-details "decide-kind")
;(get-details "defmacro!")
;(get-details "walk-eval!")
