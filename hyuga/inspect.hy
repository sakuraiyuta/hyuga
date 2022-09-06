(require hyrule * :readers *)
(import hyrule.collections [prewalk])
(import hyrule.iterables [butlast drop-last])
(import hyrule.control *)

(import toolz.itertoolz *)

(import sys)
(import types [ModuleType])
(import collections.abc [Iterable])

(import pygls.server [LanguageServer])

(import hyuga.log [logger])
(import hyuga.global [$GLOBAL])
(import hyuga.importer [DummyImporter])
(import hy.models [Lazy Expression])
(import hyuga.sym.helper *)

(defn filter-add-targets
  [sym-py/vals]
  "TODO: doc"
  (->> sym-py/vals
       (map sym-py/val->sym-hy/val)
       (filter not-in-$SYM?)
       (filter not-exclude-sym?)))

(defn is-eval-target?
  [form]
  (and (isinstance form Expression)
       ;; TODO: get config from server params
       (or (.startswith (first form) "require")
           (.startswith (first form) "def")
           (.startswith (first form) "setv")
           (.startswith (first form) "import"))))

(defn -walk-eval!
  [-hyuga-eval-form]
  "TODO: doc"
  (try
    (when (is-eval-target? -hyuga-eval-form)
      (let [raw-import-str (.format "({} {})"
                                    (first -hyuga-eval-form)
                                    (second -hyuga-eval-form))
            evaled (hy.eval -hyuga-eval-form
                            :locals (locals))]
        (logger.debug (.format "found def/import: {}"
                               raw-import-str))
        ;; import raw package(exclude * or [symbol])
        (when (= (-> -hyuga-eval-form first str) "import")
          (logger.debug (.format "raw import: {}"
                                 raw-import-str))
          (-> raw-import-str hy.read hy.eval))
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

(defn get-module-in-syms
  [sym-hy]
  "TODO: doc"
  (as-> ($GLOBAL.get-$SYMS) it
    (.items it)
    (filter #%(= (first %1) sym-hy) it)
    (first it)
    (get (second it) "type")))

(defn get-module-attrs
  [splitted-by-dot]
  "TODO: doc"
  (try
    (let [module (->> splitted-by-dot butlast tuple (.join ".") get-module-in-syms)]
      (logger.debug (.format "get-module-attrs: module={}"
                             module))
      (module.__dict__.items))
    (except [e BaseException]
            (logger.warning (.format "get-module-attrs: error e={} e.type={}"
                                     e (type e))))))

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
