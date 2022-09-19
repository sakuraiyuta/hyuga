(require hyrule * :readers *)
(import hyrule.collections [walk])

(import hy.reserved)
(import hy.models [Expression Keyword])

(import sys)
(import functools [reduce partial])
(import toolz.dicttoolz [merge])

(import hyuga.log [logger])
(import hyuga.sym.helper *)

(defn filter-add-targets
  [sym-py/vals]
  "TODO: doc"
  (or (->> sym-py/vals
           (map sym-py/val->sym-hy/val)
           (filter not-in-$SYM?)
           (filter not-exclude-sym?))
      #()))

(defn -is-eval-target?
  [form]
  (branch (isinstance form it)
          Keyword False
          Expression
          (let [form-str (-> form first str)]
            (or (.startswith form-str "require")
                (.startswith form-str "def")
                (.startswith form-str "setv")
                (.startswith form-str "import")))
          else False))

(defn get-syms-for-local
  []
  "TODO: docs"
  (let [local-syms
        (->> ($GLOBAL.get-$SYMS) .items
             (map #%(return
                      {(first %1)
                       (-> %1 second (get "type"))})))]
    (reduce merge local-syms {})))

(defn -eval-and-add-sym!
  [-hyuga-eval-form]
  "TODO: docs"
  (logger.debug f"-eval-and-add-sym!: ({(first -hyuga-eval-form)} {(second -hyuga-eval-form)})")
  (try
    (hy.eval -hyuga-eval-form :locals (locals))
    ;; TODO: parse defn/defmacro args and show in docs
    ;; TODO: can't import module/class in current sourcetree
    (->> (locals) (.items)
         filter-add-targets
         (map #%(add-sym! %1 "local"))
         tuple)
    (->> (globals) (.items)
         filter-add-targets
         (map #%(add-sym! %1 "globals"))
         tuple)
    (->> __macros__ (.items)
         filter-add-targets
         (map #%(add-sym! %1 "macro"))
         tuple)
    (except [e BaseException]
            (error-trace logger.warning "-eval-and-add-sym!" e))))

(defn -try-eval!
  [form]
  "TODO: doc"
  (when (-is-eval-target? form)
    (try
      (logger.debug
        f"found def/import: ({(first form)} {(second form)})")
      (-eval-and-add-sym! form)
      (except [e BaseException]
              (error-trace logger.warning "-try-eval" e)))))

(defn -prewalk
  [form]
  "TODO: doc"
  (walk -prewalk
        #%(do (-try-eval! form)
              %1)
        #%(return %1)))

(defn walk-eval!
  [forms]
  "TODO: doc"
  (try
    (-prewalk forms)
    ;    (->> forms (prewalk #(-walk-eval! %1 False)) tuple)
    (except [e BaseException]
            (error-trace logger.warning "walk-eval!" e))))

(defn load-sys!
  []
  "TODO: docs"
  ;; TODO: toggle enable/disable to list sys.modules
  (->> (sys.modules.items) tuple
       filter-add-targets
       (map #%(add-sym! %1 "sys"))
       tuple))

(defn load-src!
  [src]
  "TODO: docs"
  ;; FIXME: HyEvalError("module 'hy' has no attribute 'hyx_XampersandXreader'")
  ;; when eval (require hyrule * :readers *).
  ;; @see https://github.com/hylang/hy/issues/2291
  (try
    (let [forms (hy.read-many src)]
      (->> forms (map walk-eval!) tuple)
      (logger.debug f"eval done. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}"))
    (except
      [e BaseException]
      (error-trace logger.warning "load-src!" e))
    (else (logger.debug "load-src! done."))))

(defn load-builtin!
  []
  "TODO: docs"
  (->> __builtins__ (.items)
       filter-add-targets
       (map #%(add-sym! %1 "builtin"))
       tuple))

(defn load-hy-special!
  []
  "TODO: docs"
  (->> (hy.reserved.names)
       (map #%(tuple [%1 "<hy built-in special form>"]))
       filter-add-targets
       (map #%(add-sym! %1 "hy-special"))
       tuple))

(defn load-hy-macro!
  []
  "TODO: docs"
  (->> (hy.reserved.macros)
       (map #%(tuple [%1 "<hy built-in macro>"]))
       filter-add-targets
       (map #%(add-sym! %1 "hy-macro"))
       tuple))
