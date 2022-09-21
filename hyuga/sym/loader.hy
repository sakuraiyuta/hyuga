(require hyrule * :readers *)
(import hyrule.collections [walk])

(import hy.reserved :as -reserved)
(import hy.models [Expression Keyword])

(import os [environ listdir])
(import os.path [isdir])
(import sys [modules])
(import re [sub])
(import functools [reduce partial])
(import toolz.dicttoolz [merge])

(import hyuga.log [logger])
(import hyuga.sym.helper *)

(defn add-sym!
  [sym-hy/val scope
   [pos #(None None)]]
  "TODO: doc"
  (let [[sym-hy val] sym-hy/val
        docs (create-docs sym-hy val scope)]
    ($GLOBAL.add-$SYMS sym-hy val
                       (judge-scope val scope)
                       docs pos)))

(defn filter-add-targets
  [sym-py/vals]
  "TODO: doc"
  (or (->> sym-py/vals
           (map sym-py/val->sym-hy/val)
           (filter not-in-$SYM?)
           (filter not-exclude-sym?))
      #()))

(defn -def-or-setv?
  [form]
  "TODO: doc"
  (if (isinstance form Expression)
    (let [sym (-> form first str)]
      (or (.startswith sym "def")
          (.startswith sym "setv")))
    False))

(defn -eval-target?
  [form]
  "TODO: doc"
  (if (isinstance form Expression)
    (let [sym (-> form first str)]
      (or (.startswith sym "require")
          (.startswith sym "import")
;          (.startswith sym "defmacro")
          (.startswith sym "def")))
    False))

;; TODO: WIP for textDocument/definition
(defn -add-hy-sym!
  [form]
  "TODO: doc"
  (print (first form))
  (let [summary (branch (= it (-> form first str))
                        "defn" (get-defn-summary form)
                        "defclass" (get-defclass-summary form)
                        else (get-setv-summary form))]
    (add-sym! [(get summary "name") form]
              "local"
              (get summary "pos"))))

(defn -eval-and-add-sym!
  [-hyuga-eval-form root-uri]
  "TODO: docs"
  (logger.debug f"-eval-and-add-sym!: ({(first -hyuga-eval-form)} {(second -hyuga-eval-form)})")
  (try
    ;; TODO: parse defn/defmacro args and show in docs
    ;; TODO: need fix for Hy definition(defn/defmacro/defclass): don't eval and keep hy.models
    (hy.eval -hyuga-eval-form :locals (globals))
    (let [pos (get-form-pos -hyuga-eval-form)]
      (->> (globals) (.items)
           filter-add-targets
           ;; FIXME: dirty hack: avoid filter when developping hyuga.
           (filter #%(if (.endswith root-uri "hyuga")
                       True
                       (not-in (sym-hy->py (first %1)) -hyuga-syms)))
           (filter #%(not (.startswith (first %1) "-hyuga-syms")))
           (map #%(add-sym! %1 "local" pos))
           tuple)
      (->> __macros__ (.items)
           filter-add-targets
           (map #%(add-sym! %1 "macro" pos))
           tuple))
    (except [e BaseException]
            (error-trace logger.warning "-eval-and-add-sym!" e))))

(defn -try-eval!
  [form root-uri]
  "TODO: doc"
  (when (-eval-target? form)
    (-eval-and-add-sym! form root-uri))
;  (branch (it form)
;          -eval-target? (-eval-and-add-sym! form root-uri)
;          -def-or-setv? (-add-hy-sym! form))
  form)

(defn -prewalk
  [root-uri form]
  "TODO: doc"
  (let [f #%(do (-try-eval! form root-uri)
                %1)]
    (walk (partial -prewalk root-uri)
          #%(return %1)
          (f form))))

(defn walk-eval!
  [forms root-uri]
  "TODO: doc"
  (try
    (-prewalk root-uri forms)
    (except [e BaseException]
            (error-trace logger.warning "walk-eval!" e))))

(defn load-sys!
  []
  "TODO: docs"
  ;; TODO: toggle enable/disable to list sys.modules
  (->> (modules.items) tuple
       filter-add-targets
       (filter #%(not (.startswith (first %1) "hyuga")))
       (filter #%(not-in (sym-hy->py (first %1)) -hyuga-syms))
       (map #%(add-sym! %1 "sys"))
       tuple))

(defn load-src!
  [src root-uri]
  "TODO: docs"
  (try
    (logger.debug f"load-src!: started. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)} root-uri={root-uri}")
    (let [fixed-uri (sub "^[a-z]+://" "" root-uri)
          venv-lib-path f"{fixed-uri}/.venv/lib"]
      (hy.eval `(import sys))
      (hy.eval `(sys.path.append ~fixed-uri))
      ;; add import path for poetry venv
      (when (isdir venv-lib-path)
        (logger.debug f"found venv: venv-path={venv-lib-path}")
        (let [dirname (-> venv-lib-path listdir first)
              target-path f"{venv-lib-path}/{dirname}/site-packages"]
          (logger.debug f"adding module path: target-path={target-path}")
          (hy.eval `(sys.path.append ~target-path)))))
    (let [forms (hy.read-many src)]
      (->> forms (map #%(walk-eval! %1 root-uri)) tuple)
      )
    (except
      [e BaseException]
      (error-trace logger.warning "load-src!" e))
    (else (logger.debug f"load-src!: done. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}"))))

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
  (->> (-reserved.names)
       (map #%(tuple [%1 "<hy-special>"]))
       filter-add-targets
       (map #%(add-sym! %1 "hy-special"))
       tuple))

(setv -hyuga-syms
      (let [sys-modules (-> (modules.keys) list)
            hyuga-syms (-> (get modules "hyuga.sym.loader")
                           dir)]
        (map #%(when (in %1 sys-modules)
                 (.remove hyuga-syms %1))
             hyuga-syms)
        hyuga-syms))
