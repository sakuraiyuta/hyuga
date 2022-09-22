(require hyrule * :readers *)
(require hyrule.collections [assoc])
(import hyrule.collections [walk])

(import hy.reserved :as -reserved)
(import hy.models [Expression Keyword List])
(import hy.compiler [HyASTCompiler])
(import hy.reader [HyReader])

(import os [environ listdir])
(import os.path [isdir])
(import sys [modules])
(import functools [reduce partial])
(import toolz.dicttoolz [merge])

(import hyuga.log [logger])
(import hyuga.sym.helper *)
(import hyuga.sym.dummy)

; {"{doc-uri}" {"compiler" HyASTCompiler
;               "reader" HyReader}}
(setv $compiler {})

(defn get-hy-builder
  [doc-uri mod key]
  (if (in doc-uri ($compiler.keys))
    (-> (get $compiler doc-uri) (get key))
    (do
      (logger.debug f"creating new compiler: doc-uri={doc-uri}, mod={mod}")
      (assoc $compiler doc-uri
             {"compiler" (HyASTCompiler
                           :module mod
                           :filename (remove-uri-prefix doc-uri))
              "reader" (HyReader)})
      (get-hy-builder doc-uri mod key))))

(defn -load!
  [prefix syms [pos None] [uri None]]
  (->> syms
       (filter-add-targets prefix)
       (map #%(add-sym! %1 prefix pos uri))
       tuple))

(defn add-sym!
  [sym-hy/val scope [pos None] [doc-uri None]]
  "TODO: doc"
  (let [[sym-hy val] sym-hy/val
        docs (create-docs sym-hy val scope)]
    ($GLOBAL.add-$SYMS
      {"sym" (get-full-sym scope sym-hy) "type" val "uri" doc-uri
       "scope" (judge-scope val scope)
       "docs" docs "pos" pos})))

(defn filter-add-targets
  [prefix sym-py/vals]
  "TODO: doc"
  (or (->> sym-py/vals
           (map sym-py/val->sym-hy/val)
           (filter #%(not-in-$SYM? prefix %1))
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
;(defn -add-hy-sym!
;  [form]
;  "TODO: doc"
;  (print (first form))
;  (let [summary (branch (= it (-> form first str))
;                        "defn" (get-defn-summary form)
;                        "defclass" (get-defclass-summary form)
;                        else (get-setv-summary form))]
;    (add-sym! [(get summary "name") form]
;              "local"
;              (get summary "pos"))))

(defn -dummy-eval!
  [form [doc-uri "file:///dummy"] [mod "hyuga.sym.dummy"]]
  (logger.debug f"-dummy-eval!: form={(hy.repr form)}, doc-uri={doc-uri}, mod={mod}")
  (hy.eval form
           :locals hyuga.sym.dummy.__dict__
           :compiler (get-hy-builder doc-uri mod "compiler")))

(defn -cleanup-dummy-syms!
  [[summary {"includes" []}]]
  (->> hyuga.sym.dummy.__dict__ (.keys) tuple
       (filter #%(not (or (and (.startswith %1 "__")
                               (.endswith %1 "__"))
                          (= "hy" %1))))
       (filter #%(branch (isinstance (:includes summary) it)
                   List (not (in %1 (:includes summary)))
                   str False))
       (map #%(.pop hyuga.sym.dummy.__dict__ %1))
       tuple))

(defn -imported-hy-src
  [form doc-uri]
  (logger.debug f"-imported-hy-src: trying to import {(second form)} doc-uri={doc-uri}")
  (-> f"({(first form)} {(second form)})"
      (hy.read)
      (-dummy-eval! doc-uri))
  (logger.debug f"-imported-hy-src: imported {(second form)} doc-uri={doc-uri}")
  (let [dic (-> f"{(second form)}.__dict__"
                hy.read (-dummy-eval! doc-uri))
        keys (.keys dic)]
    (when (and (in "hy" keys)
               (in "__file__" keys))
      (with [file (open (get dic "__file__"))]
        (let [fname file.name]
          (logger.debug f"hy-source detected: try to read. filename={fname}")
          (-> (file.read)
              (load-src! "" f"file://{fname}" (second form))
              tuple))
        ))))

(defn -load-macro!
  []
  (-load! "macro" (__macros__.items)))

(defn -load-local!
  [prefix pos uri]
  (logger.debug f"-load-local! prefix={prefix} pos={pos} uri={uri}")
  (-load! prefix
          (-> f"({prefix}.__dict__.items)"
              hy.read
              (-dummy-eval! uri prefix))
          pos
          uri))

(defn -eval-and-add-sym!
  [form root-uri doc-uri prefix]
  "TODO: docs"
  (logger.debug f"-eval-and-add-sym!: ({(first form)} {(second form)}) prefix={prefix}, root-uri={root-uri}, doc-uri={doc-uri}, prefix={prefix}")
  (try
    ;; TODO: parse defn/defmacro args and show in docs
    ;; TODO: need fix for Hy definition(defn/defmacro/defclass): don't eval and keep hy.models
    (let [pos (get-form-pos form)
          import? (= "import" (-> form first str))]
      (when (and import?
                 (not prefix))
        (logger.debug f"import found. try to import {(second form)}")
        ;(-cleanup-dummy-syms! (get-import-summary form))
        ;(-cleanup-dummy-syms!)
        (-imported-hy-src form doc-uri)
        (logger.debug f"import complete. try to load {(second form)}"))
      (-dummy-eval! form doc-uri (or prefix "hyuga.sym.dummy"))
      (-load-macro!)
      (-load-local! (or prefix "hyuga.sym.dummy") pos doc-uri)
      ;(-cleanup-dummy-syms!)
      )
    (except [e BaseException]
            (error-trace logger.warning "-eval-and-add-sym!" e))))

(defn -try-eval!
  [form root-uri doc-uri prefix]
  "TODO: doc"
  (when (-eval-target? form)
    (-eval-and-add-sym! form root-uri doc-uri prefix))
  form)

(defn -prewalk
  [root-uri doc-uri prefix form]
  "TODO: doc"
  (let [f #%(do (-try-eval! form root-uri doc-uri prefix)
                %1)]
    (walk (partial -prewalk root-uri doc-uri prefix)
          #%(return %1)
          (f form))))

(defn walk-eval!
  [forms root-uri doc-uri prefix]
  "TODO: doc"
  (try
    (-prewalk root-uri doc-uri prefix forms)
    (except [e BaseException]
            (error-trace logger.warning "walk-eval!" e))))

(defn load-sys!
  []
  "TODO: docs"
  ;; TODO: toggle enable/disable to list sys.modules
  (-load! "sys" (modules.items)))

(defn load-src!
  [src root-uri doc-uri prefix]
  "TODO: docs"
  (try
    (logger.debug f"load-src!: started. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)} root-uri={root-uri} doc-uri={doc-uri}")
    (let [fixed-uri (remove-uri-prefix root-uri)
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
    (-dummy-eval! `(import hyuga.sym.dummy)
                  doc-uri "hyuga.sym.dummy")
    (let [forms (hy.read-many src)]
      (->> forms (map #%(walk-eval! %1 root-uri doc-uri "")) tuple))
    (except
      [e BaseException]
      (error-trace logger.warning "load-src!" e))
    (else (logger.debug f"load-src!: done. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}"))))

(defn load-builtin!
  []
  "TODO: docs"
  (-load! "builtin" (__builtins__.items)))

(defn load-hy-special!
  []
  "TODO: docs"
  (-load! "hy-special" (-reserved.names)))
