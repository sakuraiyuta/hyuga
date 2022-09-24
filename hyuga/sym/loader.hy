(require hyrule * :readers *)
(require hyrule.collections [assoc])
(import hyrule.collections [walk])

(import hy.reserved [macros :as hy-macros
                     names :as hy-specials])
(import hy.models [Expression Keyword List])
(import hy.compiler [HyASTCompiler])
(import hy.reader [HyReader])

(import os [environ listdir])
(import os.path [isdir])
(import sys [modules])
(import re)
(import functools [reduce partial])
(import toolz.dicttoolz [merge])

(import hyuga.log [logger])
(import hyuga.sym.helper *)
(import hyuga.sym.dummy)

; {"{doc-uri}" {"compiler" HyASTCompiler
;               "reader" HyReader}}
(setv $compiler {})

(defn fix-prefix
  [prefix]
  (or prefix "hyuga.sym.dummy"))

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
  [mod syms [pos None] [uri None] [update? False]]
  (->> syms
       (filter-add-targets mod uri update?)
       (map #%(add-sym! %1 mod pos uri))
       tuple))

(defn add-sym!
  [sym-hy/val scope [pos None] [doc-uri None]]
  "TODO: doc"
  (let [[sym-hy val] sym-hy/val
        docs (create-docs sym-hy val scope doc-uri)]
    ($GLOBAL.add-$SYMS
      {"sym" (get-full-sym scope sym-hy) "type" val "uri" doc-uri
       "scope" (judge-scope val scope)
       "docs" docs "pos" pos})))

(defn filter-add-targets
  [mod uri update? sym-py/vals]
  "TODO: doc"
  (let [hy? #%(= (-> %1 first get-sym) "hy")
        pyattr? #%(and (.startswith (-> %1 first get-sym) "__")
                       (.endswith (-> %1 first get-sym) "__"))]
    (or (->> sym-py/vals
             (map sym-py/val->sym-hy/val)
             (filter #%(not (or (hy? %1)
                                (pyattr? %1))))
             (filter #%(or update?
                           (not-in-$SYM? mod uri %1)))
             (filter not-exclude-sym?))
        #())))

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
  (hy.eval form
           :locals hyuga.sym.dummy.__dict__
           :compiler (get-hy-builder doc-uri mod "compiler")))

(defn -cleanup-dummy-syms!
  [mod doc-uri [summary {"includes" None}]]
  (let [dic (-> f"{mod}.__dict__" hy.read (-dummy-eval! doc-uri mod))
        matches (->> dic (.keys) tuple
                     (filter #%(not (or (and (.startswith %1 "__")
                                             (.endswith %1 "__"))
                                        (= "hy" %1)
                                        (.startswith %1 "hyuga"))))
                     (filter #%(branch (isinstance (:includes summary) it)
                                       List (not (in %1 (:includes summary)))
                                       str False
                                       else True))
                     tuple)]
    (logger.debug f"-cleanup-dummy-syms!: matches={matches} summary={summary}")
    (tuple (map dic.pop matches))))

(defn -load-hy-src!
  [form root-uri doc-uri]
  (-> f"({(first form)} {(second form)})"
      (hy.read)
      (-dummy-eval! doc-uri))
  (let [dic (-> f"{(second form)}.__dict__"
                hy.read (-dummy-eval! doc-uri))
        keys (.keys dic)]
    (when (and (in "hy" keys)
               (in "__file__" keys)
               (not-in f"file://{(get dic "__file__")}" ($compiler.keys))
               (re.search r".hy[c]*$" (get dic "__file__")))
      (with [file (open (get dic "__file__"))]
        (logger.debug f"hy-source import detected: trying to read. filename={file.name}")
        (-> (file.read)
            (load-src! root-uri f"file://{file.name}" (second form)))))))

(defn -load-macro!
  [name prefix pos uri update?]
  (let [items (-> f"({prefix}.__macros__.items)"
                  (hy.read)
                  (-dummy-eval! uri prefix))
        matched (->> items
                     (filter #%(= name (first %1)))
                     tuple)]
    (-load! prefix matched pos uri update?)))

(defn -load-local!
  [name prefix pos uri update?]
  "TODO: doc"
  (logger.debug f"-load-local!: name={name}, prefix={prefix}, pos={pos}, uri={uri}, update?={update?}")
  (let [items (-> f"({prefix}.__dict__.items)"
                  (hy.read)
                  (-dummy-eval! uri prefix))
        matched (->> items
                     (filter #%(= name (first %1)))
                     tuple)]
    (-load! prefix matched pos uri update?)))

(defn -eval-and-add-sym!
  [form root-uri doc-uri prefix update?]
  "TODO: docs"
  (try
    ;; TODO: parse defn/defmacro args and show in docs
    ;; TODO: need fix for Hy definition(defn/defmacro/defclass): don't eval and keep hy.models
    (let [summary (get-form-summary form)
          mod-name (fix-prefix prefix)
          pos (when summary (:pos summary))
          import? (when summary (= "import" (:type summary)))
          hytype (when summary (:type summary))
          name (when summary (:name summary)) ]
      (logger.debug f"-eval-and-add-sym!: summary={hytype}/{name}, doc-uri={doc-uri}, prefix={prefix}, update?={update?}")
      (-dummy-eval! form doc-uri mod-name)
      (when import?
        (-load-hy-src! form root-uri doc-uri))
      (when (and hytype (= hytype "defmacro"))
        (-load-macro! name mod-name pos doc-uri update?))
      (when (and hytype
                 (or (= hytype "defn")
                     (= hytype "defclass")
                     (= hytype "setv")))
        (-load! mod-name #(#(name summary)) pos doc-uri update?))
      (when (and hytype
                 (= hytype "defclass"))
        (for [method-summary (:methods summary)]
          (let [method-name (:name method-summary)
                cls-name name
                method-pos (:pos method-summary)]
            (-load! mod-name
                    #(#(f"{cls-name}.{method-name}"
                         method-summary))
                      method-pos
                      doc-uri
                      update?))))
;      (if import?
;        (-cleanup-dummy-syms! prefix doc-uri (get-import-summary form))
;        (-cleanup-dummy-syms! prefix doc-uri))
      )
    (except [e BaseException]
            (log-warn "-eval-and-add-sym!" e))))

(defn -try-eval!
  [form root-uri doc-uri prefix update?]
  "TODO: doc"
  (when (-eval-target? form)
    (-eval-and-add-sym! form root-uri doc-uri prefix update?))
  form)

(defn -prewalk
  [root-uri doc-uri prefix update? form]
  "TODO: doc"
  (let [f #%(do (-try-eval! form
                            root-uri
                            doc-uri
                            prefix
                            update?)
                %1)]
    (walk (partial -prewalk root-uri doc-uri prefix update?)
          #%(return %1)
          (f form))))

(defn walk-eval!
  [forms root-uri doc-uri prefix update?]
  "TODO: doc"
  (try
    (-prewalk root-uri doc-uri prefix update? forms)
    (except [e BaseException]
            (log-warn "walk-eval!" e))))

(defn load-sys!
  []
  "TODO: docs"
  (logger.debug f"load-sys!")
  ;; TODO: toggle enable/disable to list sys.modules
  (-load! "sys" (->> modules .items list)))

(defn load-src!
  [src root-uri doc-uri prefix [update? False]]
  "TODO: docs"
  (try
    (logger.debug f"load-src!: $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, prefix={prefix}, update?={update?}")
    (let [root-path (remove-uri-prefix root-uri)
          venv-lib-path f"{root-path}/.venv/lib"]
      (-dummy-eval! `(import sys)
                    doc-uri
                    "hyuga.sym.dummy")
      ;; add import path for poetry venv
      (when (isdir venv-lib-path)
        (logger.debug f"found venv: venv-path={venv-lib-path}")
        (let [dirname (-> venv-lib-path listdir first)
              target-path f"{venv-lib-path}/{dirname}/site-packages"]
          (logger.debug f"adding module path: target-path={target-path}")
          (-dummy-eval! `(when (not (= ~root-path (first sys.path)))
                           (sys.path.insert 0 ~target-path))
                        doc-uri
                        "hyuga.sym.dummy")))
      ;; add import path root-uri
      (logger.debug f"adding root-uri path: root-path={root-path}")
      (-dummy-eval! `(when (not (= ~root-path (get sys.path 0)))
                       (sys.path.insert 0 ~root-path))
                    doc-uri "hyuga.sym.dummy"))
    (-dummy-eval! `(import hyuga.sym.dummy) doc-uri "hyuga.sym.dummy")
    (let [forms (hy.read-many src :filename doc-uri)]
      (->> forms (map #%(walk-eval! %1 root-uri doc-uri (fix-prefix prefix) update?)) tuple))
    (except
      [e BaseException]
      (log-warn "load-src!" e))
    (else (logger.debug f"load-src!: finished. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, update?={update?}"))))

(defn load-builtin!
  []
  "TODO: docs"
  (-load! "builtin" (__builtins__.items)))

(defn load-hy-special!
  []
  "TODO: docs"
  (-load! "hy-macro" (->> (hy-macros) (map #%(return [%1 %1]))))
  (-load! "hy-special" (->> (hy-specials) (map #%(return [%1 %1])))))
