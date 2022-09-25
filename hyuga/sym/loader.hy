(require hyrule * :readers *)
(require hyrule.collections [assoc])
(import hyrule.collections [walk])

(import hy.reserved [macros :as hy-macros
                     names :as hy-specials])
(import hy.models [Expression Keyword List])
(import hy.compiler [HyASTCompiler])
(import hy.reader [HyReader])

(import os [listdir])
(import os.path [isdir])
(import sys [modules])
(import re)
(import functools [partial])

(import hyuga.log [logger])
(import hyuga.sym.helper *)
(import hyuga.sym.dummy)
(import hyuga.sym.summary [get-form-summary])
(import hyuga.sym.doc [create-docs])
(import hyuga.sym.filter [filter-add-targets])
(import hyuga.uri.helper [remove-uri-prefix])

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

(defn load-sym!
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
       "scope" scope
       "docs" docs "pos" pos})))

(defn load-target?
  [form]
  "TODO: doc"
  (if (isinstance form Expression)
    (let [sym (-> form first str)]
      (or (.startswith sym "require")
          (.startswith sym "import")
          (.startswith sym "setv")
          (.startswith sym "def")))
    False))

(defn eval-in!
  [form [doc-uri "file:///dummy"] [mod "hyuga.sym.dummy"]]
  (hy.eval form
           :locals hyuga.sym.dummy.__dict__
           :compiler (get-hy-builder doc-uri mod "compiler")))

(defn load-hy-src!
  [form fname root-uri]
  (with [file (open fname)]
    (-> (file.read)
        (load-src! root-uri f"file://{file.name}" (second form)))))

(defn hy-source-imported?
  [form doc-uri]
  "TODO: doc"
  (let [dic (-> f"{(second form)}.__dict__"
                hy.read (eval-in! doc-uri))
        keys (.keys dic)]
    (when (and (in "hy" keys)
               (in "__file__" keys)
               (not-in f"file://{(get dic "__file__")}" ($compiler.keys))
               (re.search r".hy[c]*$" (get dic "__file__")))
      dic)))

(defn load-pypkg!
  [summary mod doc-uri update?]
  (let+ [{name "name" pos "pos"
          includes "includes"} summary
         pypkg-items (-> f"{name}.__dict__"
                         hy.read
                         (eval-in! doc-uri mod)
                         .items tuple)
         filtered (if (= includes "*")
                    pypkg-items
                    (->> pypkg-items
                         (filter #%(in (first %1) includes))
                         tuple))]
    ;; TODO: bugfix
    (logger.debug f"tyring to load pypkg syms. name={name}")
    (load-sym! mod filtered
               pos doc-uri update?)))

(defn load-import!
  [form summary mod root-uri doc-uri update?]
  (-> f"({(first form)} {(second form)})"
      (hy.read)
      (eval-in! doc-uri))
  (let [dic (hy-source-imported? form doc-uri)]
    (if dic
      (load-hy-src! form (get dic "__file__") root-uri)
      ;; FIXME: need update?
      (load-pypkg! summary mod doc-uri update?))))

(defn load-macro!
  [name prefix pos uri update?]
  (let [items (-> f"({prefix}.__macros__.items)"
                  (hy.read)
                  (eval-in! uri prefix))
        matched (->> items
                     (filter #%(= name (first %1)))
                     tuple)]
    (load-sym! prefix matched pos uri update?)))

(defn load-class-methods!
  [mod name doc-uri summary update?]
  (for [method-summary (:methods summary)]
    (let [method-name (:name method-summary)
          cls-name name
          method-pos (:pos method-summary)]
      (load-sym! mod
                 #(#(f"{cls-name}.{method-name}"
                      method-summary))
                 method-pos
                 doc-uri
                 update?))))

(defn try-eval-setv!
  [form mod doc-uri summary]
  (try
    (eval-in! form doc-uri mod)
    (except
      [e Exception]
      (logger.debug f"can't eval ({(first form)} {(second form)}). set temp value None.")
      ;; TODO: replace setv form to None
      None)))

(defn analyze-form!
  [form root-uri doc-uri prefix update?]
  "TODO: docs"
  (try
    (let+ [summary (get-form-summary form)
           mod (fix-prefix prefix)
           {pos "pos" hytype "type" name "name"} summary]
      (logger.debug f"-eval-and-add-sym!: summary={hytype}/{name}, doc-uri={doc-uri}, prefix={prefix}, update?={update?}")
      (if (and hytype
               (= hytype "setv"))
        (try-eval-setv! form doc-uri mod summary)
        (eval-in! form doc-uri mod))
      (when (= "import" hytype)
        (load-import! form summary mod
                      root-uri doc-uri update?))
      (when (and hytype (= hytype "defmacro"))
        (load-macro! name mod pos doc-uri update?))
      (when (and hytype
                 (or (= hytype "defn")
                     (= hytype "defclass")
                     (= hytype "setv")))
        (load-sym! mod
                   #(#(name summary))
                   pos doc-uri update?))
      (when (and hytype
                 (= hytype "defclass"))
        (load-class-methods! mod name doc-uri
                             summary update?)))
    (except [e BaseException]
            (log-warn "analyze-form!" e))
    (finally form)))

(defn prewalk-form!
  [root-uri doc-uri prefix update? form]
  "TODO: doc"
  (let [f #%(when (load-target? form)
              (analyze-form! form
                             root-uri
                             doc-uri
                             prefix
                             update?)
              %1)]
    (walk (partial prewalk-form! root-uri doc-uri prefix update?)
          #%(return %1)
          (f form))))

(defn walk-form!
  [forms root-uri doc-uri prefix update?]
  "TODO: doc"
  (try
    (prewalk-form! root-uri doc-uri prefix update? forms)
    (except [e BaseException]
            (log-warn "walk-form!" e))))

(defn load-sys!
  []
  "TODO: docs"
  (logger.debug f"load-sys!")
  ;; TODO: toggle enable/disable to list sys.modules #11
  (load-sym! "sys" (->> modules .items list)))

(defn load-src!
  [src root-uri doc-uri prefix [update? False]]
  "TODO: docs"
  (try
    (logger.debug f"load-src!: $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, prefix={prefix}, update?={update?}")
    (let [root-path (remove-uri-prefix root-uri)
          venv-lib-path f"{root-path}/.venv/lib"]
      (eval-in! `(import sys)
                doc-uri
                "hyuga.sym.dummy")
      ;; add import path for venv
      ;; TODO: user can set config #11
      (when (isdir venv-lib-path)
        (logger.debug f"found venv: venv-path={venv-lib-path}")
        (let [dirname (-> venv-lib-path listdir first)
              target-path f"{venv-lib-path}/{dirname}/site-packages"]
          (logger.debug f"adding module path: target-path={target-path}")
          (eval-in! `(when (not (= ~root-path (get sys.path 0)))
                       (sys.path.insert 0 ~target-path))
                    doc-uri
                    "hyuga.sym.dummy")))
      ;; add import path root-uri
      (logger.debug f"adding root-uri path: root-path={root-path}")
      (eval-in! `(when (not (= ~root-path (get sys.path 0)))
                   (sys.path.insert 0 ~root-path))
                doc-uri "hyuga.sym.dummy"))
    (eval-in! `(import hyuga.sym.dummy) doc-uri "hyuga.sym.dummy")
    (let [forms (hy.read-many src :filename doc-uri)]
      (->> forms (map #%(walk-form! %1 root-uri doc-uri (fix-prefix prefix) update?)) tuple))
    (except
      [e BaseException]
      (log-warn "load-src!" e))
    (else (logger.debug f"load-src!: finished. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, update?={update?}"))))

(defn load-builtin!
  []
  "TODO: docs"
  (load-sym! "builtin" (__builtins__.items)))

(defn load-hy-special!
  []
  "TODO: docs"
  (load-sym! "hy-special" (->> (hy-specials) (map #%(return [%1 %1])))))
