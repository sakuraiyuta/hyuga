(require hyrule * :readers *)
(require hyrule.collections [assoc])
(import hyrule.collections [walk])

(import hy.reserved [names :as hy-specials])
(import hy.models [Expression])
(import hy.compiler [HyASTCompiler])
(import hy.reader [HyReader])

(import os [listdir])
(import os.path [isdir dirname])
(import sys [modules])
(import re)
(import functools [partial])
(import pkgutil [iter_modules])
(import types [ModuleType])

(import hyuga.log [logger])
(import hyuga.sym.helper *)
(import hyuga.sym.dummy)
(import hyuga.sym.summary [get-form-summary])
(import hyuga.sym.doc [create-docs])
(import hyuga.sym.filter [filter-add-targets])
(import hyuga.uri.helper [remove-uri-prefix get-venv])

; {"{doc-uri}" {"compiler" HyASTCompiler
;               "reader" HyReader}}
(setv $compiler {})

(defn get-hy-builder
  [doc-uri mod key]
  "TODO: doc"
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
  [mod syms [pos None] [uri None] [changed? False]]
  (->> syms
       (filter-add-targets mod uri changed?)
       (map #%(add-sym! %1 mod pos uri))
       tuple))

(defn add-sym!
  [sym-hy/val scope [pos None] [doc-uri None]]
  "TODO: doc"
  (let [[sym-hy val] sym-hy/val
        docs (create-docs sym-hy val scope doc-uri)
        doc-uri (or doc-uri
                    (and (not (= scope "(builtin)"))
                         (isinstance val ModuleType)
                         (hasattr val "__file__")
                         val.__file__))]
    ($GLOBAL.add-$SYMS
      {"sym" (get-full-sym scope sym-hy) "type" val "uri" doc-uri
       "scope" scope "docs" docs "pos" pos})))

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
  (let [result
        (hy.eval form
                 :locals hyuga.sym.dummy.__dict__
                 :compiler (get-hy-builder doc-uri mod "compiler"))]
    result))

(defn load-hy-src!
  [form fname root-uri]
  "TODO: doc"
  (logger.debug f"trying to load hy-source. fname={fname}")
  (with [file (open fname)]
    (-> (file.read)
        (load-src! root-uri f"file://{file.name}" (second form)
                   False False))))

(defn hy-src?
  [summary doc-uri]
  "TODO: doc"
  (logger.debug f"hy-src? summary={summary}, doc-uri={doc-uri}")
  (let [name (:name summary)
        has-dict? (-> f"(hasattr {name} \"__dict__\")" hy.read eval-in!)]
    (when (and (-> f"(hasattr {name} \"__dict__\")" hy.read eval-in!)
               (-> f"(hasattr {name} \"__file__\")" hy.read eval-in!)
               (-> f"(in \"__builtins__\" ({name}.__dict__.keys))" hy.read eval-in!)
               (-> f"(in \"__hy_injected__\" (-> {name}.__dict__ (get \"__builtins__\") .keys))" hy.read eval-in!)
               (-> f"(get {name}.__dict__ \"__builtins__\" \"__hy_injected__\")" hy.read eval-in!)
               (re.search r".hy[c]*$" (-> f"{name}.__file__" hy.read eval-in!)))
      (-> f"{name}.__file__" hy.read eval-in!))))

(defn load-imported-pypkg!
  [summary mod doc-uri changed?]
  "TODO: doc"
  (let+ [{name "name" pos "pos"
          includes "includes"} summary
         pypkg-items (-> f"{name}.__dict__"
                         hy.read
                         (eval-in! doc-uri mod)
                         .items tuple)
         filtered (cond
                    (= includes "*") pypkg-items
                    (isinstance includes list)
                    (->> pypkg-items
                         (filter #%(in (first %1) includes))
                         tuple)
                    True #())]
    ;; TODO: check imported syms in pypkg.(candidates can't find all syms...use getattr?)
    (logger.debug f"trying to load pypkg syms. name={name}, includes={includes}, mod={mod}, doc-uri={doc-uri}, changed?={changed?}")
    (load-sym! mod filtered pos doc-uri changed?)))

(defn load-pymodule-syms!
  [summary doc-uri changed?]
  "TODO: doc"
  (let+ [{name "name"} summary]
    (load-imported-pypkg! {"name" name
                           "pos" None
                           "includes" "*"}
                          name
                          doc-uri changed?)))

(defn load-import!
  [form summary mod root-uri doc-uri changed?]
  "TODO: doc"
  (logger.debug f"load-import!: summary={summary}, mod={mod}, root-uri={root-uri}, doc-uri={doc-uri}, changed?={changed?}")
  (-> f"(import {(:name summary)})"
      (hy.read)
      (eval-in! doc-uri "hyuga.sym.dummy"))
  (let [fname (hy-src? summary doc-uri)]
    (if fname
      (load-hy-src! form fname root-uri)
      (load-pymodule-syms! summary doc-uri changed?)))
  (load-imported-pypkg! summary mod doc-uri changed?))

(defn load-class-methods!
  [mod name doc-uri summary changed?]
  (for [method-summary (:methods summary)]
    (let [method-name (:name method-summary)
          cls-name name
          method-pos (:pos method-summary)]
      (load-sym! mod
                 #(#(f"{cls-name}.{method-name}"
                      method-summary))
                 method-pos
                 doc-uri
                 changed?))))

(defn analyze-form!
  [form root-uri doc-uri mod changed? need-import?]
  "TODO: docs"
  (try
    (let+ [summary (get-form-summary form)
           mod (or mod (uri->mod root-uri doc-uri))
           {pos "pos" hytype "type" name "name"} summary]
      (logger.debug f"analyze-form!: summary={hytype}/{name}, doc-uri={doc-uri}, mod={mod}, changed?={changed?}")
      (when (= "defmacro" hytype)
        (load-macro! name mod pos doc-uri changed?))
      (when (and (or (= "require" hytype)
                     (= "import" hytype))
                 need-import?)
        (load-import! form summary mod
                      root-uri doc-uri changed?))
      ;; TODO: fix require loading
      (when (or (= "defreader" hytype)
                (= "require" hytype))
        (eval-in! form doc-uri mod)
        (load-required-macros! summary mod doc-uri changed?))
      (when (or (.startswith hytype "def")
                (= hytype "setv"))
        (load-sym! mod
                   #(#(name summary))
                   pos doc-uri changed?))
      (when (= hytype "defclass")
        (load-class-methods! mod name doc-uri
                             summary changed?)))
    (except [e BaseException]
            (log-warn "analyze-form!" e))
    (finally form)))

(defn prewalk-form!
  [root-uri doc-uri mod changed? need-import? form]
  "TODO: doc"
  (let [f #%(when (load-target? form)
              ;; TODO: fix for nested defn/defclass
              (analyze-form! form
                             root-uri
                             doc-uri
                             mod
                             changed?
                             need-import?)
              %1)]
    (walk (partial prewalk-form! root-uri doc-uri mod
                   changed? need-import?)
          #%(return %1)
          (f form))))

(defn walk-form!
  [forms root-uri doc-uri mod changed? need-import?]
  "TODO: doc"
  (try
    (prewalk-form! root-uri doc-uri mod changed? need-import? forms)
    (except [e BaseException]
            (log-warn "walk-form!" e))))

(defn added-import-path?
  [root-path]
  (eval-in! `(not (= ~root-path (get sys.path 0)))))

(defn add-import-path!
  [doc-uri root-path add-path]
  (when (added-import-path? root-path)
    (logger.debug f"add-import-path!: doc-uri={doc-uri}, root-path={root-path}, add-path={add-path}")
    (eval-in! `(sys.path.insert 0 ~add-path) doc-uri)))

(defn load-src!
  [src root-uri doc-uri [mod None] [changed? False] [need-import? True]]
  "TODO: docs"
  (try
    (logger.debug f"load-src!: $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, mod={mod}, changed?={changed?}")
    (let [mod (or mod (uri->mod root-uri doc-uri))
          root-path (remove-uri-prefix root-uri)]
      (eval-in! `(import sys) doc-uri)
      ;; add import path root-uri
      (add-import-path! doc-uri root-path root-path)
      ;; read and analyze src
      (->> (hy.read-many src :filename doc-uri)
           (map #%(walk-form! %1
                              root-uri doc-uri
                              mod changed? need-import?))
           tuple))
    (except
      [e BaseException]
      (log-warn "load-src!" e))
    (else (logger.debug f"load-src!: finished. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, changed?={changed?}"))))

(defn load-builtin!
  []
  "TODO: docs"
  (load-sym! "(builtin)" (__builtins__.items)))

(defn load-hy-special!
  []
  "TODO: docs"
  (load-sym! "(hykwd)" (->> (hy-specials) (map #%(return [%1 %1])))))

(defn load-sys!
  []
  "TODO: docs"
  ;; TODO: toggle enable/disable to list sys.modules #11
  (load-sym! "(sysenv)" (->> modules .items
                             (filter #%(not (.startswith (first %1) "hyuga.")))
                             tuple)))

(defn load-venv!
  [root-uri doc-uri]
  (eval-in! `(import sys))
  (let [venv-path (get-venv root-uri)
        prev-sys-path (eval-in! `sys.path)]
    ;; TODO: user can set config #11
    (logger.debug f"load-venv! venv-path={venv-path} prev-sys-path={prev-sys-path}")
    (eval-in! `(sys.path.insert 0 ~venv-path))
    (let [syms (->> (iter-modules :path [venv-path])
                    (map #%(. %1 name))
                    (filter #%(not (= "sys" %1)))
                    (filter #%(not (in f"(sysenv)\\{(first %1)}" (->> ($GLOBAL.get-$SYMS) .keys))))
                    tuple)
          items (->> syms
                     (map #%(return #(%1 (-> f"(import {%1})\n{%1}" hy.read-many eval-in!))))
                     tuple)]
      (load-sym! "(venv)" items))
    (eval-in! `(setv sys.path ~prev-sys-path))))

(defn load-required-macros!
  [summary mod doc-uri changed?]
  "TODO: doc"
  (let+ [{name "name" pos "pos"} summary
         items (-> f"({name}.__macros__.items)"
                   hy.read (eval-in! doc-uri mod))]
    (logger.debug f"load-required-macros! summary={summary}, mod={mod}, doc-uri={doc-uri}, changed?={changed?}")
    (load-sym! mod items pos doc-uri changed?)))

(defn load-macro!
  [name mod pos uri changed?]
  "TODO: doc"
  (logger.debug f"load-macro! name={name}, mod={mod}, uri={uri}, changed?={changed?}")
  (let [items (-> f"({mod}.__macros__.items)"
                  (hy.read)
                  (eval-in! uri mod))
        matched (->> items
                     (filter #%(= name (first %1)))
                     tuple)]
    (load-sym! mod matched pos uri changed?)))
