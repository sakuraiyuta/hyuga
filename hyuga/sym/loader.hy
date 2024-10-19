(require hyrule * :readers *)
(require hyrule.argmove [-> ->>])
(import hyrule.collections [assoc])
(import hyrule.collections [walk])

(import hy.models [Expression])
(import hy.compiler [HyASTCompiler])
(import hy.reader [HyReader])

(import os [listdir])
(import os.path [isdir dirname])
(import sys [modules])
(import re)
(import functools [partial])
(import pkgutil [iter_modules get_loader])
(import types [ModuleType])

(import hyuga.log [logger])
(import hyuga.sym.helper *)
(import hyuga.sym.dummy)
(import hyuga.sym.summary [get-form-summary])
(import hyuga.sym.doc [create-docs])
(import hyuga.sym.filter [filter-add-targets
                          filter-not-reserved])
(import hyuga.uri.helper [remove-uri-prefix get-venv])

(import builtins)

(setv hy-specials (.keys builtins._hy_macros))

; {"{doc-uri}" {"compiler" HyASTCompiler
;               "reader" HyReader}}
(setv $compiler {})

(defn get-hy-builder
  [doc-uri ns key]
  "TODO: doc"
  (if (in doc-uri ($compiler.keys))
    (-> (get $compiler doc-uri) (get key))
    (do
      (logger.debug f"creating new compiler: doc-uri={doc-uri}, ns={ns}")
      (assoc $compiler doc-uri
             {"compiler" (HyASTCompiler
                           :module ns
                           :filename (remove-uri-prefix doc-uri))
              "reader" (HyReader)})
      (get-hy-builder doc-uri ns key))))

(defn load-sym!
  [ns syms [pos None] [uri None] [recur? False] [scope ""]]
  (->> syms
       (filter-add-targets ns scope uri recur?)
       (map #%(add-sym! %1 ns pos uri scope))
       tuple))

(defn add-sym!
  [sym-hy/val ns [pos None] [doc-uri None] [scope ""]]
  "TODO: doc"
  (let [[sym-hy val] sym-hy/val
        docs (create-docs sym-hy val ns doc-uri)
        doc-uri (or doc-uri
                    (and (not (= ns "(builtin)"))
                         (isinstance val ModuleType)
                         (hasattr val "__file__")
                         val.__file__))]
    ($GLOBAL.add-$SYMS
      {"sym" (get-full-sym scope ns sym-hy) "type" val "uri" doc-uri
       "scope" scope "ns" ns "docs" docs "pos" pos})))

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
  [form [doc-uri "file:///dummy"] [ns "hyuga.sym.dummy"]]
  (let [result
        (hy.eval form
                 :locals hyuga.sym.dummy.__dict__)]
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
  (let [name (:name summary)]
    (when (and (-> f"(hasattr {name} \"__dict__\")" hy.read eval-in!)
               (-> f"(hasattr {name} \"__file__\")" hy.read eval-in!)
               (-> f"(in \"__builtins__\" ({name}.__dict__.keys))" hy.read eval-in!)
               (-> f"(in \"__hy_injected__\" (-> {name}.__dict__ (get \"__builtins__\") .keys))" hy.read eval-in!)
               (-> f"(get {name}.__dict__ \"__builtins__\" \"__hy_injected__\")" hy.read eval-in!)
               (re.search r".hy[c]*$" (-> f"{name}.__file__" hy.read eval-in!)))
      (-> f"{name}.__file__" hy.read eval-in!))))

(defn load-imported-pypkg!
  [summary ns doc-uri recur?]
  "TODO: doc"
  (let+ [{name "name" pos "pos"
          includes "includes"} summary
         pypkg-items (-> f"{name}.__dict__"
                         hy.read
                         (eval-in! doc-uri ns)
                         .items tuple)
         next-items (->> pypkg-items
                         filter-not-reserved
                         tuple)
         pypkg-ns (-> f"{name}.__name__"
                      hy.read (eval-in! doc-uri ns))
         filtered (cond
                    (= includes "*") next-items
                    (isinstance includes list)
                    (->> next-items
                         (filter #%(in (first %1) includes))
                         tuple)
                    True #())]
    ;; TODO: check imported syms in pypkg.(candidates can't find all syms...use getattr?)
    (logger.debug f"trying to load pypkg syms. name={name}, includes={includes}, pypkg-ns={pypkg-ns}, ns={ns}, doc-uri={doc-uri}, recur?={recur?}")
    (load-sym! pypkg-ns filtered pos doc-uri recur? ns)))

(defn load-pymodule-syms!
  [summary doc-uri recur?]
  "TODO: doc"
  (logger.debug f"load-pymodule-syms! summary={summary}")
  (let+ [{name "name"} summary]
    (load-imported-pypkg! {"name" name
                           "pos" None
                           "includes" "*"}
                          name
                          doc-uri recur?)))

(defn load-import!
  [form summary ns root-uri doc-uri recur?]
  "TODO: doc"
  (logger.debug f"load-import!: summary={summary}, ns={ns}, root-uri={root-uri}, doc-uri={doc-uri}, recur?={recur?}")
  (-> f"(import {(:name summary)})"
      (hy.read)
      (eval-in! doc-uri "hyuga.sym.dummy"))
  (let [fname (hy-src? summary doc-uri)]
    (if fname
      (load-hy-src! form fname root-uri)
  (load-imported-pypkg! summary ns doc-uri recur?))))

(defn load-class-methods!
  [ns name doc-uri summary recur?]
  (for [method-summary (:methods summary)]
    (let [method-name (:name method-summary)
          cls-name name
          method-pos (:pos method-summary)]
      (load-sym! ns
                 #(#(f"{cls-name}.{method-name}"
                      method-summary))
                 method-pos
                 doc-uri
                 recur?))))

(defn analyze-form!
  [form root-uri doc-uri ns recur? need-import?]
  "TODO: docs"
  (try
    (let+ [summary (get-form-summary form)
           ns (or ns (uri->mod root-uri doc-uri))
           {pos "pos" hytype "type" name "name"} summary]
      (logger.debug f"analyze-form!: summary={hytype}/{name}, doc-uri={doc-uri}, ns={ns}, recur?={recur?}")
      (when (= "defmacro" hytype)
        (load-macro! name ns pos doc-uri recur?))
      (when (= "require" hytype)
        (eval-in! form doc-uri))
      (when (and (or (= "require" hytype)
                     (= "import" hytype))
                 need-import?)
        (load-import! form summary ns
                      root-uri doc-uri recur?))
      (when (and need-import?
                 (or (= "defreader" hytype)
                     (= "require" hytype)))
        (load-required-macros! summary ns doc-uri recur?))
      (when (or (.startswith hytype "def")
                (= hytype "setv"))
        (load-sym! ns
                   #(#(name summary))
                   pos doc-uri recur?))
      (when (= hytype "defclass")
        (load-class-methods! ns name doc-uri
                             summary recur?)))
    (except [e BaseException]
            (log-warn "analyze-form!" e))
    (finally form)))

(defn prewalk-form!
  [root-uri doc-uri ns recur? need-import? form]
  "TODO: doc"
  (let [f #%(when (load-target? form)
              ;; TODO: fix for nested defn/defclass
              (analyze-form! form
                             root-uri
                             doc-uri
                             ns
                             recur?
                             need-import?)
              %1)]
    (walk (partial prewalk-form! root-uri doc-uri ns
                   recur? need-import?)
          #%(return %1)
          (f form))))

(defn walk-form!
  [forms root-uri doc-uri ns recur? need-import?]
  "TODO: doc"
  (try
    (prewalk-form! root-uri doc-uri ns recur? need-import? forms)
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
  [src root-uri doc-uri [ns None] [recur? False] [need-import? True]]
  "TODO: docs"
  (try
    (logger.debug f"load-src!: $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, ns={ns}, recur?={recur?}")
    (let [ns (or ns (uri->mod root-uri doc-uri))
          root-path (remove-uri-prefix root-uri)]
      (eval-in! `(import sys) doc-uri)
      ;; add import path root-uri
      (add-import-path! doc-uri root-path root-path)
      ;; add self as module
      (try
        (let [val (-> f"(and (import {ns}) ns)" hy.read
                      (eval-in! doc-uri))]
          (add-sym! #(ns val) "(venv)" #(0 0) doc-uri))
        (except [e BaseException]
          (logger.debug f"\tit seems not in namespace...skipping ns={ns}")))
      ;; read and analyze src
      (->> (hy.read-many src :filename doc-uri)
           (map #%(walk-form! %1
                              root-uri doc-uri
                              ns recur? need-import?))
           tuple))
    (except
      [e BaseException]
      (log-warn "load-src!" e))
    (else (logger.debug f"load-src!: finished. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, recur?={recur?}"))))

(defn load-builtin!
  []
  "TODO: docs"
  (load-sym! "(builtin)" (__builtins__.items)))

(defn load-hy-special!
  []
  "TODO: docs"
  (load-sym! "(hykwd)" (map #%(return #(%1 (hy.eval `(get-macro ~%1)))) hy-specials)))

(defn load-sys!
  []
  "TODO: docs"
  ;; TODO: toggle enable/disable to list sys.modules #11
  (load-sym!
    "(sysenv)"
    (->> modules .items
         (filter #%(and (not (.startswith (first %1) "hyuga.sym.dummy"))
                        (not (in f"(venv)\\{%1}" (->> ($GLOBAL.get-$SYMS) .keys)))
                        (not (.startswith (first %1) "_")))))))

(defn load-venv!
  [root-uri doc-uri]
  "Load venv."
  ;; TODO: user can set config #11
  (eval-in! `(import sys))
  (let [venv-path (get-venv root-uri)
        prev-sys-path (eval-in! `sys.path)]
    (logger.debug f"load-venv! venv-path={venv-path} prev-sys-path={prev-sys-path}")
    (when (isdir venv-path)
      (eval-in! `(import pkgutil))
      (eval-in! `(sys.path.insert 0 ~venv-path))
      (let [filter-fn #%(and (not (.startswith %1 "_"))
                             ;; FIXME: importing pip causes distutils AssertionError.
                             ;; @see https://github.com/pypa/setuptools/issues/3297
                             (not (= %1 "pip"))
                             (not (in f"(sysenv)\\{%1}" (->> ($GLOBAL.get-$SYMS) .keys))))
            syms (as-> `(pkgutil.iter-modules :path [~venv-path])
                   it
                   (eval-in! it doc-uri)
                   (map #%(. %1 name) it)
                   (filter filter-fn it))
            items (->> syms
                       (map #%(return
                                #(%1 (-> `(-> ~%1
                                              pkgutil.get-loader
                                              .load-module)
                                         (eval-in! doc-uri)))))
                       tuple)]
        (load-sym! "(venv)" items)))))

(defn load-required-macros!
  [summary ns doc-uri recur?]
  "TODO: doc"
  (let+ [{name "name" pos "pos"} summary
         items (-> f"({name}.__macros__.items)" hy.read (eval-in! doc-uri ns))
         filtered (->> items filter-not-reserved tuple)]
    (logger.debug f"load-required-macros! summary={summary}, ns={ns}, doc-uri={doc-uri}, recur?={recur?}")
    (load-sym! name filtered pos doc-uri recur?)))

(defn load-macro!
  [name ns pos uri recur?]
  "TODO: doc"
  (logger.debug f"load-macro! name={name}, ns={ns}, uri={uri}, recur?={recur?}")
  (let [items (-> f"({ns}.__macros__.items)"
                  (hy.read)
                  (eval-in! uri ns))
        matched (->> items
                     (filter #%(= name (first %1))))]
    (load-sym! ns matched pos uri recur?)))
