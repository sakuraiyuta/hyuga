(require hyrule * :readers *)
(require hyrule.collections [assoc])
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
(import importlib.machinery [ModuleSpec])

(import .eval [eval-in!])
(import .helper *)
(import .spec *)
(import .summary.core [get-form-summary])
(import .doc [create-docs])
(import .filter [filter-add-targets
                 filter-not-reserved])
(import .venv [remove-uri-prefix get-venv])
(import ..log *)

(defn load-sym!
  [ns syms [pos None] [uri None] [recur? False] [scope ""]]
  (->> syms tuple ;; for avoid `RuntimeError: dictionary changed size during iteration`
       ;; TODO: check load target is hy-src?
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
      {"sym" sym-hy
       "type" val
       "uri" doc-uri
       "scope" scope
       "ns" ns
       "docs" docs
       "pos" pos})))

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

(defn load-hy-src!
  [form fname root-uri ns]
  "TODO: doc"
  (logger.debug f"trying to load hy-source. fname={fname}")
  (with [file (open fname)]
    (-> (file.read)
        (load-src! root-uri f"file://{file.name}" ns
                   False False))))

(defn hy-src?
  [summary]
  "TODO: doc"
  (logger.debug f"hy-src? summary={summary}")
  (let [name (:name summary)
        mod-spec (find-loaded-spec {:sym name})
        mod (when mod-spec (get-module mod-spec))]
    (when (and mod
               (hasattr mod "__file__")
               (in "__hy_injected__" (-> mod dir))
               (re.search r".hy[c]*$" mod.__file__))
      mod.__file__)))

(defn load-imported-pypkg!
  [summary ns doc-uri recur?]
  "TODO: doc"
  ;; TODO: use Python AST
  (let+ [{name "name" pos "pos" includes "includes"} summary
         _ (logger.debug f"trying to load pypkg syms. name={name}, includes={includes}, ns={ns}, doc-uri={doc-uri}, recur?={recur?}")
         spec (find-loaded-spec {:sym name
                                 :ns name})
         pypkg-items (-> (find-loaded-spec {:sym name
                                            :ns name})
                         get-module
                         vars .items)
         next-items (->> pypkg-items
                         filter-not-reserved
                         tuple)
         filtered (cond
                    (= includes "*") next-items
                    (isinstance includes list)
                    (->> next-items
                         (filter #%(in (first %1) includes))
                         tuple)
                    True #())]
    (load-sym! name filtered pos doc-uri recur? ns)))

(defn load-pymodule-syms!
  [summary doc-uri recur? editting-mod]
  "TODO: doc"
  (logger.debug f"load-pymodule-syms! summary={summary}")
  (let+ [{name "name"} summary]
    (load-imported-pypkg! {"name" name
                           "pos" None
                           "includes" "*"}
                          editting-mod
                          doc-uri recur?)))

(defn load-import!
  [form summary ns root-uri doc-uri recur?]
  "TODO: doc"
  (logger.debug f"load-import!: summary={summary}, ns={ns}, root-uri={root-uri}, doc-uri={doc-uri}, recur?={recur?}")
  (let+ [{name "name" uri "uri"} summary]
    (try
      (let [mod-spec (or (find-loaded-spec {:sym name :ns name})
                         (get-spec-by-name name))
            mod (get-module mod-spec)
            fname (mod-is-hy? mod)]
        (load-sym! name (-> mod vars .items) #(1 1)
                   (if (hasattr mod "__file__")
                     mod.__file__
                     None)
                   False
                   ns)
        (if fname
          ;; TODO: fix loading definition other src's symbol
          (load-hy-src! form fname root-uri name)
          (load-imported-pypkg! summary ns doc-uri recur?)))
      (except [e BaseException]
        (log-error "load-import" e)))))

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
      (when (= "require" hytype)
        (eval-in! form))
      (when (and (or (= "require" hytype)
                     (= "import" hytype))
                 need-import?)
        (load-import! form summary ns
                      root-uri doc-uri recur?))
      (when (or (.startswith hytype "def")
                (= hytype "setv")
                (= hytype "setx"))
        (load-sym! ns
                   #(#(name summary))
                   pos doc-uri recur? ns))
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

(defn load-src!
  [src root-uri doc-uri [ns None] [recur? False] [need-import? True]]
  "TODO: docs"
  (logger.debug f"load-src!: $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, ns={ns}, recur?={recur?}")
  (try
    (let [root-path (remove-uri-prefix root-uri)]

      (try
        ;; try import self as module/package
        (eval-in! `(import sys pkgutil) ns)
        (when (and ns
                   (eval-in! `(not (in ~root-path sys.path)) ns))
          (logger.debug f"\tadd sys.path and import self: root-path={root-path}, ns={ns}")
          ;(load-sym! ns (get-modules-by-pkgutil root-path))
          )

        (except [e BaseException]
          (log-warn f"\tit seems not in namespace...skipping ns={ns}" e)
          ;; remove ns and try again
          (when (not ns) (load-src! src root-uri doc-uri None True)))

        (else
          ;; read and analyze src
          (->> (hy.read-many src :filename doc-uri)
               (map #%(walk-form! %1
                                  root-uri doc-uri
                                  ns recur? need-import?))
               tuple))))

    (else (logger.debug f"load-src!: finished. $SYMS.count={(->> ($GLOBAL.get-$SYMS) count)}, root-uri={root-uri}, doc-uri={doc-uri}, recur?={recur?}"))))

(defn load-builtin!
  []
  "TODO: docs"
  (eval-in! `(import builtins))
  (as-> `(.items (vars builtins)) it
       (eval-in! it)
       (load-sym! "(builtin)" it
                  None None False "(builtin)")))

(defn load-hy-kwd!
  []
  "TODO: docs"
  (load-sym! "(hykwd)" (get-hy-macros)
             None None False "(hykwd)"))

(defn load-sys!
  []
  "TODO: docs"
  ;; TODO: toggle enable/disable to list sys.modules #11
  (->> modules .items
       (filter #%(let [name (-> %1 first sym-py->hy)]
                   (not (find-loaded-spec {:sym name
                                           :ns name
                                           :scope "(venv)"}))))
       (map #%(let [name (-> %1 first sym-py->hy)
                    entity (second %1)]
                (load-sym! name
                          [[name entity]]
                          #(1 1)
                          (getattr entity "__file__" None)
                          entity
                          "(sysenv)")))
       tuple))

(defn load-venv!
  [root-uri]
  "Load venv."
  ;; TODO: user can set config #11
  (let [venv-path (get-venv root-uri)]
    (logger.debug f"load-venv! venv-path={venv-path}")
    (when (isdir venv-path)
      (->> (get-specs [venv-path])
           (filter #%(let [name (-> %1.name sym-py->hy)]
                       (not (find-loaded-spec {:sym name
                                               :ns name
                                               :scope "(sysenv)"}))))
           (map #%(load-sym! (sym-py->hy %1.name)
                             [[%1.name %1]]
                             #(1 1)
                             %1.origin
                             %1
                             "(venv)"))
           tuple))))
