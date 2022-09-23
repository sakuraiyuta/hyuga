(require hyrule * :readers *)
(import hyrule.iterables [butlast drop-last rest])
(import hyrule.collections [prewalk walk])

(import hy.models [Object Expression List String Symbol])
(import toolz.itertoolz *)
(import re [sub])

(import hyuga.log *)
(import hyuga.global [$GLOBAL])

(defn remove-uri-prefix
  [uri]
  (sub "^[a-z]+://" "" uri))

(defn get-ns
  [symkey]
  (-> symkey get-scope/ns second get-ns/sym first))

(defn get-sym
  [symkey]
  (-> symkey get-scope/ns second get-ns/sym second))

(defn get-ns/sym
  [val]
  (let [splitted (.split val ".")]
    [(->> splitted
       (drop-last 1)
       tuple
       (.join "."))
     (last splitted)]))

(defn get-scope/ns
  [symkey]
  (let [splitted (.split symkey "\\")]
    (if (> (count splitted) 1)
      splitted
      ["" (first splitted)])))

(defn get-full-sym
  [prefix sym]
  (if prefix
    (+ prefix "\\" sym)
    (+ "(unknown)\\" sym)))

(defn not-exclude-sym?
  [sym-hy/val]
  (and (not (.startswith (first sym-hy/val) "_hy-"))
       (!= (first sym-hy/val) "-hyuga-eval-form")))

(defn judge-scope
  [val expect-scope]
  "TODO: doc"
  (branch (in (str val) it)
          "builtin" "builtin"
          else expect-scope))

(defn not-in-$SYM?
  [prefix sym-hy/val]
  (let [full-sym (get-full-sym prefix (first sym-hy/val))
        keys (.keys ($GLOBAL.get-$SYMS))]
    (not (in full-sym keys))))

(defn sym-py/val->sym-hy/val
  [sym-py/val]
  [(->> sym-py/val first sym-py->hy)
   (second sym-py/val)])

(defn sym-py->hy
  [sym-py]
  (->> (.split sym-py ".")
       (map #%(if %1 (hy.unmangle %1) %1))
       (.join ".")))

(defn sym-hy->py
  [sym-hy]
  (->> (.split sym-hy ".")
       (map #%(if %1 (hy.mangle %1) %1))
       (.join ".")))

(defn -get-macro-doc
  [sym-hy symtype]
  "Get macro documents.
  FIXME: So dirty hack!"
  (let [eval-tgt
        `(do
           (import io)
           (import contextlib [redirect-stdout])
           (with [buf (io.StringIO)
                  _ (redirect-stdout buf)]
             (doc ~sym-hy)
             (buf.getvalue)))]
    (hy.eval eval-tgt
             :locals
             {(sym-hy->py sym-hy) symtype})))

(defn -get-help
  [sym-hy symtype]
  "Get macro documents.
  FIXME: So dirty hack!"
  (let [eval-tgt
        `(do
           (import io)
           (import contextlib [redirect-stdout])
           (with [buf (io.StringIO)
                  _ (redirect-stdout buf)]
             (help ~sym-hy)
             (buf.getvalue)))]
    (hy.eval eval-tgt
             :locals
             ($GLOBAL.get-$SYMS))))

(defn create-docs
  [sym-hy symtype scope uri]
  "TODO: doc"
  (logger.debug f"create-docs: sym-hy={sym-hy}, scope={scope}, uri={uri}")
  (if (isinstance symtype Expression)
    (branch (= it (-> symtype first str))
            "defclass"
            (let+ [{inherits "inherits"
                    docstr "docs"} (get-defclass-summary symtype)]
              f"{sym-hy} {(hy.repr inherits)}\n[{scope}] Hy defined\n\n{docstr}")
            "defn"
            (let+ [{args "args"
                    decorators "decorators"
                    docstr "docs"} (get-defn-summary symtype)]
              f"{sym-hy} {(hy.repr decorators)} {(hy.repr args)}\n[{scope}] Hy defined\n\n{docstr}")
            else f"unknown")
    (try
      (logger.debug f"-----test {symtype} {(dir symtype)}")
      (if (or (= "None" sym-hy)
              (= "hy-special" scope)
              (= "hy-macro" scope))
        f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{(-get-macro-doc sym-hy symtype)}"
        (let [docstr (or symtype.__doc__ "No docs.")]
          f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{docstr}"))
      (except
        [e BaseException]
        (try
          (logger.debug f"tyring to read doc. sym-hy={sym-hy}, scope={scope}, uri={uri} e={e}")
          f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{(-get-macro-doc sym-hy symtype)}"
          (except
            [e BaseException]
            (try
              (logger.debug f"tyring to read help. sym-hy={sym-hy}, scope={scope}, uri={uri} e={e}")
              f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{(-get-help sym-hy symtype)}"
              (except
                [e BaseException]
                (log-warn "create-docs" e)
                f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n(cannot load docs.)"))))))))

(defn module-or-class?
  [sym-splitted]
  "TODO: doc"
  (if (-> sym-splitted count (> 1))
    (->> sym-splitted (drop-last 1) (.join "."))
    ""))

(defn get-module-in-syms
  [sym-hy]
  "TODO: doc"
  (logger.debug f"get-module-in-syms: sym-hy={sym-hy}")
  (->> ($GLOBAL.get-$SYMS)
       .items
       (filter #%(= sym-hy (-> %1 first get-sym)))
       first second :type))

(defn get-module-attrs
  [module-name]
  "TODO: doc"
  (try
    (let [module (get-module-in-syms module-name)]
      (logger.debug f"get-module-attrs: module={module}")
      (module.__dict__.items))
    (except [e BaseException]
            (error-trace logger.warning "get-module-attrs" e))))

(defn hy-module?
  [module]
  "TODO: src"
  (->> module dir (in "hy")))

(defn get-module-hy-src
  [module]
  "TODO: src"
  (if (->> module dir (in "__file__"))
    (let [fpath (module.__file__)]
      (with [f (open fpath)]
        (hy.read-many f :filename fpath)))
    None))

(defn get-form-pos
  [form]
  (branch (.startswith (-> form first str) it)
          "def" #((getattr form "start_line")
                  (getattr form "start_column"))
          "setv" #((getattr form "start_line")
                   (getattr form "start_column"))
          ;; TODO: implement
          "import" None
          else None))

(defn get-defn-summary
  [form]
  (setv ret {"name" ""
             "docs" ""
             "decorators" None
             "args" ""
             "pos" None})
  (if (-> form second (isinstance List))
    (do
      (.update ret {"decorator" (second form)})
      (.update ret {"name" (nth 2 form)})
      (.update ret {"pos" #((getattr (nth 2 form) "start_line")
                            (getattr (nth 2 form) "start_column"))})
      (.update ret {"args" (nth 3 form)})
      (when (isinstance (nth 4 form) String)
        (.update ret {"docs" (-> (nth 4 form) str)})))
    (do
      (.update ret {"name" (second form)})
      (.update ret {"pos" #((getattr (second form) "start_line")
                            (getattr (second form) "start_column"))})
      (.update ret {"args" (->> form (nth 2))})
      (when (isinstance (nth 3 form) String)
        (.update ret {"docs" (-> (nth 3 form) str)}))))
  ret)

(defn get-defclass-summary
  [form]
  (setv ret {"name" (-> form second str)
             "docs" ""
             "inherits" (nth 2 form)
             ;; TODO: implement
             "methods" []
             "pos" #((getattr (second form) "start_line")
                     (getattr (second form) "start_column"))})
  (walk #%(when (isinstance %1 Expression)
            (let [summary (get-defn-summary %1)
                  methods (get ret "methods")]
              (.append methods summary)
              (.update ret {"methods" methods}))
            %1)
        #%(return %1)
        (drop 3 form))
  (when (isinstance (nth 3 form) String)
    (.update ret {"docs" (-> (nth 3 form) str)}))
  ret)

(defn get-setv-summary
  [form]
  {"name" (-> form second str)
   "docs" (try (hy.eval (nth 2 form))
               (except [e Exception]
                       "(can't eval)"))
   "pos" #((getattr (second form) "start_line")
           (getattr (second form) "start_column"))})

(defn get-import-summary
  [form]
  (setv ret {"name" (-> form second str)
             "includes" []})
  (let [options (list (drop 2 form))]
    (print (count options))
    (when (-> options count (> 0))
      (let [option (first options)]
        (if (isinstance option List)
          (.update ret {"includes" (->> option
                                        (walk #%(-> %1 hy.repr (.lstrip "'"))
                                          #%(return %1))
                                        hy.eval
                                        (filter #%(not (= ":as" %1)))
                                        (map sym-hy->py)
                                        tuple)})
          (.update ret {"includes" "*"})))))
  ret)
