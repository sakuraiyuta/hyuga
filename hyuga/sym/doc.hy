(require hyrule * :readers *)

(import hyrule.hypprint [pformat])
(import importlib.machinery [ModuleSpec])
(import ast [get-docstring])

(import hyuga.log *)
(import hyuga.sym.helper *)
(import hyuga.sym.eval [eval-in!])
(import hyuga.sym.module *)

(defn get-doc-from-spec
  [spec]
  (when (and (not (spec-is-hy? spec))
             (spec-is-py? spec))
    (let [ast (get-ast spec)]
    (get-docstring ast))))

(defn create-docs
  [sym-hy symtype scope uri]
  "TODO: doc"
  (if (and (isinstance symtype dict)
           (in "type" (symtype.keys)))
    (branch (= it (:type symtype))

            "defclass"
            (let+ [{inherits "inherits"
                    docs "docs"} symtype]
              f"defclass {sym-hy} {(fix-hy-symbol inherits)}\n\t[{scope}]\n\n{docs}")

            "defn"
            (let+ [{args "args"
                    decorators "decorators"
                    argtypes "argtypes"
                    docs "docs"} symtype]
              f"<{scope}>\n\ndefn {(fix-hy-symbol decorators)} :tp {(fix-hy-symbol argtypes)} {sym-hy}\n\t{(fix-hy-symbol args)}\n\n{docs}")

            "setv"
            (let+ [{args "args"
                    value "value"
                    docs "docs"} symtype
                   val (if (or (= (type value) hy.models.Expression)
                               (= (type value) hy.models.Symbol))
                         (hy.repr value)
                         (pformat (eval-in! value scope)))]
              f"setv {sym-hy}\n\t[{scope}]\n\n`{val}`")

            else f"unknown")
    (try
      (let [docs (if (isinstance symtype ModuleSpec)
                   (get-doc-from-spec symtype)
                   (or symtype.__doc__ "No docs."))]
        (-> f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{docs}" .strip))

      (except
          [e BaseException]
          (log-warn "create-docs" e)
          f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n(cannot load docs.)"))))

