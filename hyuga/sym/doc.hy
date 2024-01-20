(require hyrule * :readers *)

(import hyrule.hypprint [pformat])

(import hyuga.log *)
(import hyuga.sym.helper *)
(import hyuga.sym.eval [eval-in!])

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
                    docs "docs"} symtype]
              f"defn {sym-hy} {(fix-hy-symbol decorators)} {(fix-hy-symbol args)}\n\t[{scope}]\n\n{docs}")

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
      (let [docs (or symtype.__doc__ "No docs.")]
        f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{docs}")

      (except
          [e BaseException]
          (log-warn "create-docs" e)
          f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n(cannot load docs.)"))))

