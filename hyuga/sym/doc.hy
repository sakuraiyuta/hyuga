(require hyrule * :readers *)

(import hyuga.log *)
(import hyuga.sym.helper *)

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
                    docs "docs"} symtype]
              f"setv {sym-hy}\n\t[{scope}]\n\n{docs}")
            else f"unknown")
    (try
      (if (or (= "None" sym-hy)
              (= "hy-special" scope)
              (= "hy-macro" scope))
        f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{(-get-macro-doc sym-hy symtype)}"
        (let [docs (or symtype.__doc__ "No docs.")]
          f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{docs}"))
      (except
        [e BaseException]
        (try
          (logger.debug f"trying to read doc. sym-hy={sym-hy}, scope={scope}, uri={uri} e={e}")
          f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{(-get-macro-doc sym-hy symtype)}"
          (except
            [e BaseException]
            (try
              (logger.debug f"trying to read help. sym-hy={sym-hy}, scope={scope}, uri={uri} e={e}")
              f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{(-get-help sym-hy symtype)}"
              (except
                [e BaseException]
                (log-warn "create-docs" e)
                f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n(cannot load docs.)"))))))))

