(require hyrule * :readers *)

(import hyuga.sym.helper [sym-py->hy])

(defn docs-str
  [sym scope]
  f"{(sym-py->hy sym.__name__)} [{scope}]\n\t{sym}\n\n{sym.__doc__}")
