(require hyrule * :readers *)

(import builtins)
(import textwrap [dedent])

(import .path *)
(import hyuga.sym.helper [sym-hy->py])

(setv
  details
  ;-------------
  {"defn"
  {"sym"  "\\(hykwd)\\defn"
  "ns"    "(hykwd)"
  "pos"   None
  "scope" ""
  "type"  (get builtins._hy_macros "defn")
  "uri"   False
  "docs"
  (-> f"
  defn [(hykwd)]
  \t{(get builtins._hy_macros "defn")}

  No docs.
  "
  dedent .strip)
  }

  ;-------------
  "defn/a"
  {"sym"  "\\(hykwd)\\defn/a"
  "ns"    "(hykwd)"
  "pos"   None
  "scope" ""
  "type"  (get builtins._hy_macros (sym-hy->py "defn/a"))
  "uri"   False
  "docs"
  (-> f"
  defn/a [(hykwd)]
  \t{(get builtins._hy_macros (sym-hy->py "defn/a"))}

  No docs.
  "
  dedent .strip)
  }

  ;-------------
  "str-sample"
  {"sym"  "doc\\doc\\str-sample"
  "ns"    "doc"
  "pos"   #(7 7)
  "scope" "doc"
  "type"  {"docs" "\"sample value\""
           "name" "str-sample"
           "pos" #(7 7)
           "type" "setv"
            "value" (hy.models.String "sample value")}
  "uri"   (str test-src-doc-path)
  "docs"  (-> f"
  setv str-sample
  \t[doc]
  
  `\"sample value\"`
  "
  dedent .strip)}

  ;-------------
  "int-sample"
  {"sym"  "doc\\doc\\int-sample"
  "ns"    "doc"
  "pos"   #(8 7)
  "scope" "doc"
  "type"  {"docs" "123"
           "name" "int-sample"
           "pos" #(8 7)
           "type" "setv"
            "value" (hy.models.Integer 123)}
  "uri"   (str test-src-doc-path)
  "docs"  (-> f"
  setv int-sample
  \t[doc]
  
  `123`
  "
  dedent .strip)}

  ;-------------
  "dict-sample"
  {"sym"  "doc\\doc\\dict-sample"
  "ns"    "doc"
  "pos"   #(9 7)
  "scope" "doc"
  "type"  {"docs" "{:key 12345}"
           "name" "dict-sample"
           "pos" #(9 7)
           "type" "setv"
            "value" (hy.models.Dict [(hy.models.Keyword "key")
                                     (hy.models.Integer 12345)])}
  "uri"   (str test-src-doc-path)
  "docs"  (-> "
  setv dict-sample
  \t[doc]
  
  `{:key 12345}`
  "
  dedent .strip)}

  ;-------------
  "vars"
  {"sym"  "\\(builtin)\\vars"
  "ns"    "(builtin)"
  "pos"   None
  "scope" ""
  "type"  (get (vars builtins) "vars")
  "uri"   False
  "docs"  (-> f"
  vars [(builtin)]
  \t{(get (vars builtins) "vars")}

  vars([object]) -> dictionary

  Without arguments, equivalent to locals().
  With an argument, equivalent to object.__dict__.
  "
  dedent .strip)}

  ;-------------
  "fn-sample"
  {"sym"  "doc\\doc\\fn-sample"
  "ns"    "doc"
  "pos"   #(3 7)
  "scope" "doc"
  "type"  {"args" (hy.models.List)
           "decorators" None
           "docs" ""
           "name" "fn-sample"
           "pos" #(3 7)
           "type" "defn"}
  "uri"   (str test-src-doc-path)
  "docs"  (-> f"
  defn fn-sample None []
  \t[doc]"
  dedent .strip (+ "\n\n"))}
  })
