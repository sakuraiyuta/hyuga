(require hyrule * :readers *)

(import builtins)
(import toolz.itertoolz *)
(import textwrap [dedent])
(import pathlib [Path])

(defn path->uri
  [path [joins []]]
  (let [joined (hy.eval `(.joinpath path ~@joins))]
    (->> joined str (+ "file://"))))

(setv test-src-path
      (let [path (Path __file__)]
        (-> (nth 1 path.parents)
            (.joinpath "test_src"))))

(setv root-path (-> test-src-path
                    (.joinpath "root.hy")
                    str))

(setv
  vals
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
  "str-sample"
  {"sym"  "root\\root\\str-sample"
  "ns"    "root"
  "pos"   #(13 7)
  "scope" "root"
  "type"  {"docs" "\"sample value\""
           "name" "str-sample"
           "pos" #(13 7)
           "type" "setv"
            "value" (hy.models.String "sample value")}
  "uri"   root-path
  "docs"  (-> f"
  setv str-sample
  \t[root]
  
  `\"sample value\"`
  "
  dedent .strip)}

  ;-------------
  "int-sample"
  {"sym"  "root\\root\\int-sample"
  "ns"    "root"
  "pos"   #(15 7)
  "scope" "root"
  "type"  {"docs" "123"
           "name" "int-sample"
           "pos" #(15 7)
           "type" "setv"
            "value" (hy.models.Integer 123)}
  "uri"   root-path
  "docs"  (-> f"
  setv int-sample
  \t[root]
  
  `123`
  "
  dedent .strip)}

  ;-------------
  "dict-sample"
  {"sym"  "root\\root\\dict-sample"
  "ns"    "root"
  "pos"   #(17 7)
  "scope" "root"
  "type"  {"docs" "{:key 12345}"
           "name" "dict-sample"
           "pos" #(17 7)
           "type" "setv"
            "value" (hy.models.Dict [(hy.models.Keyword "key")
                                     (hy.models.Integer 12345)])}
  "uri"   root-path
  "docs"  (-> "
  setv dict-sample
  \t[root]
  
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
  {"sym"  "root\\root\\fn-sample"
  "ns"    "root"
  "pos"   #(5 7)
  "scope" "root"
  "type"  {"args" (hy.models.List)
           "decorators" None
           "docs" ""
           "name" "fn-sample"
           "pos" #(5 7)
           "type" "defn"}
  "uri"   root-path
  "docs"  (-> f"
  defn fn-sample None []
  \t[root]"
  dedent .strip (+ "\n\n"))}
  })
