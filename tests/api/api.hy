(require hyrule * :readers *)

(import builtins)
(import pytest)
(import textwrap [dedent])
(import pathlib [Path])

(import hyuga.api *)
(import ..fixture.sym [fixture-syms])

(setv uri (let [path (Path __file__)]
            (-> (nth 2 path.parents)
                (.joinpath "test_src" "root.hy")
                str)))

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
  {"sym"  "test-src.root\\test-src.root\\str-sample"
  "ns"    "test-src.root"
  "pos"   #(13 7)
  "scope" "test-src.root"
  "type"  {"docs" "\"sample value\""
           "name" "str-sample"
           "pos" #(13 7)
           "type" "setv"
            "value" (hy.models.String "sample value")}
  "uri"   uri
  "docs"  (-> f"
  setv str-sample
  \t[test-src.root]
  
  `\"sample value\"`
  "
  dedent .strip)}

  ;-------------
  "int-sample"
  {"sym"  "test-src.root\\test-src.root\\int-sample"
  "ns"    "test-src.root"
  "pos"   #(15 7)
  "scope" "test-src.root"
  "type"  {"docs" "123"
           "name" "int-sample"
           "pos" #(15 7)
           "type" "setv"
            "value" (hy.models.Integer 123)}
  "uri"   uri
  "docs"  (-> f"
  setv int-sample
  \t[test-src.root]
  
  `123`
  "
  dedent .strip)}

  ;-------------
  "dict-sample"
  {"sym"  "test-src.root\\test-src.root\\dict-sample"
  "ns"    "test-src.root"
  "pos"   #(17 7)
  "scope" "test-src.root"
  "type"  {"docs" "{:key 12345}"
           "name" "dict-sample"
           "pos" #(17 7)
           "type" "setv"
            "value" (hy.models.Dict [(hy.models.Keyword "key")
                                     (hy.models.Integer 12345)])}
  "uri"   uri
  "docs"  (-> "
  setv dict-sample
  \t[test-src.root]
  
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
  {"sym"  "test-src.root\\test-src.root\\fn-sample"
  "ns"    "test-src.root"
  "pos"   #(5 7)
  "scope" "test-src.root"
  "type"  {"args" (hy.models.List)
           "decorators" None
           "docs" ""
           "name" "fn-sample"
           "pos" #(5 7)
           "type" "defn"}
  "uri"   uri
  "docs"  (-> f"
  defn fn-sample None []
  \t[test-src.root]"
  dedent .strip (+ "\n\n"))}
  })

(defn [(pytest.mark.parametrize
         #("full_sym" "expected")
         [#("defn" (get vals "defn"))
          #("vars" (get vals "vars"))
          #("str-sample" (get vals "str-sample"))
          #("int-sample" (get vals "int-sample"))
          #("dict-sample" (get vals "dict-sample"))
          #("fn-sample" (get vals "fn-sample"))])]
  test_get_details
  [full_sym expected fixture-syms]
  (let+ [{root-uri :root-uri
          doc-uri :doc-uri} fixture-syms
         result (get-details full_sym root-uri doc-uri)]
    (assert (= result expected))))
;
;(defn [(pytest.mark.parametrize
;         #("val" "expected")
;         [#("eval"
;             #({"sym"   "eval"
;                "type"  eval
;                "scope" "builtin"
;                "docs"  (docs-str eval "builtin")}))
;          #("filter"
;             #({"sym"   "attr.filters"
;                "type"  attr.filters
;                "scope" "sys"
;                "docs"  (docs-str attr.filters "sys")}
;               {"sym"   "filter"
;                "type"  filter
;                "scope" "builtin"
;                "docs"  (docs-str filter "builtin")}))])]
;  test_get-candidates
;  [val expected fixture-syms]
;  (assert (= (get-candidates val) expected)))
