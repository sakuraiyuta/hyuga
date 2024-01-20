(require hyrule * :readers *)

(import builtins)
(import pytest)
(import attr)
(import textwrap [dedent])

(import hyuga.api *)
(import .fixture [fixture-syms])

(setv
  vals
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

  "fn-sample"
  {"sym"  "sample-src.root\\sample-src.root\\fn-sample"
  "ns"    "sample-src.root"
  "pos"   #(1 7)
  "scope" "sample-src.root"
  "type"  {"args" (hy.models.List)
           "decorators" None
           "docs" ""
           "name" "fn-sample"
           "pos" #(1 7)
           "type" "defn"}
  "uri"   (-> __file__ os.path.dirname (os.path.join "sample_src" "root.hy"))
  "docs"  (-> f"
  defn fn-sample None []
  \t[sample-src.root]"
  dedent .strip (+ "\n\n"))}
  })

(defn [(pytest.mark.parametrize
         #("full_sym" "expected")
         [#("defn" (get vals "defn"))
          #("vars" (get vals "vars"))
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
