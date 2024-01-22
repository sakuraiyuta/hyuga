(require hyrule * :readers *)

(import pytest)

(import hyuga.api *)
(import ..define [vals])
(import ..fixture.sym [fixture-syms])

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
