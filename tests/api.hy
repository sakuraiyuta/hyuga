(require hyrule * :readers *)

(import pytest)
(import attr)

(import hyuga.api *)
(import fixture [fixture-syms])

;(defn [(pytest.mark.parametrize
;         #("val" "expected")
;         [#("eval"
;             {"sym"   "eval"
;              "type"  eval
;              "scope" "builtin"
;              "docs"  (docs-str eval "builtin")})
;          #("first"
;             {"sym"   "first"
;              "type"  first
;              "scope" "local"
;              "docs"  (docs-str first "local")})])]
;  test_get-details
;  [val expected fixture-syms]
;  (assert (= (get-details val) expected)))
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
