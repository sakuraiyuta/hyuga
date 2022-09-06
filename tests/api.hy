(require hyrule * :readers *)

(import pytest)

(import hyuga.global [$GLOBAL])
(import hyuga.api *)
(import fixture [fixture-syms])

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("eval" {"sym" "eval"
                    "type" eval
                    "scope" "builtin"
                    "docs"  f"eval {eval}\n\tbuiltin\n\n{eval.__doc__}"})
          #("first" {"sym" "first"
                     "type" first
                     "scope" "local"
                     "docs"  f"first {first}\n\tlocal\n\n{first.__doc__}"})])]
  test_get-details
  [val expected fixture-syms]
  (assert (= (get-details val) expected)))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("eval"
             #({"sym"   "eval"
                "type"  eval
                "scope" "builtin"
                "docs"  f"eval {eval}\n\tbuiltin\n\n{eval.__doc__}"}))
          #("fi"
             #({"sym"   "first"
                "type"  first
                "scope" "local"
                "docs"  f"first {first}\n\tlocal\n\n{first.__doc__}"}
               {"sym"   "filter"
                "type"  filter
                "scope" "builtin"
                "docs"  f"filter {filter}\n\tbuiltin\n\n{filter.__doc__}"}))])]
  test_get-candidates
  [val expected fixture-syms]
  (assert (= (get-candidates val) expected)))
