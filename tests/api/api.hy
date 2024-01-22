(require hyrule * :readers *)

(import pytest)

(import hyuga.api *)
(import ..asserts.details [details])
(import ..asserts.candidates [get-expected-candidates])
(import ..fixture.sym [fixture-syms])

(defn [(pytest.mark.parametrize
         #("full_sym" "expected")
         [#("defn" (get details "defn"))
          #("vars" (get details "vars"))
          #("str-sample" (get details "str-sample"))
          #("int-sample" (get details "int-sample"))
          #("dict-sample" (get details "dict-sample"))
          #("fn-sample" (get details "fn-sample"))])]
  test_get_details
  [full_sym expected fixture-syms]
  (let+ [{root-uri :root-uri
          doc-uri :doc-uri} fixture-syms
         result (get-details full_sym root-uri doc-uri)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("prefix" "expected")
         [#("defn" (get-expected-candidates
                     ["defn" "defn/a"]))])]
  test_get_candidates
  [prefix expected fixture-syms]
  (let+ [{root-uri :root-uri
          doc-uri :doc-uri} fixture-syms
         result (get-candidates prefix root-uri doc-uri)]
    (assert (= result expected))))
