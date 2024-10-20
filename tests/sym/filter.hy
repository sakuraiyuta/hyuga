(require hyrule * :readers *)

(import pytest)
(import toolz.itertoolz [first])
(import toolz)

(import hyuga.api *)

(import hyuga.sym.filter *)
(import fixture [fixture-syms])
(import misc *)

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#(["eval" None] True)
          #(["first" None] True)
          #(["toolz" None] True)
          #(["_hy-let" None] False)
          #(["_hy-anon" None] False)
          #(["-hyuga-eval-form" None] False)])]
  test_not-exclude-sym
  [val expected fixture-syms]
  (assert (= (not-exclude-sym? val) expected)))

(defn [(pytest.mark.parametrize
         #("val" "mod" "scope" "uri" "expected")
         [#(["filter" None] "(builtin)" "" False False)
          #(["first" None] "toolz.itertoolz" ".None" "." False)
          #(["toolz" None] "(sysenv)" "" toolz.__file__ False)
          #(["!not-in-sym!" None] "" "" False True)])]
  test_not-in-sym
  [val mod scope uri expected fixture-syms pytestconfig]
  (setv roots (+ "file://" (os.path.join pytestconfig.rootdir "tests" "sym" "filter.hy")))
;   (print (get-candidates (first val) roots roots))
;   (print)
;   (print (not-in-$SYM? mod scope uri val))
;   (print)
;   (print expected)
  (assert (= (not-in-$SYM? mod scope uri val) expected)))