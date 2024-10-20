(require hyrule * :readers *)

(import pytest)
(import toolz.itertoolz [first])

(import hyuga.sym.loader *)

(import fixture [fixture-syms])
(import misc *)

(defn [(pytest.mark.parametrize 
        #("val" "scope" "expected") 
        [#(["eval" eval] 
           "local" 
           {"sym" "\\local\\eval"
            "type" eval
            "uri" False
            "scope" ""
            "ns" "local"
            "docs" (docs-str eval "local")
            "pos" None}) 
         #(["first" first] 
           "global" 
           {"sym" "\\global\\first"
            "type" first
            "uri" False
            "scope" ""
            "ns" "global"
            "docs" (docs-str first "global")
            "pos" None})])]
  test_add-sym
  [val scope expected fixture-syms]
  (print (add-sym! val scope))
  (print)
  (print expected)
  (assert (= (add-sym! val scope) expected)))