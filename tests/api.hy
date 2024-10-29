(require hyrule * :readers *)

(import pytest)
(import os)

(import hyuga.api *)
(import fixture [fixture-syms])
(import misc *)

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("eval"
             {"sym"   "\\(builtin)\\eval"
              "type"  eval
              "uri"   False
              "scope" ""
              "ns"    "(builtin)"
              "docs"  (docs-str eval "(builtin)")
              "pos"   None})
          #("first"
             {"sym"   ".None\\toolz.itertoolz\\first"
              "type"  first
              "uri"   "."
              "scope" ".None"
              "ns"    "toolz.itertoolz"
              "docs"  (docs-str first "toolz.itertoolz")
              "pos"   #(2, 15)})])]
  test_get-details
  [val expected fixture-syms pytestconfig]
  (setv roots (+ "file://" (os.path.join pytestconfig.rootdir "tests" "api.hy")))
;   (print (get-details val roots roots))
;   (print)
;   (print expected)
  (assert (= (get-details val roots roots) expected)))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("eval" 
            #(#("\\(builtin)\\eval" 
                {"sym"   "\\(builtin)\\eval"
                 "type"  eval
                 "uri"   False
                 "scope" ""
                 "ns"    "(builtin)"
                 "docs"  (docs-str eval "(builtin)")
                 "pos"   None}) 
              #("\\(hykwd)\\eval-and-compile" 
                {"sym"   "\\(hykwd)\\eval-and-compile"
                 "type"  (get-macro eval-and-compile)
                 "uri"   False
                 "scope" ""
                 "ns"    "(hykwd)"
                 "docs"  (docs-str (get-macro eval-and-compile) "(hykwd)")
                 "pos"   None}) 
              #("\\(hykwd)\\eval-when-compile" 
                {"sym"   "\\(hykwd)\\eval-when-compile" 
                 "type"  (get-macro eval-when-compile) 
                 "uri"   False
                 "scope" ""
                 "ns"    "(hykwd)"
                 "docs"  (docs-str (get-macro eval-when-compile) "(hykwd)")
                 "pos"   None})))
         ;  #("filter" 
         ;    #(#("\\(builtin)\\filter"
         ;        {"sym"   "\\(builtin)\\filter"
         ;         "type"  filter
         ;         "uri"   False
         ;         "scope" ""
         ;         "ns"    "(builtin)"
         ;         "docs"  (docs-str filter "(builtin)")
         ;         "pos"   None})
         ;      #("\\(sysenv)\\filter"
         ;        {"sym"   "\\(sysenv)\\filter"
         ;         "type"  (importlib.import_module "sym.filter" :package "filter")
         ;         "uri"   False
         ;         "scope" "" 
         ;         "ns"    "(sysenv)"
         ;         "docs"  (docs-str (importlib.import_module "sym.filter" :package "filter") "(sysenv)")
         ;         "pos"   None})))
          ])]
  test_get-candidates
  [val expected fixture-syms pytestconfig]
  (setv roots (+ "file://" (os.path.join pytestconfig.rootdir "tests" "api.hy")))
;   (print (get-candidates val roots roots))
;   (print)
;   (print expected)
  (assert (= (get-candidates val roots roots) expected))) eval
