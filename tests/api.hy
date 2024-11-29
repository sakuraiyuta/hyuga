(require hyrule * :readers *)

(import pytest)
(import os)

(import hyuga.api *)
(import fixture [fixture-syms])
(import misc *)

(defn [(pytest.mark.parametrize
        #("src" "root_uri" "doc_uri" "expected")
        [#("(import toolz.itertoolz *)"
           "."
           "."
           None)])]
  test_parse-src
  [src root-uri doc-uri expected]
  (print f"expected: { expected }" )
  (print f"got:      { (parse-src! src root-uri doc-uri) }")
  (assert (= (parse-src! src root-uri doc-uri) expected)))

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
              "pos"   #(1 9)})])]
  test_get-details
  [val expected fixture-syms pytestconfig]
  (setv roots (+ "file://" (os.path.join pytestconfig.rootdir "tests" "api.hy")))
  (print f"expected: { expected }" )
  (print f"got:      { (get-details val roots roots) }")
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
                 "pos"   None})))])]
  test_get-candidates
  [val expected pytestconfig]
  (setv roots (+ "file://" (os.path.join pytestconfig.rootdir "tests" "api.hy")))
  (print f"expected: { expected }" )
  (print f"got:      { (get-candidates val roots roots) }")
  (assert (= (get-candidates val roots roots) expected)))

(defn [(pytest.mark.parametrize
        #("tgt_full_sym" "root_uri" "doc_uri" "expected")
        [#("eval"
           "."
           "."
           #(#("\\(builtin)\\eval" 
               {"sym"   "\\(builtin)\\eval"
                "type"  eval
                "uri"   False
                "scope" ""
                "ns"    "(builtin)"
                "docs"  (docs-str eval "(builtin)")
                "pos"   None})) )])]
  test_get-matches
  [tgt-full-sym root-uri doc-uri expected]
  (print f"expected: { expected }" )
  (print f"got:      { (get-matches tgt-full-sym root-uri doc-uri) }")
  (assert (= (get-matches tgt-full-sym root-uri doc-uri) expected)))