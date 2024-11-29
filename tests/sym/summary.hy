(import pytest)

(import hyuga.sym.summary *)
(import fixture [fixture-syms])

(defn [(pytest.mark.parametrize
         #("form" "expected")
         [#('(defn test [] True)
            {"name" "test"
             "type" "defn"
             "docs" ""
             "decorators" None
             "args" (hy.models.List)
             "pos" #(1 1)})
          #('(defn test2 [input] "docstring" True) 
            {"name" "test2"
             "type" "defn"
             "docs" "docstring"
             "decorators" None
             "args" (hy.models.List [(hy.models.Symbol "input")])
             "pos" #(1 1)})])]
  test_get-defn-summary
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-defn-summary form) }")
  (assert (= (get-defn-summary form) expected)))

(defn [(pytest.mark.parametrize 
         #("forms" "ret" "expected")
         [#('((defn test [] True)) {"methods" []} 
           (hy.models.Expression 
             [(hy.models.Expression  
               [(hy.models.Symbol "defn")
                (hy.models.Symbol "test")
                (hy.models.List)
                (hy.models.Symbol "True")])]))])]
  test_get-defclass-methods
  [forms ret expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-defclass-methods forms ret) }")
  (assert (= (get-defclass-methods forms ret) expected)))

(defn [(pytest.mark.parametrize 
         #("form" "expected")
         [#('(defclass Test [] (defn test [] True)) 
            {"name" "Test"
             "type" "defclass"
             "docs" ""
             "inherits" (hy.models.List)
             "methods" [{"name" "test" 
                         "type" "defn" 
                         "docs" "" 
                         "decorators" None 
                         "args" (hy.models.List) 
                         "pos" #(1 1)}]
            "pos" #(1 1)})])]
  test_get-defclass-summary
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-defclass-summary form) }")
  (assert (= (get-defclass-summary form) expected)))

(defn [(pytest.mark.parametrize 
         #("form" "expected")
         [#('(setv test True) 
            {"name" "test"
             "type" "setv"
             "value" (hy.models.Symbol "True")
             "docs" "True"
             "pos" #(1 1)})])]
  test_get-setv-summary
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-setv-summary form) }")
  (assert (= (get-setv-summary form) expected)))

(defn [(pytest.mark.parametrize 
         #("form" "expected")
         [#('(import hy) 
            {"name" "hy"
             "type" "import"
             "pos" #(1 1)
             "includes" None})
          #('(import hyrule.collections [walk])
            {"name" "hyrule.collections"
             "type" "import"
             "pos" #(1 1)
             "includes" ["walk"]})])]
  test_get-import-summary 
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-import-summary form) }")
  (assert (= (get-import-summary form) expected)))

(defn [(pytest.mark.parametrize 
         #("form" "expected")
         [#('(require hyrule.argmove [-> ->>]) 
            {"name" "hyrule.argmove"
             "type" "require"
             "pos" #(1 1)
             "includes" []})])]
  test_get-require-summary 
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-require-summary form) }")
  (assert (= (get-require-summary form) expected)))

(defn [(pytest.mark.parametrize
         #("form" "expected")
         [#('(defmacro test [] '(+ 1 1)) 
            {"name" "test"
             "type" "defmacro"
             "pos" #(1 1)
             "includes" []})
          #('(defmacro test2 [a] '(+ 1 a))
            {"name" "test2"
             "type" "defmacro"
             "pos" #(1 1)
             "includes" []})])]
  test_get-defmacro-summary 
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-defmacro-summary form) }")
  (assert (= (get-defmacro-summary form) expected)))

(defn [(pytest.mark.parametrize
         #("form" "expected")
         [#('(defaweirdthing test (+ 1 1)) 
            {"name" "test"
             "type" "defaweirdthing"
             "pos" #(1 1)})
          #('(defaweirdthing test2 1000) 
            {"name" "test2"
             "type" "defaweirdthing"
             "pos" #(1 1)})])]
  test_get-unknown-def-summary 
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-unknown-def-summary form) }")
  (assert (= (get-unknown-def-summary form) expected)))

(defn [(pytest.mark.parametrize
         #("form" "expected")
         [#('(defaweirdthing test (+ 1 1)) 
            {"name" "test"
             "type" "defaweirdthing"
             "pos" #(1 1)})
          #('(defaweirdthing test2 1000) 
            {"name" "test2"
             "type" "defaweirdthing"
             "pos" #(1 1)})
          #('(defmacro test [] '(+ 1 1)) 
            {"name" "test"
             "type" "defmacro"
             "pos" #(1 1)
             "includes" []})
          #('(defmacro test2 [a] '(+ 1 a))
            {"name" "test2"
             "type" "defmacro"
             "pos" #(1 1)
             "includes" []})
          #('(require hyrule.argmove [-> ->>]) 
            {"name" "hyrule.argmove"
             "type" "require"
             "pos" #(1 1)
             "includes" []})
          #('(import hy) 
            {"name" "hy"
             "type" "import"
             "pos" #(1 1)
             "includes" None})
          #('(import hyrule.collections [walk])
            {"name" "hyrule.collections"
             "type" "import"
             "pos" #(1 1)
             "includes" ["walk"]})
          #('(setv test True) 
            {"name" "test"
             "type" "setv"
             "value" (hy.models.Symbol "True")
             "docs" "True"
             "pos" #(1 1)})
          #('(defclass Test [] (defn test [] True)) 
            {"name" "Test"
             "type" "defclass"
             "docs" ""
             "inherits" (hy.models.List)
             "methods" [{"name" "test" 
                         "type" "defn" 
                         "docs" "" 
                         "decorators" None 
                         "args" (hy.models.List) 
                         "pos" #(1 1)}]
            "pos" #(1 1)})
          #('(defn test [] True)
            {"name" "test"
             "type" "defn"
             "docs" ""
             "decorators" None
             "args" (hy.models.List)
             "pos" #(1 1)})
          #('(defn test2 [input] "docstring" True) 
            {"name" "test2"
             "type" "defn"
             "docs" "docstring"
             "decorators" None
             "args" (hy.models.List [(hy.models.Symbol "input")])
             "pos" #(1 1)})])]
  test_get-form-summary 
  [form expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (get-form-summary form) }")
  (assert (= (get-form-summary form) expected)))

