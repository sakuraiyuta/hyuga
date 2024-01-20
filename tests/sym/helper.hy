(require hyrule * :readers *)
(import hyrule.hypprint [pp])

(import pytest)

(import hyuga.sym.helper *)

(defn [(pytest.mark.parametrize
         #("root_uri" "doc_uri" "expected")
         [#(
            "file:///home/yuta/git/hyuga"
            "file:///home/yuta/git/hyuga/hyuga/sym/spec.hy"
            "hyuga.sym.spec"
            )
          #(
            "file:///dummy"
            "file:///home/yuta/git/hyuga/hyuga/sym/spec.hy"
            "file:...home.yuta.git.hyuga.hyuga.sym.spec"
            )])]
  test_uri__mod
  [root_uri doc_uri expected]
  (let [result (uri->mod root_uri doc_uri)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("form" "expected")
         [#('(a) "(a)")
          #('a "a")
          #('(com (ple) x) "(com (ple) x)")])]
  test_fix_hy_symbol
  [form expected]
  (let [result (fix-hy-symbol form)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("symkey" "expected")
         [#("a\\b\\c" "b")])]
  test_get_ns
  [symkey expected]
  (let [result (get-ns symkey)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("symkey" "expected")
         [#("a\\b\\c" "c")])]
  test_get_sym
  [symkey expected]
  (let [result (get-sym symkey)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("symkey" "expected")
         [#("a\\b\\c" "a")])]
  test_get_scope
  [symkey expected]
  (let [result (get-scope symkey)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("symkey" "expected")
         [#("a\\b\\c" ["a" "b" "c"])])]
  test_get_scope_ns_sym
  [symkey expected]
  (let [result (get-scope/ns/sym symkey)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("scope" "ns" "sym" "expected")
         [#("a" "b" "c" "a\\b\\c")])]
  test_get_full_sym
  [scope ns sym expected]
  (let [result (get-full-sym scope ns sym)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#(["a_b" None] ["a-b" None])
          #(["hyx_aXsolidusXb" None] ["a/b" None])])]
  test_sym_py_val_sym_hy_val
  [val expected]
  (let [result (sym-py/val->sym-hy/val val)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("a_b" "a-b")
          #("a>b" "a>b")
          #("a.__doc__" "a.__doc__")])]
  test_sym_py_hy
  [val expected]
  (let [result (sym-py->hy val)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("a-b" "a_b")
          #("a>b" "hyx_aXgreaterHthan_signXb")
          #("a.__doc__" "a.__doc__")])]
  test_sym_hy_py
  [val expected]
  (let [result (sym-hy->py val)]
    (assert (= result expected))))

(defn [(pytest.mark.parametrize
         #("sym" "ns" "expected")
         [#("mesa.test" "" "mesa")
          #("mesa" "" "")
          #(".x" "" "")
          #(".x" "mesa" "mesa")
          #("hyuga.sym.helper" "" "hyuga.sym")])]
  test_module_or_class
  [sym ns expected]
  (let [result (module-or-class? sym ns)]
    (assert (= result expected))))

;; TODO: implement
;(defn [(pytest.mark.parametrize
;         #("sym_hy" "expected")
;         [#()])]
;  test_get_module_in_syms
;  [sym_hy expected]
;  (let [result (get-module-in-syms sym_hy)]
;    (assert (= result expected))))

;; TODO: implement
;(defn [(pytest.mark.parametrize
;         #("module_name" "expected")
;         [#()])]
;  test_get_module-attrs
;  [module_name expected]
;  (let [result (get-module-in-syms module_name)]
;    (assert (= result expected))))

;; TODO: implement
;(defn [(pytest.mark.parametrize
;         #("expected")
;         [#("mesa")])]
;  test_get_hy_macros
;  [expected]
;  (let [result (get-hy-macros)]
;    (pp result)
;    (assert (= result expected))))
