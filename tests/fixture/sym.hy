(require hyrule * :readers *)

(import functools [reduce])
(import pytest)
(import pathlib [Path])
(import toolz.itertoolz *)

(import hyuga.api [parse-src!])
(import hyuga.global [$GLOBAL])
(import ..asserts.path *)

(defn [pytest.fixture] fixture-syms
  []
  ($GLOBAL.clean-$SYMS)
  (let [root-uri (str test-src-root-path)
        doc-uri (str test-src-doc-path)
        src (with [f (open doc-uri "r")]
              (.join "" (f.readlines)))]
    (parse-src! src root-uri doc-uri)
    {:src src
     :root-uri root-uri
     :doc-uri doc-uri}))
