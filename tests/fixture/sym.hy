(require hyrule * :readers *)

(import functools [reduce])
(import pytest)
(import pathlib [Path])
(import toolz.itertoolz *)

(import hyuga.api [parse-src!])
(import hyuga.global [$GLOBAL])

(defn [pytest.fixture] fixture-syms
  []
  ($GLOBAL.clean-$SYMS)
  (let [path (Path __file__)
        root-uri (-> (nth 2 path.parents)
                    (.joinpath "test_src")
                     str)
        doc-uri (-> (nth 2 path.parents)
                    (.joinpath "test_src" "root.hy")
                    str)
        src (with [f (open doc-uri "r")]
              (.join "\n" (f.readlines)))]
    (parse-src! src root-uri doc-uri)
    {:src src
     :root-uri root-uri
     :doc-uri doc-uri}))
