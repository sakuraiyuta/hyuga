(require hyrule * :readers *)
(import functools [reduce])
(import pytest)
(import os)
(import hyuga.api [parse-src!])
(import hyuga.global [$GLOBAL])

(defn [pytest.fixture] fixture-syms
  []
  ($GLOBAL.clean-$SYMS)
  (let [root-uri (-> __file__
                     os.path.dirname)
        doc-uri (-> root-uri
                    (os.path.join "sample_src" "root.hy"))
        src (with [f (open doc-uri "r")]
              (.join "\n" (f.readlines)))]
    (parse-src! src root-uri doc-uri)
    {:src src
     :root-uri root-uri
     :doc-uri doc-uri}))
