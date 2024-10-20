(import pytest)

(import hyuga.api [parse-src!])

(setv SRC "
      (import toolz.itertoolz *)
          ")

(defn [pytest.fixture]
  fixture-syms
  []
  (parse-src! SRC "." "."))
