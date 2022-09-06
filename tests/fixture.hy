(import pytest)

(import hyuga.inspect [eval-define!])

(setv SRC "(import toolz.itertoolz *)")

(defn [pytest.fixture]
  fixture-syms
  []
  (eval-define! SRC))
