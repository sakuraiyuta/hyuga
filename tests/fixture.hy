(import pytest)

(setv SRC "
      (import toolz.itertoolz *)
          ")

(defn [pytest.fixture]
  fixture-syms
  []
  ;(eval-define! SRC)
  )
