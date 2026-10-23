(import hyrule.iterables [coll?])

(defn walk [inner outer form]
  "``walk`` traverses ``form``, an arbitrary data structure. Applies
  ``inner`` to each element of form, building up a data structure of the
  same type.  Applies ``outer`` to the result.

  Examples:
    ::

       => (import hyrule.contrib.walk [walk])
       => (setv a '(a b c d e f))
       => (walk ord (fn [x] x)  a)
       '(97 98 99 100 101 102)

    ::

       => (walk ord (fn [x] (get x 0)) a)
       97
  "
  (cond
    (isinstance form hy.models.Expression)
    (outer (hy.models.Expression (map inner form)))
    (or (isinstance form #(hy.models.Sequence list)))
    ((type form) (outer (hy.models.Expression (map inner form))))
    (coll? form)
    (walk inner outer (list form))
    True
    (outer form)))
