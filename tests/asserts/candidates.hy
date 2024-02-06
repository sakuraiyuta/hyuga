(require hyrule * :readers *)

(import hy.models [reduce])
(import toolz.itertoolz *)

(import .details [details])
(import hyuga.sym.helper [get-sym])

(setv
  candidates
  (->> (details.items)
       (map #%(let [sym-data (second %1)]
                (return {(-> sym-data (get "sym") get-sym)
                         sym-data})))
       (reduce #%(| %1 %2))))

(defn get-expected-candidates
  [syms]
  (->> syms
       (map #%(let+ [{sym "sym" ns "ns" scope "scope"}
                     (get candidates %1)]
                #(f"{scope}\\{ns}\\{sym}" (get candidates %1))))
       tuple))
