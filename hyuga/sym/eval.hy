(require hyrule * :readers *)

(defn eval-in!
  [form [ns "hyuga.sym.dummy"]]
  (let [result
        (hy.eval form
                 :locals
                 (-> f"(if (in \"{ns}\" (globals)) {ns}.__dict__ (globals))" hy.read hy.eval))]
    result))

