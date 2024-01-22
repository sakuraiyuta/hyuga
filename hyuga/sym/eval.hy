(require hyrule * :readers *)

(defn eval-in!
  [form [ns "hyuga.sym.dummy"]]
  (hy.eval form
           :globals
           (-> f"(if (in \"{ns}\" (globals)) {ns}.__dict__ (globals))" hy.read hy.eval)))

