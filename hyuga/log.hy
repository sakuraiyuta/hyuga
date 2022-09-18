(import logging)
(import traceback)

(setv logger (logging.getLogger "hyuga"))
(let [handler (logging.FileHandler :filename "/tmp/hyuga.log")]
  (.setLevel logger logging.DEBUG)
  (.setFormatter handler (logging.Formatter "%(levelname)-9s %(asctime)s [%(name)s] %(message)s"))
  (logger.addHandler handler))

(defn error-trace
  [logfn fname e]
  "TODO: doc"
  (logfn f"{fname}: error e={e}")
  (logfn f"------ stacktrace")
  (logfn f"{(.join "" (traceback.format_exception e))}")
  (logfn f"------ stacktrace end"))
