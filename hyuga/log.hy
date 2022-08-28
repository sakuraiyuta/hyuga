(import logging)

(setv logger (logging.getLogger "hyuga"))
(let [handler (logging.FileHandler :filename "/tmp/hyuga.log")]
  (.setLevel logger logging.DEBUG)
  (.setFormatter handler (logging.Formatter "%(levelname)-9s %(asctime)s [%(name)s] %(message)s"))
  (logger.addHandler handler))
