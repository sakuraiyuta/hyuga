(import re [sub])

(defn remove-uri-prefix
  [uri]
  (sub "^[a-z]+://" "" uri))
