(require hyrule * :readers *)
(import re [sub])
(import subprocess)
(import shutil [which])

(defn remove-uri-prefix
  [uri]
  (sub "^[a-z]+://" "" uri))

(defn get-poetry-venv
  []
  (let [result
        (subprocess.run
          ["poetry" "env" "info" "-p"]
          :stdout subprocess.PIPE)]
    (.rstrip (result.stdout.decode "utf-8"))))

(defn get-venv
  [root-uri]
  (let [root-path (remove-uri-prefix root-uri)]
    (if (which "poetry")
      f"{(get-poetry-venv)}/lib"
      f"{root-path}/.venv/lib")))
