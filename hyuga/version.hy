(import toml)

(defn get-version
  []
  (let [pyprj (toml.load "pyproject.toml")]
    (get pyprj "tool" "poetry" "version")))
