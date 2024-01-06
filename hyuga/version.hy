(import pkg-resources)

(defn get-version
  []
  (. (pkg-resources.get-distribution "hyuga") version))
