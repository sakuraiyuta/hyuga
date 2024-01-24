(import re)

;; TODO: get config from server params
(setv re-word-str r"[\+\.\?\-\w\!\>\<\$\/]+")

(defn cursor-line
  [src ln]
  "TODO: doc"
    (get (src.split "\n") ln))

(defn cursor-word
  [src ln cn]
  "TODO: doc"
  (let [line (cursor-line src ln)]
    (for [m (re.finditer re-word-str line)]
      (when (and (<= (m.start) cn) (<= cn (m.end)))
        (return (cut line (m.start) cn))))))

(defn cursor-word-all
  [src ln cn]
  "TODO: doc"
  (let [line (cursor-line src ln)]
    (for [m (re.finditer re-word-str line)]
      (when (and (<= (m.start) cn) (<= cn (m.end)))
        (return (cut line (m.start) (m.end)))))))
