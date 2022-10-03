(import re)

;; TODO: get config from server params
(setv re-word-str r"[\+\.\?\-\w\!\>\<\$\/]+")

(defn cursor-line
  [$SERVER uri ln]
  "TODO: doc"
  (let [doc ($SERVER.workspace.get_document uri)
        content doc.source
        lines (content.split "\n")]
    (get lines ln)))

(defn cursor-word
  [$SERVER uri ln cn]
  "TODO: doc"
  (let [line (cursor-line $SERVER uri ln)]
    (for [m (re.finditer re-word-str line)]
      (when (and (<= (m.start) cn) (<= cn (m.end)))
        (return (cut line (m.start) cn))))))

(defn cursor-word-all
  [$SERVER uri ln cn]
  "TODO: doc"
  (let [line (cursor-line $SERVER uri ln)]
    (for [m (re.finditer re-word-str line)]
      (when (and (<= (m.start) cn) (<= cn (m.end)))
        (return (cut line (m.start) (m.end)))))))
