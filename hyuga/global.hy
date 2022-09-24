(require hyrule * :readers *)
(require hyrule.collections [assoc])
(import hyuga.log *)

(import toolz.itertoolz *)

;; TODO: refactor(remove Global)
(defclass Global [object]
  (defn __init__
    [self]
    (setv self.$SYMS {}))

  (defn add-$SYMS
    [self data]
    ;; {"sym" f"{scope}\\{ns}.{sym}"
    ;;  "type" v
    ;;  "scope" scope
    ;;  "pos" pos
    ;;  "uri" uri
    ;;  "docs" docs}
    (let+ [{sym "sym" v "v" scope "scope"
            pos "pos" docs "docs" uri "uri"} data]
      (logger.debug
        f"add-$SYMS: sym={sym}, scope={scope}, pos={pos}, uri={uri}")
      (assoc self.$SYMS sym data))
    data)

  (defn clean-$SYMS
    [self]
    (setv self.$SYMS {}))

  (defn get-$SYMS
    [self]
    ;; FIXME: why suddenly added malformed symdata?(e.g. hy module)
;    (->> self.$SYMS .values
;         (filter #%(not (isinstance %1 dict)))
;         (map #%(logger.warning f"xxxxxxxxx {%1}"))
;         tuple)
    (->> self.$SYMS .keys tuple
         (filter #%(not-in "\\" %1))
         (map #%(.pop self.$SYMS %1))
         tuple)
    (->> self.$SYMS .items
         (filter #%(not (isinstance (second %1) dict)))
         (map #%(.pop self.$SYMS (first %1)))
         tuple)
    self.$SYMS))

(setv $GLOBAL (Global))
