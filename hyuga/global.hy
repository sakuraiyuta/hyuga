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
    ;; {"sym" sym
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
    self.$SYMS))

(setv $GLOBAL (Global))
