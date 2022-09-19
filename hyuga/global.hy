(require hyrule * :readers *)
(require hyrule.collections [assoc])
(import hyuga.log *)

;; TODO: refactor(remove Global)
(defclass Global [object]
  (defn __init__
    [self]
    (setv self.$SYMS {}))

  (defn add-$SYMS
    [self sym v scope docs]
    (logger.debug
      (.format "add-$SYMS: sym={}, v={}, scope={}"
               sym v scope))
    (let [data {"sym" sym
                "type" v
                "scope" scope
                "docs" docs}]
      (assoc self.$SYMS sym data)
      data))

  (defn clean-$SYMS
    [self]
    (setv self.$SYMS {}))

  (defn get-$SYMS
    [self]
    self.$SYMS))

(setv $GLOBAL (Global))
