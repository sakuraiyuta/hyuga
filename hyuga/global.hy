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
    [self sym v scope docs
     [pos #(None None)]]
    (logger.debug
      f"add-$SYMS: sym={sym}, v={v}, scope={scope} ln={(first pos)} cn={(second pos)}")
    (let [data {"sym" sym
                "type" v
                "scope" scope
                "pos" pos
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
