(require hyrule * :readers *)
(import sys)
(import importlib.machinery [ModuleSpec])

(defclass DummyImporter [object]
  (defn [classmethod] find_spec
    [self fullname [path None] [target None]]
    (ModuleSpec fullname self))

  (defn [classmethod] create_module
    [self spec]
    None)

  (defn [classmethod] exec_module
    [self module]
    None))
;(-> 
;  "
;import hy
;
;def hello():
;  return 'hello __name__={}'.format(__name__)
;"
;  (exec module.__dict__)
;  )))

;(do
;  (import sys)
;  (sys.meta_path.append DummyImporter)
;  (try
;    (import xxx)
;    (print (dir xxx))
;    (print xxx.__package__)
;    (except [e BaseException]
;      (print (.format "exception: {}" e)))
;    (finally
;      (sys.meta_path.pop -1))))
;;(xxx.hello)
;;(dir xxx.hy)
;
;(.__str__ 
;  (let [code (-> "(defn hello [] (print 1))" hy.read)]
;    (hy.eval code)))
;
;(dir xxx)
;(dir)
