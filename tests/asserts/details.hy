(require hyrule * :readers *)

(import builtins)
(import textwrap [dedent])
(import sys)
(import shutil)
(import iniconfig)
(import importlib.machinery [PathFinder])

(import .path *)
(import hyuga.sym.helper [sym-hy->py])

(setv
  details
  ;-------------
  {"defn"
  {"sym"  "defn"
  "ns"    "(hykwd)"
  "pos"   None
  "scope" "(hykwd)"
  "type"  (get builtins._hy_macros "defn")
  "uri"   False
  "docs"
  (-> f"
  defn [(hykwd)]
  \t{(get builtins._hy_macros "defn")}

  No docs.
  "
  dedent .strip)
  }

  ;-------------
  "defn/a"
  {"sym"  "defn/a"
  "ns"    "(hykwd)"
  "pos"   None
  "scope" "(hykwd)"
  "type"  (get builtins._hy_macros (sym-hy->py "defn/a"))
  "uri"   False
  "docs"
  (-> f"
  defn/a [(hykwd)]
  \t{(get builtins._hy_macros (sym-hy->py "defn/a"))}

  No docs.
  "
  dedent .strip)
  }

  ;-------------
  "str-sample"
  {"sym"  "str-sample"
  "ns"    "doc"
  "pos"   #(8 7)
  "scope" "doc"
  "type"  {"docs" "\"sample value\""
           "name" "str-sample"
           "pos" #(8 7)
           "type" "setv"
            "value" (hy.models.String "sample value")}
  "uri"   (str test-src-doc-path)
  "docs"  (-> f"
  setv str-sample
  \t[doc]
  
  `\"sample value\"`
  "
  dedent .strip)}

  ;-------------
  "int-sample"
  {"sym"  "int-sample"
  "ns"    "doc"
  "pos"   #(9 7)
  "scope" "doc"
  "type"  {"docs" "123"
           "name" "int-sample"
           "pos" #(9 7)
           "type" "setv"
            "value" (hy.models.Integer 123)}
  "uri"   (str test-src-doc-path)
  "docs"  (-> f"
  setv int-sample
  \t[doc]
  
  `123`
  "
  dedent .strip)}

  ;-------------
  "dict-sample"
  {"sym"  "dict-sample"
  "ns"    "doc"
  "pos"   #(10 7)
  "scope" "doc"
  "type"  {"docs" "{:key 12345}"
           "name" "dict-sample"
           "pos" #(10 7)
           "type" "setv"
            "value" (hy.models.Dict [(hy.models.Keyword "key")
                                     (hy.models.Integer 12345)])}
  "uri"   (str test-src-doc-path)
  "docs"  (-> "
  setv dict-sample
  \t[doc]
  
  `{:key 12345}`
  "
  dedent .strip)}

  ;-------------
  "vars"
  {"sym"  "vars"
  "ns"    "(builtin)"
  "pos"   None
  "scope" "(builtin)"
  "type"  (get (vars builtins) "vars")
  "uri"   False
  "docs"  (-> f"vars [(builtin)]\n\t{(get (vars builtins) "vars")}\n\n{(vars.__doc__.strip)}" .strip)}

  ;-------------
  "shutil"
  {"sym"  "shutil"
  "ns"    "shutil"
  "pos"   #(1 1)
  "scope" "(sysenv)"
  "type"  (get sys.modules "shutil")
  "uri"   (-> (get sys.modules "shutil") (getattr "__file__"))
  "docs"  (-> f"
  shutil [shutil]
  \t{(get sys.modules "shutil")}

  Utility functions for copying and archiving files and directory trees.

  XXX The functions here don't copy the resource fork or other metadata on Mac.
  "
  dedent .strip)}

  ;-------------
  "iniconfig"
  {"sym"  "iniconfig"
  "ns"    "iniconfig"
  "pos"   #(1 1)
  "scope" "(venv)"
  "type"  (PathFinder.find_spec "iniconfig")
  "uri"   (-> (get sys.modules "iniconfig") (getattr "__file__"))
  "docs"  (-> f"
  iniconfig [iniconfig]
  \t{(PathFinder.find_spec "iniconfig")}

  brain-dead simple parser for ini-style files.
  (C) Ronny Pfannschmidt, Holger Krekel -- MIT licensed
  "
  dedent .strip)}

  ;-------------
  "fn-sample"
  {"sym"  "fn-sample"
  "ns"    "doc"
  "pos"   #(4 7)
  "scope" "doc"
  "type"  {"args" (hy.models.List)
           "argtypes" (hy.models.List)
           "decorators" (hy.models.List)
           "docs" ""
           "name" "fn-sample"
           "pos" #(4 7)
           "type" "defn"}
  "uri"   (str test-src-doc-path)
  "docs"  (-> f"
  <doc>

  defn [] :tp [] fn-sample
  \t[]
  "
  dedent .strip (+ "\n\n"))}
  })
