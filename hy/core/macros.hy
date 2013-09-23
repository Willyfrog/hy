;; Hy core macros
;;
;; Copyright (c) 2013 Nicolas Dandrimont <nicolas.dandrimont@crans.org>
;; Copyright (c) 2013 Paul Tagliamonte <paultag@debian.org>
;; Copyright (c) 2013 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(import [hy.compiler [HyTypeError]])


(defmacro error [location reason]
  "error out properly within a macro"
  `(raise (HyTypeError ~location ~reason)))


(defmacro defmacro-alias [names lambda-list &rest body]
  "define one macro with several names"
  (setv ret `(do))
  (foreach [name names]
    (.append ret
	     `(defmacro ~name ~lambda-list ~@(list body))))
  ret)


(defmacro-alias [defn defun] [name lambda-list &rest body]
  "define a function `name` with signature `lambda-list` and body `body`"
  `(setv ~name (fn ~lambda-list ~@(list body))))


(defmacro for [args &rest body]
  "shorthand for nested foreach loops:
  (for [x foo y bar] baz) ->
  (foreach [x foo]
    (foreach [y bar]
      baz))"
  ;; TODO: that signature sucks.
  ;; (for [[x foo] [y bar]] baz) would be more consistent
  (if args
    (do
     (if (% (len args) 2)
       (error args "for needs an even number of elements in its first argument")
       `(foreach [~(.pop args 0) ~(.pop args 0)] (for ~args ~@(list body)))))
    `(do ~@(list body))))


(defmacro-alias [car first] [thing]
  "Get the first element of a list/cons"
  `(get ~thing 0))


(defmacro-alias [cdr rest] [thing]
  "Get all the elements of a thing, except the first"
  `(slice ~thing 1))


(defmacro cond [&rest branches]
  "shorthand for nested ifs:
   (cond [foo bar] [baz quux]) ->
   (if foo
     bar
     (if baz
       quux))"
  (setv branches (iter branches))
  (setv branch (next branches))
  (defn check-branch [branch]
    "check `cond` branch for validity, return the corresponding `if` expr"
    (if (not (= (type branch) HyList))
      (error branch "cond branches need to be a list"))
    (if (!= (len branch) 2)
      (error branch "cond branches need two items: a test and a code branch"))
    (setv (, test thebranch) branch)
    `(if ~test ~thebranch))

  (setv root (check-branch branch))
  (setv latest-branch root)

  (foreach [branch branches]
    (setv cur-branch (check-branch branch))
    (.append latest-branch cur-branch)
    (setv latest-branch cur-branch))
  root)


(defmacro -> [head &rest rest]
  ;; TODO: fix the docstring by someone who understands this
  (setv ret head)
  (foreach [node rest]
    (if (not (isinstance node HyExpression))
      (setv node `(~node)))
    (.insert node 1 ret)
    (setv ret node))
  ret)


(defmacro ->> [head &rest rest]
  ;; TODO: fix the docstring by someone who understands this
  (setv ret head)
  (foreach [node rest]
    (if (not (isinstance node HyExpression))
      (setv node `(~node)))
    (.append node ret)
    (setv ret node))
  ret)


(defmacro let [variables &rest body]
  "Execute `body` in the lexical context of `variables`"
  (setv macroed_variables [])
  (if (not (isinstance variables HyList))
    (error variables "let lexical context must be a list"))
  (foreach [variable variables]
    (if (isinstance variable HyList)
      (do
       (if (!= (len variable) 2)
	 (error variable "let variable assignments must contain two items"))
       (.append macroed-variables `(setv ~(get variable 0) ~(get variable 1))))
      (.append macroed-variables `(setv ~variable None))))
  `((fn []
     ~@macroed-variables
     ~@(list body))))


(defmacro when [test &rest body]
  "Execute `body` when `test` is true"
  `(if ~test (do ~@(list body))))


(defmacro unless [test &rest body]
  "Execute `body` when `test` is false"
  `(if ~test None (do ~@(list body))))


(defmacro if-python2 [python2-form python3-form]
  "If running on python2, execute python2-form, else, execute python3-form"
  (import sys)
  (if (< (get sys.version_info 0) 3)
    python2-form
    python3-form))


(defmacro yield-from [_hy_yield_from_els]
  "Yield all the items from _hy_yield_from_els"
  `(for [_hy_yield_from_x ~_hy_yield_from_els]
     (yield _hy_yield_from_x)))
