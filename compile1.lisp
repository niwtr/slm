(in-package :slm)


;;定义一个fn类型，包括几个槽code, env, name, args
;;这里设置了print-function为print-fn来屏蔽输出。
;;machine接受的对象就是fn。
(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

;;TODO
(defvar *label-num* 0)

;;编译一个表达式块，像是把它封装在一个无参数的lambda里面。

;;FIXME i changed comp-lambda to compil-lambda.
(defun compiler (x)
  "Compile an expression as if it were in a parameterless lambda."
  (setf *label-num* 0) ;TODO
  (compil-lambda '() (list x) nil)) ;;compil-lambda's in compile2.lisp.

(defun comp-show (x)
  "Compile an expression and show the resulting code"
   (show-fn (compiler x))
   (values))
(defun show-forward (x)
  (show-fn x) x)

;;; ==============================


(defun gen (opcode &rest args)
  "Return a one-element list of the specified instruction."
  (list (cons opcode args)))

(defun seq (&rest code)
  "Return a sequence of instructions"
  (apply #'append code))

(defun gen-label (&optional (label 'L))
  "Generate a label (a symbol of the form Lnnn)"
  (intern (format nil "~a~d" label (incf *label-num*))))

;;; ==============================

(defun gen-var (var env)
  "Generate an instruction to reference a variable's value."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LVAR (first p) (second p) ";" var)
        (gen 'GVAR var))))
(defun gen-set (var env)
  "Generate an instruction to set a variable to top-of-stack."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LSET (first p) (second p) ";" var)
        (if (assoc var *primitive-fns*)
            (error "Can't alter the constant ~a" var)
            (gen 'GSET var)))))
;;; ==============================

(def-scheme-macro define (name &rest body)
  (if (atom name)
      `(name! (set! ,name . ,body) ',name)
      (scheme-macro-expand
         `(define ,(first name)
            (lambda ,(rest name) . ,body)))))


;;this one is registered as a global var, see init-schema-comp
(defun name! (fn name)
  "Set the name field of fn, if it is an un-named fn."
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)


(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (fn-name fn) '??)))

(defun label-p (x) "Is x a label?" (atom x))

(defun in-env-p (symbol env)
  "If symbol is in the environment, return its index numbers."
  (let ((frame (find symbol env :test #'find)))
    (if frame (list (position frame env) (position symbol frame)))))
