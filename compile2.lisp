(in-package :slm)

(defclass expr () (code))

(defun compil-const (x val? more?)
  "Compile a constant expression."
  (if val? (seq (if (member x '(t nil -1 0 1 2))
                    (gen x)
                    (gen 'CONST x))
                (unless more? (gen 'RETURN)))))

(defun compil-var (x env val? more?)
  "Compile a variable reference."
  (if val? (seq (gen-var x env) (unless more? (gen 'RETURN)))))

(defun compil-list (exps env)
  "Compile a list, leaving them all on the stack."
  (if (null exps) nil
      (seq (compil (first exps) env t t)
           (compil-list (rest exps) env))))

(defun compil-if (pred then else env val? more?)
  "Compile a conditional (IF) expression."
  (cond
    ((null pred)          ; (if nil x y) ==> y
     (compil else env val? more?))
    ((constantp pred)     ; (if t x y) ==> x
     (compil then env val? more?))
    ((and (listp pred)    ; (if (not p) x y) ==> (if p y x)
          (length=1 (rest pred))
          (primitive-p (first pred) env 1)
          (eq (prim-opcode (primitive-p (first pred) env 1)) 'not))
     (compil-if (second pred) else then env val? more?))
    (t (let ((pcode (compil pred env t t))
             (tcode (compil then env val? more?))
             (ecode (compil else env val? more?)))
         (cond
           ((equal tcode ecode) ; (if p x x) ==> (begin p x)
            (seq (compil pred env nil t) ecode))
           ((null tcode)  ; (if p nil y) ==> p (TJUMP L2) y L2:
            (let ((L2 (gen-label)))
              (seq pcode (gen 'TJUMP L2) ecode (list L2)
                   (unless more? (gen 'RETURN)))))
           ((null ecode)  ; (if p x) ==> p (FJUMP L1) x L1:
            (let ((L1 (gen-label)))
              (seq pcode (gen 'FJUMP L1) tcode (list L1)
                   (unless more? (gen 'RETURN)))))
           (t             ; (if p x y) ==> p (FJUMP L1) x L1: y
                          ; or p (FJUMP L1) x (JUMP L2) L1: y L2:
            (let ((L1 (gen-label))
                  (L2 (if more? (gen-label))))
              (seq pcode (gen 'FJUMP L1) tcode
                   (if more? (gen 'JUMP L2))
                   (list L1) ecode (if more? (list L2))))))))))

(defun compil-lambda (args body env)
  "Compile a lambda form into a closure with compiled code."
  (new-fn :env env :args args
          :code (seq (gen-args args 0)
                     (compil-begin body
                                   (cons (make-true-list args) env)
                                   t nil))))



(defun compil-funcall (f args env val? more?)
  "Compile an application of a function to arguments."
  (let ((prim (primitive-p f env (length args))))
    (cond
      (prim  ; function compilable to a primitive instruction
       (if (and (not val?) (not (prim-side-effects prim)))
           ;; Side-effect free primitive when value unused
           (compil-begin args env nil more?)
           ;; Primitive with value or call needed
           (seq (compil-list args env)
                (gen (prim-opcode prim))
                (unless val? (gen 'POP))
                (unless more? (gen 'RETURN)))))
      ((and (starts-with f 'lambda) (null (second f)))
       ;; ((lambda () body)) => (begin body)
       (assert (null args) () "Too many arguments supplied")
       (compil-begin (rest2 f) env val? more?))
      (more? ; Need to save the continuation point
       (let ((k (gen-label 'k)))
         (seq (gen 'SAVE k)
              (compil-list args env)
              (compil f env t t)
              (gen 'CALLJ (length args))
              (list k)
              (if (not val?) (gen 'POP)))))
      (t     ; function call as rename plus goto
       (seq (compil-list args env)
            (compil f env t t)
            (gen 'CALLJ (length args)))))))

(defun compil-begin (exps env val? more?)
  "Compile a sequence of expressions,
  returning the last one as the value."
  (cond ((null exps) (compil-const nil val? more?))
        ((length=1 exps) (compil (first exps) env val? more?))
        (t (seq (compil (first exps) env nil t)
                (compil-begin (rest exps) env val? more?)))))


;;====================================================

(defgeneric compil (x env val? more?)
  (:documentation "generic function for comp"))

;;; must chain them up .


;; lift a list to expr.
(defmethod compil (x env val? more?)
  (let ((expr (make-instance 'expr)))
    (setf (slot-value expr 'code) x)
  (compil expr env val? more?)))

(defmethod compil ((x expr) env val? more?)
  (macrolet ((-> (i new-class)
               `(change-class ,i ,new-class)))
    (with-slots (code) x
      (cond ((member code '(t nil))
             (-> x 'expr/const))
            ((symbolp code)
             (-> x 'expr/symbol))
            ((atom code)
             (-> x 'expr/const))
            ((scheme-macro (first code))
             (-> x 'expr/scheme-macro))
            ((case (first code)
               (QUOTE  (-> x 'expr/quote ))
               (BEGIN  (-> x 'expr/begin ))
               (SET!   (-> x 'expr/set!  ))
               (if     (-> x 'expr/if    ))
               (LAMBDA (-> x 'expr/lambda))
               (t      (-> x 'expr/funcall-alike))))))
    (compil x env val? more?))) ;; let's chain'em up!

(defmacro defcompil (specific-type-of-expr &body body)
  "define a compile case.
   :receives
     specific-type-of-expr --the more specific type of expr
     body -- the body of definition.
   :exposes
     code: the actual code in x
     env
     val?
     more?"
  `(progn
     (defclass ,specific-type-of-expr (expr) nil)
     (defmethod compil ((x ,specific-type-of-expr) env val? more?)
       (with-slots (code) x ,@body))))

(defcompil expr/const
  (compil-const code val? more?))

(defcompil expr/symbol
  (compil-var code env val? more?))

(defcompil expr/scheme-macro
  (setf code (scheme-macro-expand code))
  (call-next-method))

(defcompil expr/quote
  (check-arg-count code 1)
  (compil-const (second code) val? more?))

(defcompil expr/begin
  (compil-begin (rest code) env val? more?))

(defcompil expr/set!
  (check-arg-count code 2)
  (assert (symbolp (second code)) (code)
          "Only symbols can be set!, not ~a in ~a"
          (second code) code)
  (seq (compil (third code) env t t)
       (gen-set (second code) env)
       (if (not val?) (gen 'POP))
       (unless more? (gen 'RETURN))))

(defcompil expr/if
  (check-arg-count code 2 3)
  (compil-if (second code) (third code) (fourth code)
             env val? more?))

(defcompil expr/lambda
  (when val?
    (let ((f (compil-lambda (second code) (rest2 code) env)))
      (seq (gen 'FN f) (unless more? (gen 'RETURN))))))

(defcompil expr/funcall-alike
  (compil-funcall (first code) (rest code) env val? more?))

;;========================================



(defun check-arg-count (form min &optional (max min))
  "Report an error if form has wrong number of args."
  (let ((n-args (length (rest form))))
    (assert (<= min n-args max) (form)
      "Wrong number of arguments for ~a in ~a:
       ~d supplied, ~d~@[ to ~d~] expected"
      (first form) form n-args min (if (/= min max) max))))


;;; ==============================
;;; scheme primitives

(defstruct (prim (:type list))
  symbol n-args opcode always side-effects)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *primitive-fns*
    '((+ 2 + true) (- 2 - true) (* 2 * true) (/ 2 / true)
      (< 2 <) (> 2 >) (<= 2 <=) (>= 2 >=) (/= 2 /=) (= 2 =)
      (eq? 2 eq) (equal? 2 equal) (eqv? 2 eql)
      (not 1 not) (null? 1 not)
      (car 1 car) (cdr 1 cdr)  (cadr 1 cadr) (cons 2 cons true)
      (list 1 list1 true) (list 2 list2 true) (list 3 list3 true)
      (read 0 scheme-read nil t) (eof-object? 1 eof-object?) ;***
      (write 1 write nil t) (display 1 display nil t)
      (newline 0 newline nil t) (compiler 1 compiler t)
      (show-forward 1 show-forward t)
      (name! 2 name! true t) (random 1 random true nil))))

(defun primitive-p (f env n-args)
  "F is a primitive if it is in the table, and is not shadowed
  by something in the environment, and has the right number of args."
  (and (not (in-env-p f env))
       (find f *primitive-fns*
             :test #'(lambda (f prim)
                       (and (eq f (prim-symbol prim))
                            (= n-args (prim-n-args prim)))))))

(defun list1 (x) (list x))
(defun list2 (x y) (list x y))
(defun list3 (x y z) (list x y z))

(defun display (x) (princ x))
(defun newline () (terpri))

(defun gen-args (args n-so-far)
  "Generate an instruction to load the arguments."
  (cond ((null args) (gen 'ARGS n-so-far))
        ((symbolp args) (gen 'ARGS. n-so-far))
        ((and (consp args) (symbolp (first args)))
         (gen-args (rest args) (+ n-so-far 1)))
        (t (error "Illegal argument list"))))

(defun make-true-list (dotted-list)
  "Convert a possibly dotted list into a true, non-dotted list."
  (cond ((null dotted-list) nil)
        ((atom dotted-list) (list dotted-list))
        (t (cons (first dotted-list)
                 (make-true-list (rest dotted-list))))))

(defun new-fn (&key code env name args)
  "Build a new function."
  (assemble (make-fn :env env :name name :args args
                     :code (optimize-scm code))))

