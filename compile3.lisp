(in-package :slm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *primitive-fns*
    '((+ 2 + true) (- 2 - true) (* 2 * true) (/ 2 / true)
      (< 2 <) (> 2 >) (<= 2 <=) (>= 2 >=) (/= 2 /=) (= 2 =)
      (eq? 2 eq) (equal? 2 equal) (eqv? 2 eql)
      (not 1 not) (null? 1 not)
      (car 1 car) (cdr 1 cdr)  (cadr 1 cadr) (cons 2 cons true)
      (list 1 list1 true) (list 2 list2 true) (list 3 list3 true)
      (read 0 scheme-read nil t) (eof-object? 1 eof-object?)
      (write 1 write nil t) (display 1 display nil t)
      (newline 0 newline nil t) (compiler 1 compiler t)
      (show-forward 1 show-forward t)
      (name! 2 name! true t) (random 1 random true nil))))
(defun opcode (instr) (if (label-p instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))
(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

(defsetf arg1 (instr) (val) `(setf (second ,instr) ,val))

;;; ==============================

(defun assemble (fn)
  "Turn a list of instructions into a vector."
  (multiple-value-bind (length labels)
      (asm-first-pass (fn-code fn))
    (setf (fn-code fn)
          (asm-second-pass (fn-code fn)
                           length labels))
    fn))

(defun asm-first-pass (code)
  "Return the labels and the total code length."
  (let ((length 0)
        (labels nil))
    (dolist (instr code)
      (if (label-p instr)
          (push (cons instr length) labels)
          (incf length)))
    (values length labels)))

(defun asm-second-pass (code length labels)
  "Put code into code-vector, adjusting for labels."
  (let ((addr 0)
        (code-vector (make-array length)))
    (dolist (instr code)
      (unless (label-p instr)
        (if (is instr '(JUMP TJUMP FJUMP SAVE))
            (setf (arg1 instr)
                  (cdr (assoc (arg1 instr) labels))))
        (setf (aref code-vector addr) instr)
        (incf addr)))
    code-vector))

;;; ==============================

(defun show-fn (fn &optional (stream *standard-output*) (indent 2))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it,
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (not (fn-p fn))
      (format stream "~8a" fn)
      (progn
        (fresh-line)
        (dotimes (i (length (fn-code fn)))
          (let ((instr (elt (fn-code fn) i)))
            (if (label-p instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-fn arg stream (+ indent 8)))
                  (fresh-line))))))))

;;; ==============================

(defstruct ret-addr fn pc env)

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op)
      (member (opcode instr) op)
      (eq (opcode instr) op)))

(defun top (stack) (first stack))


(defparameter ins-hash-table (make-hash-table :size 1024))


(defclass stack-machine ()
  ((pc :accessor stack-machine-pc :initform 0 :initarg :pc)
   (env :accessor stack-machine-env :initform nil :initarg :env)
   (stack :accessor stack-machine-stack :initform nil :initarg :stack)
   (n-args :accessor n-args-of-cur-opcode :initform 0 :initarg :narg)
   (ins :accessor instruct :initform nil :initarg :ins)
   (global :accessor global-vars :initform (make-hash-table :test #'eq)
           :initarg :gvars)
   (fn :accessor stack-machine-fn :initform nil :initarg :fn)))

(defmacro define-machine-instruction (names &body body)
  "define a type of instruction for stack machine assembly lang."
  (let* ((names-list (if (atom names) `(,names) names))
         (vname (gensym-name 'machine))
         (rbody (replacefun
                 `((env (stack-machine-env ,vname))
                   (ins (instruct ,vname))
                   (stack (stack-machine-stack  ,vname))
                   (n-args (n-args-of-cur-opcode ,vname))
                   (global (global-vars ,vname))
                   (pc (stack-machine-pc  ,vname))
                   (f (stack-machine-fn ,vname)))
                 body)))
    (with-gensym-walker 
      `(progn
         ,@(mapcar (lambda (name)
                     (format t "; Machine instruction ~a~%" name) 
                     `(flet ((g!tmp (,vname) ,rbody))
                        (setf (gethash ',name ins-hash-table) #'g!tmp)))
                   names-list)))))

;; Here it begins.     

(define-machine-instruction LVAR 
  (push (elt (elt env (arg1 ins)) (arg2 ins)) stack))

(define-machine-instruction LSET
  (setf (elt (elt env (arg1 ins)) (arg2 ins)) (top stack)))

(define-machine-instruction GVAR
  (push (gethash (arg1 ins) global) stack))

(define-machine-instruction GSET
  (setf (gethash (arg1 ins) global) (top stack)))

(define-machine-instruction POP 
  (pop stack))

(define-machine-instruction CONST
  (push (arg1 ins) stack))

(define-machine-instruction JUMP
  (setf pc (arg1 ins)))

(define-machine-instruction FJUMP
  (if (null (pop stack)) (setf pc (arg1 ins))))

(define-machine-instruction SAVE 
  (push (make-ret-addr :pc (arg1 ins)
                       :fn f :env env)
        stack))

(define-machine-instruction RETURN 
  (setf f (ret-addr-fn (second stack))
        env (ret-addr-env (second stack))
        pc (ret-addr-pc (second stack)))
  (setf stack (cons (first stack) (rest2 stack))))

(define-machine-instruction CALLJ
  (pop env)                 ; discard the top frame
  (setf f (pop stack)
        env (fn-env f)
        pc 0
        n-args (arg1 ins)))

(define-condition error-num-of-arg (error)
  ((comment  :accessor error-num-of-arg/comment  :initarg :comment)
   (expected :accessor error-num-of-arg/expected :initarg :expected)
   (supplied :accessor error-num-of-arg/supplied :initarg :supplied))
  (:report (lambda (c s)
             (format s (error-num-of-arg/comment c);;
                     (error-num-of-arg/expected c)
                     (error-num-of-arg/supplied c)))))

(define-machine-instruction ARGS
  (when (not (= n-args (arg1 ins)))
    (error 'error-num-of-arg
           :comment "Wrong number of arguments: ~d expected, ~d supplied"
           :expected (arg1 ins)
           :supplied n-args))
  (push (make-array (arg1 ins)) env)
  (loop for i from (- n-args 1) downto 0 do
        (setf (elt (first env) i) (pop stack))))

(define-machine-instruction ARGS.  
  (when (not (>= n-args (arg1 ins)))
    (error 'error-num-of-arg
           :comment "Wrong number of arguments: ~d or more expected, ~d supplied"
           :expected (arg1 ins)
           :supplied n-args))
  (push (make-array (+ 1 (arg1 ins))) env)
  (loop repeat (- n-args (arg1 ins)) do
        (push (pop stack) (elt (first env) (arg1 ins))))
  (loop for i from (- (arg1 ins) 1) downto 0 do
        (setf (elt (first env) i) (pop stack))))

(define-machine-instruction FN    
  (push (make-fn :code (fn-code (arg1 ins))
                 :env env) stack))

(define-machine-instruction PRIM 
  (push (apply (arg1 ins)
               (loop with args = nil repeat n-args
                     do (push (pop stack) args)
                     finally (return args)))
        stack))

;; Continuation instructions:
(define-machine-instruction SET-CC 
  (setf stack (top stack)))

(define-machine-instruction CC 
  (push (make-fn
         :env (list (vector stack))
         :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
                 (LVAR 0 0) (RETURN)))
        stack))

;; Nullary operations:
(define-machine-instruction (SCHEME-READ NEWLINE) 
  (push (funcall (opcode ins)) stack))
;; Unary operations:
(define-machine-instruction
    (CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM SHOW-FORWARD)
  (push (funcall (opcode ins) (pop stack)) stack))
;; Binary operations:
(define-machine-instruction
    (+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
  (setf stack (cons (funcall (opcode ins) (second stack)
                             (first stack))
                    (rest2 stack))))
;; Ternary operations:
(define-machine-instruction LIST3
  (setf stack (cons (funcall (opcode ins) (third stack)
                             (second stack) (first stack))
                    (rest3 stack))))
;; Constants:
(define-machine-instruction (T NIL -1 0 1 2)
  (push (opcode ins) stack))
;; Other:
(define-condition system-halt (condition)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "System halted by user."))))
(define-machine-instruction HALT
  (error 'system-halt))



(defmethod machine-loop ((machine stack-machine) (f fn))
  (with-slots ((ins ins) (pc pc) (fn fn))
      machine
    (setf fn f)
    (handler-case 
        (loop
         (setf ins (elt (fn-code fn) pc))
         (incf pc)
         (funcall (gethash (opcode ins) ins-hash-table) machine))
      (system-halt (c) (print "System halt.")
        machine))))


(defmethod set-global-var ((m stack-machine) (var symbol) val)
  (setf (gethash var (global-vars m)) val))

(defmethod init-slml-comp ((m stack-machine))
  "Initialize values (including call/cc) for the Scheme compiler."
  (set-global-var m 'name! #'name!)
  (set-global-var m 'exit
    (new-fn :name 'exit :args '(val) :code '((HALT))))
  (set-global-var m 'call/cc
    (new-fn :name 'call/cc :args '(f)
            :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
		    (CALLJ 1)))) ; *** Bug fix, gat, 11/9/92
  (dolist (prim *primitive-fns*)
    (set-global-var m (prim-symbol prim)
           (new-fn :env nil :name (prim-symbol prim)
                   :code (seq (gen 'PRIM (prim-symbol prim))
                              (gen 'RETURN))))))
;;; ==============================

(defvar slml-top-level
  '(begin (define (slml)
            (newline)
            (display "=> ")
            (write ((show-forward (compiler (read)))))
            (slml))
          (slml)))



(defun slml ()
  (let ((m (make-instance 'stack-machine)))
    (init-slml-comp m)
    (machine-loop m (show-forward (compiler slml-top-level)))))

(defun slml-reboot (m)
  (machine-loop m (show-forward (compiler slml-top-level))))

(defun slml-exe (m exp)
  "Compile and execute the expression."
  (machine-loop m (show-forward (compiler `(exit ,exp)))))

;;;; Peephole Optimizer


;;; ==============================

(defun optimize-scm (code)
  "Perform peephole optimization on assembly code."
  (let ((any-change nil))
    ;; Optimize each tail
    (loop for code-tail on code do
          (setf any-change (or (optimize-1 code-tail code)
                               any-change)))
    ;; If any changes were made, call optimize again
    (if any-change
        (optimize-scm code)
        code)))

;;; ==============================

(defun optimize-1 (code all-code)
  "Perform peephole optimization on a tail of the assembly code.
  If a change is made, return true."
  ;; Data-driven by the opcode of the first instruction
  (let* ((instr (first code))
         (optimizer (get-optimizer (opcode instr))))
    (when optimizer
      (funcall optimizer instr code all-code))))

;;; ==============================

(let ((optimizers (make-hash-table :test #'eql)))

  (defun get-optimizer (opcode)
    "Get the assembly language optimizer for this opcode."
    (gethash opcode optimizers))

  (defun put-optimizer (opcode fn)
    "Store an assembly language optimizer for this opcode."
    (setf (gethash opcode optimizers) fn)))

;;; ==============================

(defun gen1 (&rest args) "Generate a single instruction" args)
(defun target (instr code) (second (member (arg1 instr) code)))
(defun next-instr (code) (find-if (complement #'label-p) code))

;;; ==============================

(defmacro def-optimizer (opcodes args &body body)
  "Define assembly language optimizers for these opcodes."
  (assert (and (listp opcodes) (listp args) (= (length args) 3)))
  `(dolist (op ',opcodes)
     (put-optimizer op #'(lambda ,args .,body))))

;;;; Now for some additions and answers to exercises:

;;; ==============================

(defvar eof "EoF")
(defun eof-object? (x) (eq x eof))
(defvar *scheme-readtable* (copy-readtable))

(defun scheme-read (&optional (stream *standard-input*))
  (let ((*readtable* *scheme-readtable*))
    (read stream nil eof)))

;;; ==============================

(set-dispatch-macro-character #\# #\t
  #'(lambda (&rest ignore) t)
  *scheme-readtable*)

(set-dispatch-macro-character #\# #\f
  #'(lambda (&rest ignore) nil)
  *scheme-readtable*)

(set-dispatch-macro-character #\# #\d
  ;; In both Common Lisp and Scheme,
  ;; #x, #o and #b are hexidecimal, octal, and binary,
  ;; e.g. #xff = #o377 = #b11111111 = 255
  ;; In Scheme only, #d255 is decimal 255.
  #'(lambda (stream &rest ignore)
      (let ((*read-base* 10)) (scheme-read stream)))
  *scheme-readtable*)

(set-macro-character #\`
  #'(lambda (s ignore) (list 'quasiquote (scheme-read s)))
  nil *scheme-readtable*)

(set-macro-character #\,
   #'(lambda (stream ignore)
       (let ((ch (read-char stream)))
         (if (char= ch #\@)
             (list 'unquote-splicing (read stream))
             (progn (unread-char ch stream)
                    (list 'unquote (read stream))))))
   nil *scheme-readtable*)




;;; ==============================

;(setf (scheme-macro 'quasiquote) 'quasi-q)

(defun quasi-q (x)
  "Expand a quasiquote form into append, list, and cons calls."
  (cond
    ((vectorp x)
     (list 'apply 'vector (quasi-q (coerce x 'list))))
    ((atom x)
     (if (constantp x) x (list 'quote x)))
    ((starts-with x 'unquote)
     (assert (and (rest x) (null (rest2 x))))
     (second x))
    ((starts-with x 'quasiquote)
     (assert (and (rest x) (null (rest2 x))))
     (quasi-q (quasi-q (second x))))
    ((starts-with (first x) 'unquote-splicing)
     (if (null (rest x))
         (second (first x))
         (list 'append (second (first x)) (quasi-q (rest x)))))
    (t (combine-quasiquote (quasi-q (car x))
                           (quasi-q (cdr x))
                           x))))

(defun combine-quasiquote (left right x)
  "Combine left and right (car and cdr), possibly re-using x."
  (cond ((and (constantp left) (constantp right))
         (if (and (eql (eval left) (first x))
                  (eql (eval right) (rest x)))
             (list 'quote x)
             (list 'quote (cons (eval left) (eval right)))))
        ((null right) (list 'list left))
        ((starts-with right 'list)
         (list* 'list left (rest right)))
        (t (list 'cons left right))))

;;; ==============================

(defun convert-numbers (x)
  "Replace symbols that look like Scheme numbers with their values."
  ;; Don't copy structure, make changes in place.
  (typecase x
    (cons   (setf (car x) (convert-numbers (car x)))
            (setf (cdr x) (convert-numbers (cdr x)))
	    x) ; *** Bug fix, gat, 11/9/92
    (symbol (or (convert-number x) x))
    (vector (dotimes (i (length x))
              (setf (aref x i) (convert-numbers (aref x i))))
	    x) ; *** Bug fix, gat, 11/9/92
    (t x)))

(defun convert-number (symbol)
  "If str looks like a complex number, return the number."
  (let* ((str (symbol-name symbol))
         (pos (position-if #'sign-p str))
         (end (- (length str) 1)))
    (when (and pos (char-equal (char str end) #\i))
      (let ((re (read-from-string str nil nil :start 0 :end pos))
            (im (read-from-string str nil nil :start pos :end end)))
        (when (and (numberp re) (numberp im))
          (complex re im))))))

(defun sign-p (char) (find char "+-"))


