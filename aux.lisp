;;;utils
(in-package :slm)
(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))
(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))
(defun rest3 (x)
  (rest (rest (rest x))))
(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))




