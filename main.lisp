#!/usr/bin/env sbcl --script

(defvar *pos* 2)
(defconstant *t* (open "tape" :if-exists :overwrite
                             :direction :io))
(defun L () 
  (decf *pos*))

(defun R () 
  (incf *pos*))

(defun G ()
  (file-position *t* *pos*)
  (read-char *t*)
 )

(defun P (c) 
  (file-position *t* *pos*)
  (write-char c *t*))

;;; P.236

(defun f (C B a)
  (cond
    ((eq (G) #\E)
      (L) (f1 C B a))
    (T
      (L) (f C B a))))

(defun f1 (C B a) 
  (cond
    ((eq (G) a)
      (print C)
      (eval C))
    ((or (eq (G) #\Space) (eq (G) #\Newline))
      (R) (f2 C B a))
    (T
      (R) (f1 C B a))))
    
(defun f2 (C B a)
  (cond
    ((eq (G) a)
      (eval C))
    ((or (eq (G) #\Space) (eq (G) #\Newline))
      (R) (eval B))
    (T
      (R) (f1 C B a))))

(defun test (C B a)
  (P #\Q)
  (f C B a))

(f '(test '(test '(P #\D) 0 #\Z) 0 #\Z) 0 #\Z)

(close *t*)

