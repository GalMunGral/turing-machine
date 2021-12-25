#!/usr/bin/env sbcl --script

(set-macro-character #\^ (lambda (stream char)
  (declare (ignore char))
  (list 'lambda (list) (read stream t nil t))))

(defvar *pos* 2)
(defconstant *t* (open "tape" :if-exists :overwrite
                             :direction :io))
(defun G ()
  (file-position *t* *pos*)
  (read-char *t*)
 )

(defun P (c) 
  (file-position *t* *pos*)
  (write-char c *t*))

(defun L () 
  (decf *pos*))

(defun R ()
  (incf *pos*))

(defun any (c)
  (and
    (not (eq c #\Space))
    (not (eq c #\Newline))))
  

;;;; P.236

;;; Find leftmost `a`

(defun f (C B a)
  (cond
    ((eq (G) #\E)
      (L) (f1 C B a))
    (T
      (L) (f C B a))))

(defun f1 (C B a) 
  (cond
    ((eq (G) a)
      (funcall C))
    ((any (G))
      (R) (f1 C B a))
    (T
      (R) (f2 C B a))))
    
(defun f2 (C B a)
  (cond
    ((eq (G) a)
      (funcall C))
    ((any (G))
      (R) (f1 C B a))
    (T
      (R) (funcall B))))

;;;; P.237

;;; Erase leftmost `a`

(defun e (C B a)
  (f ^(e1 C B a) B a))

(defun e1 (C B a)
  (P #\Space) (funcall C))

;;; Erase all `a`

(defun e* (B a)
  (e ^(e* B a) B a))

;;; Print `b` at the end

(defun pe (C b)
  (f ^(pe1 C b) C #\E))

(defun pe1 (C b)
  (cond
    ((any (G))
      (R) (R) (pe1 C b))
    (T
      (P b) (funcall C))))

(defun l* (C)
  (L) (funcall C))

(defun r* (C)
  (R) (funcall C))

;;; Find leftmost `a` then move left

(defun f* (C B a)
  (f ^(l* C) B a))

;;; Find leftmost `a` then move right

(defun f** (C B a)
  (f ^(r* C) B a))

;;; Write the first symbol marked with `a` to the end

(defun c (C B a)
  (f* ^(c1 C) B a))

(defun c1 (C)
  (let ((b (G)))
    (if b (pe C b))))

;;; Copy the first symbol marked with `a` to the end and erase the mark

(defun ce (C B a)
  (c ^(e C B a) B a))

;;; Copy all symbols marked with `a` to the end and erase the marks

(defun ce* (B a)
  (ce ^(ce* B a) B a))

(ce* ^1 #\a)

(close *t*)

