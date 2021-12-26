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

(defun f (C R a)
  (cond
    ((eq (G) #\E)
      (L)
      (f1 C R a))
    (T
      (L)
      (f C R a))))

(defun f1 (C R a) 
  (cond
    ((eq (G) a)
      (funcall C))
    ((any (G))
      (R)
      (f1 C R a))
    (T
      (R)
      (f2 C R a))))
    
(defun f2 (C R a)
  (cond
    ((eq (G) a)
      (funcall C))
    ((any (G))
      (R)
      (f1 C R a))
    (T
      (R)
      (funcall R))))

;;;; P.237

;;; Erase leftmost `a`

(defun e (C R a)
  (f ^(e1 C R a) R a))

(defun e1 (C R a)
  (P #\Space)
  (funcall C))

;;; Erase all `a`

(defun e* (R a) (e ^(e* R a) R a))

;;; Print `b` at the end

(defun pe (C b)
  (f ^(pe1 C b) C #\E))

(defun pe1 (C b)
  (cond
    ((any (G))
      (R) (R)
      (pe1 C b))
    (T
      (P b)
      (funcall C))))

(defun l* (C)
  (L)
  (funcall C))

(defun r* (C)
  (R)
  (funcall C))

;;; Find leftmost `a` then move left

(defun f* (C R a)
  (f ^(l* C) R a))

;;; Find leftmost `a` then move right

(defun f** (C R a)
  (f ^(r* C) R a))

;;; Write the first symbol marked with `a` to the end

(defun c (C R a)
  (f* ^(c1 C) R a))

(defun c1 (C)
  (let ((b (G)))
    (if b (pe C b))))

;;;; P.238

;;; Copy the first symbol marked with `a` to the end and erase the mark

(defun ce (C R a)
  (c ^(e C R a) R a))

;;; Copy all symbols marked with `a` to the end and erase the marks

(defun ce* (R a)
  (ce ^(ce* R a) R a))

;;; Replace the first `a` by `b`

(defun re (C R a b)
  (f ^(re1 C R a b) R a))

(defun re1 (C R a b)
  (P #\Space) (P b)
  (funcall C))

;;; Replace all `a` by `b`

(defun re* (R a b)
  (re ^(re* R a b) R a b))

(defun cr (C R a)
  (c ^(re C R a #\a) R a))

;;; Copy all symbols marked with `a` to the end

(defun cr* (R a)
  (cr ^(cr* R a) ^(re* R #\a a) a))

;;; Compare the first symbol marked with `a`
;;; with the first marked with `b`

(defun cp (C R E a b)
  (f* ^(cp1 C R b) ^(f R E b) a))

(defun cp1 (C R b)
  (let ((g (G)))
    (f* ^(cp2 C R g) R b)))

(defun cp2 (C R g)
  (cond
    ((eq (G) g)
      (funcall C))
    (t
      (funcall R))))

(defun cpe (C R E a b)
  (cp ^(e ^(e C C b) C a) R E a b))

;;; Compare sequence `a` with sequence `b`
;;; Matched symbols at the beginning are are erased

(defun cpe* (R E a b)
  (cpe ^(cpe* R E a b) R E a b))

;;;; P.239

(defun q (C)
  (cond
    ((any (G))
      (R)
      (q C))
    (t
      (R)
      (q1 C))))

(defun q1 (C)
  (cond
    ((any (G))
      (R) (q C))
    (t ; two consecutive empty slot -> end reached
      (funcall C)))) 

;;; Find the last `a`

(defun q* (C a)
  (q ^(q1* C a)))

(defun q1* (C a)
  (cond
    ((eq (G) a)
      (funcall C))
    (t
      (L)
      (q1* C a))))

;;; Print multiple symbols at the end

(defun pe2 (C a b)
  (pe ^(pe C b) a))

;;; Copy multiple sequences to the end

(defun ce2* (R a b)
  (ce* ^(ce* R b) a))

(defun ce3* (R a b c)
  (ce* ^(ce2* R b c) a))

;;; Clear all marks

(defun e* (C)
  (cond
    ((eq (G) #\E)
      (R)
      (e1* C))
    (t
      (L)
      (e* C))))

(defun e1* (C)
  (cond
    ((any (G))
      (R) (P #\Space) (R)
      (e1* C))
    (t
      (funcall C))))

;;;; P. 244 The Universal Machine


(close *t*)

