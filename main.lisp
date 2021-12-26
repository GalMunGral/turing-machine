#!/usr/bin/env sbcl --script

(set-macro-character #\^ (lambda (stream char)
  (declare (ignore char))
  (list 'lambda (list) (read stream t nil t))))

(defvar *pos* 2)
(defconstant *t* (open "tape" :if-exists :overwrite
                             :direction :io))
(defun S ()
  (file-position *t* *pos*)
  (read-char *t* nil #\Space)
 )

(defun P (c) 
  (file-position *t* *pos*)
  (write-char c *t*))

(defun L () 
  ; (print "LEFT")
  (decf *pos*))

(defun R ()
  ; (print "RIGHT")
  (incf *pos*))

(defun any (c)
  (not (eq c #\Space)))
  

;;;; P.236

;;; Find leftmost `a`

(defun f (C R a)
  (cond
    ((eq (S) #\E)
      (L)
      (f1 C R a))
    (T
      (L)
      (f C R a))))

(defun f1 (C R a) 
  (cond
    ((eq (S) a)
      (funcall C))
    ((any (S))
      (R)
      (f1 C R a))
    (T
      (R)
      (f2 C R a))))
    
(defun f2 (C R a)
  (cond
    ((eq (S) a)
      (funcall C))
    ((any (S))
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

(defun e* (R a)
  (e ^(e* R a) R a))

;;; Print `b` at the end

(defun pe (C b)
  (f ^(pe1 C b) C #\E))

(defun pe1 (C b)
  (cond
    ((any (S))
      (R) (P (S)) (R)
      (pe1 C b))
    (t
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
  (let ((b (S)))
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
  (let ((g (S)))
    (f* ^(cp2 C R g) R b)))

(defun cp2 (C R g)
  (cond
    ((eq (S) g)
      (funcall C))
    (t
      (funcall R))))

(defun cpe (C R E a b)
  (cp ^(e ^(e C C b) C a) R E a b))

;;; Compare sequence `a` with sequence `b`
;;; Matched symbols at the beginning are are erased

(defun cpe* (R C a b)
  (cpe ^(cpe* R C a b) R C a b))

;;;; P.239

(defun _g (C)
  (cond
    ((any (S))
      (R)
      (_g C))
    (t
      (R)
      (_g1 C))))

(defun _g1 (C)
  (cond
    ((any (S))
      (R) (_g C))
    (t ; two consecutive empty slot -> end reached
      (funcall C)))) 

;;; Find the last `a`

(defun g (C a)
  (_g ^(g1 C a)))

(defun g1 (C a)
  (cond
    ((eq (S) a)
      (funcall C))
    (t
      (L)
      (g1 C a))))

;;; Print multiple symbols at the end

(defun pe2 (C a b)
  (pe ^(pe C b) a))

;;; Copy multiple sequences to the end

(defun ce2* (R a b)
  (ce* ^(ce* R b) a))

(defun ce3* (R a b c)
  (ce* ^(ce2* R b c) a))

(defun ce4* (R a b c d)
  (ce* ^(ce3* R b c d) a))

(defun ce5* (R a b c d e)
  (ce* ^(ce4* R b c d e) a))

;;; (Clear the marks on multiple sequences)

(defun e2* (R a b)
  (e* ^(e* R b) a))

;;; Clear all marks

(defun e** (C)
  (cond
    ((eq (S) #\E)
      (R)
      (e1** C))
    (t
      (L)
      (e** C))))

(defun e1** (C)
  (cond
    ((any (S))
      (R) (P #\Space) (R)
      (e1** C))
    (t
      (funcall C))))

;;;; P. 244 

(defun con (C a)
  (cond
    ((eq (S) #\A)
      (L) (P a) (R) ; Mark symbol 'D'
      (con1 C a))
    (t
      (R) (R)
      (con C a))))

(defun con1 (C a)
  (cond
    ((eq (S) #\A)
      (R) (P a) (R)
      (con1 C a))
    ((eq (S) #\D)
      (R) (P a) (R) ; 'DA..AAA' -> 'DC..CCC'
      (con2 C a))))

(defun con2 (C a)
  (cond
    ((eq (S) #\C)
      (R) (P a) (R)
      (con2 C a))
    (t
      (R) (R) ; four squares to the right of last 'C'
      (funcall C))))

;;;; THE UNIVERSAL MACHINE

(defun b ()
  (f ^(b1) ^(b1) #\.))

;;; Set up initial state

(defun b1 ()
  (R) (P #\Space) (R) (P #\:)
  (R) (P #\Space) (R) (P #\D)
  (R) (P #\Space) (R) (P #\A)
  (R) (P #\Space) (R) (P #\D) ; Let's see if this works...
  (anf))

;;; Mark current state with 'y'

(defun anf ()
  (g ^(anf1) #\:))

(defun anf1 ()
  (con ^(fom) #\y))

;;; Find matching instruction

(defun fom ()
  (cond
    ((eq (S) #\;) ; Next instruction (in reverse order)
      (R) (P #\z) (L)
      (con ^(fmp) #\x)) ; Mark instruction with 'x'
    ((eq (S) #\z)
      (L) (L)
      (fom))
    ((eq (S) #\E)
      ;; unexpected configuration
      (print "NO MATCHING INSTR"))
    (t
      (L)
      (fom))))

;;; There seems to be a mistake here:
;;; On error the machine should jump to 'anf' state
;;; rather than 'fom' as 'y' marks have been erased

(defun fmp ()
  (cpe* ^(e2* ^(anf) #\x #\y) ^(sim) #\x #\y))

;;; Simulate (execute the instruction)

(defun sim ()
  (f* ^(sim1) ^(sim1) #\z))

(defun sim1 ()
  (con ^(sim2) #\Space)) ; Ignore

;;; Mark out the "DC..C" part

(defun sim2 ()
  (cond
    ((eq (S) #\A)
      (sim3)) ; DC..C(L|R|N) -> DA..A
    (t
      ;; There is a mistake in the paper
      ;; The sequence should be LPRRR rather than RPRRR
      (L) (P #\u) (R) (R) (R)
      (sim2))))

(defun sim3 ()
  (cond
    ((not (eq (S) #\A))
      (L) (P #\y)
      (e* ^(mf) #\z)) ; End of instruction
    (t
      (L) (P #\y) (R) (R) (R)
      (sim3))))

(defun mf ()
  (g ^(mf1) #\:)) ; A typo in the paper, 'mf' should be 'mf1'

(defun mf1 ()
  ;; Find "DA..A"
  (cond
    ((not (eq (S) #\A))
      (R) (R)
      (mf1))
    (t
      (L) (L) (L) (L)
      (mf2))))

(defun mf2 ()
  ;; Mark out previous symbol (DC..C)
  (cond
    ((eq (S) #\:)
      (mf4))
    ((eq (S) #\C)
      (R) (P #\x) (L) (L) (L)
      (mf2))
    ((eq (S) #\D)
      (R) (P #\x) (L) (L) (L)
      (mf3))))

(defun mf3 ()
  ;; Mark out all symbols at the beginning
  (cond
    ((eq (S) #\:)
      (mf4))
    (t
      (R) (P #\v) (L) (L) (L)
      (mf3))))

(defun mf4 ()
  ;; Move to the immediate next symbol
  (con ^(l* ^(l* ^(mf5))) #\Space))

(defun mf5 ()
  ;; Mark out symbols at the end
  (cond
    ((any (S))
      (R) (P #\w) (R)
      (mf5))
    (t
      (P #\:)
      (sh))))

(defun sh ()
  (f ^(sh1) ^(inst) #\u))

(defun sh1 ()
  (L) ; LLL in the paper ??
  (sh2))

(defun sh2 ()
  (cond
    ((eq (S) #\D)
      (R) (R) ; RRRR in the paper ??
      (sh3)) ; sh2 in the paper ??
    (t
      (inst))))

(defun sh3 ()
  (cond
    ((eq (S) #\C) ; DC..
      (R) (R)
      (sh4))
    (t
      (inst))))

(defun sh4 ()
  (cond
    ((eq (S) #\C) ; DCC..
      (R) (R)
      (sh5))
    (t ; DC
      (pe2 ^(inst) #\0 #\:))))

(defun sh5 ()
  (cond
    ((eq (S) #\C)
      (inst))
    (t ; DCC
      (pe2 ^(inst) #\1 #\:))))


(defun inst ()
  ;; Last symbol marked 'u' -> L|R|N
  (g ^(l* ^(inst1)) #\u))

(defun inst1 ()
  (let ((a (S)))
    (R) (P #\Space)
    (inst1* a)))

(defun inst1* (a)
  (case a
    (#\L (ce5* ^(ov) #\v #\y #\x #\u #\w))
    (#\R (ce5* ^(ov) #\v #\x #\u #\y #\w))
    (#\N (ce5* ^(ov) #\v #\x #\y #\u #\w))))

(defun ov ()
  (e** ^(pe ^(anf) #\D)))

(b)

(close *t*)

