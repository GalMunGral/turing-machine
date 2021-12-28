#!/usr/bin/env sbcl --script

(set-macro-character #\^ (lambda (stream char)
  (declare (ignore char))
  (list 'lambda (list) (read stream t nil t))))

(defvar *pos* 2)
(defconstant *t* (open "tape" :if-exists :overwrite :direction :io))

(defun none (c) (eq c #\Space))

(defun cur-sym ()
  (file-position *t* *pos*)
  (read-char *t* nil #\Space)
 )

(defun L () (decf *pos*))

(defun R () (incf *pos*))

(defun P (c) 
  (file-position *t* *pos*)
  (write-char c *t*))

(defun f/find-first (k return a)
  (cond
    ((eq (cur-sym) #\E)
      (L)
      (f1/find k return a))
    (t
      (L)
      (f/find-first k return a))))

(defun f1/find (k return a) 
  (cond
    ((eq (cur-sym) a)
      (funcall k))
    ((none (cur-sym))
      (R)
      (f2/find-or-fail k return a))
    (t
      (R)
      (f1/find k return a))))
    
(defun f2/find-or-fail (k return a)
  (cond
    ((eq (cur-sym) a)
      (funcall k))
    ((none (cur-sym))
      (R)
      (funcall return))
    (t
      (R)
      (f1/find k return a))))

(defun e/erase-first (k return a)
  (f/find-first ^(e1/erase k return a) return a))

(defun e1/erase (k return a)
  (P #\Space)
  (funcall k))

(defun e/erase-all (k a)
  (e/erase-first ^(e/erase-all k a) k a))

(defun pe/append (k b)
  (f/find-first ^(pe1/print-at-the-end k b) k #\E))

(defun pe1/print-at-the-end (k b)
  (cond
    ((none (cur-sym))
      (P b)
      (funcall k))
    (t
      (R) (P (cur-sym)) (R)
      (pe1/print-at-the-end k b))))

(defun l/move-left (k)
  (L)
  (funcall k))

(defun r/move-right (k)
  (R)
  (funcall k))

(defun f/find-first-marked (k return a)
  (f/find-first ^(l/move-left k) return a))

(defun c/copy-first (k return a)
  (f/find-first-marked ^(c1/copy k) return a))

(defun c1/copy (k)
  (let ((b (cur-sym)))
    (if b (pe/append k b))))

(defun ce/copy-first-erase (k return a)
  (c/copy-first ^(e/erase-first k return a) return a))

(defun ce/copy-all-erase (k a)
  (ce/copy-first-erase ^(ce/copy-all-erase k a) k a))

(defun re/replace-first (k return a b)
  (f/find-first ^(re1/replace k return a b) return a))

(defun re1/replace (k return a b)
  (P #\Space) (P b)
  (funcall k))

(defun re/replace-all (k a b)
  (re/replace-first ^(re/replace-all k a b) k a b))

(defun cr/copy-first-replace (k return a)
  (c/copy-first ^(re/replace-first k return a #\a) return a))

(defun cr/copy-all (k a)
  (cr/copy-first-replace ^(cr/copy-all k a) ^(re/replace-all k #\a a) a))

(defun cp/compare-first (k-yes k-no k-nil a b)
  (f/find-first-marked ^(cp1/compare k-yes k-no b) 
    ^(f/find-first k-no k-nil b) a))

(defun cp1/compare (k-yes k-no b)
  (let ((g (cur-sym)))
    (f/find-first-marked ^(cp2/compare k-yes k-no g) k-no b)))

(defun cp2/compare (k-yes k-no g)
  (cond
    ((eq (cur-sym) g)
      (funcall k-yes))
    (t
      (funcall k-no))))

(defun cpe/compare-first-erase (k-yes k-no k-nil a b)
  (cp/compare-first ^(e/erase-first ^(e/erase-first k-yes k-yes b) k-yes a) 
    k-no k-nil a b))

(defun cpe/compare-sequence-erase (k-no k-yes a b)
  (cpe/compare-first-erase ^(cpe/compare-sequence-erase k-no k-yes a b)
    k-no k-yes a b))

(defun g/move-to-end (k)
  (cond
    ((none (cur-sym))
      (R)
      (g1/move-to-end k))
    (t
      (R)
      (g/move-to-end k))))

(defun g1/move-to-end (k)
  (cond
    ((none (cur-sym))
      (funcall k))
    (t
      (R)
      (g/move-to-end k))))

(defun g/find-last (k a)
  (g/move-to-end ^(g1/find k a)))

(defun g1/find (k a)
  (cond
    ((eq (cur-sym) a)
      (funcall k))
    (t
      (L)
      (g1/find k a))))

(defun pe2/append-2 (k a b)
  (pe/append ^(pe/append k b) a))

(defun ce2/copy-all-erase (k a b)
  (ce/copy-all-erase ^(ce/copy-all-erase k b) a))

(defun ce3/copy-all-erase (k a b c)
  (ce/copy-all-erase ^(ce2/copy-all-erase k b c) a))

(defun ce4/copy-all-erase (k a b c d)
  (ce/copy-all-erase ^(ce3/copy-all-erase k b c d) a))

(defun ce5/copy-all-erase (k a b c d e)
  (ce/copy-all-erase ^(ce4/copy-all-erase k b c d e) a))

(defun e/erase-all-2 (k a b)
  (e/erase-all ^(e/erase-all k b) a))

(defun e/reset (k)
  (cond
    ((eq (cur-sym) #\E)
      (R)
      (e1/clear-all-marks k))
    (t
      (L)
      (e/reset k))))

(defun e1/clear-all-marks (k)
  (cond
    ((none (cur-sym))
      (funcall k))
    (t
      (R) (P #\Space) (R)
      (e1/clear-all-marks k))))

(defun con/mark-config (k a)
  (cond
    ((eq (cur-sym) #\A)
      (L) (P a) (R) ; Mark 'D'
      (con1/mark-state k a))
    (t
      (R) (R)
      (con/mark-config k a))))

(defun con1/mark-state (k a)
  (cond
    ((eq (cur-sym) #\D)
      (R) (P a) (R)
      (con2/mark-symbol k a))
    ((eq (cur-sym) #\A)
      (R) (P a) (R)
      (con1/mark-state k a))))

(defun con2/mark-symbol (k a)
  (cond
    ((not (eq (cur-sym) #\C))
      (R) (R)
      (funcall k))
    (t
      (R) (P a) (R)
      (con2/mark-symbol k a))))

;;;; THE UNIVERSAL MACHINE

(defun b/begin ()
  (f/find-first ^(b1/init) ^(b1/init) #\.))

(defun b1/init ()
  (R) (P #\Space) (R) (P #\:)
  (R) (P #\Space) (R) (P #\D)
  (R) (P #\Space) (R) (P #\A)
  (R) (P #\Space) (R) (P #\D)
  (anf/advance-to-next))

(defun anf/advance-to-next ()
  (g/find-last ^(anf1/advance-to-next) #\:))

(defun anf1/advance-to-next ()
  (con/mark-config ^(fom/find-matching-rule) #\y))

(defun fom/find-matching-rule ()
  (cond
    ((eq (cur-sym) #\;)
      (R) (P #\z) (L) ; mark visited
      (con/mark-config ^(fmp/match-rule) #\x)) 
    ((eq (cur-sym) #\z)
      (L) (L) ; skip visited rules
      (fom/find-matching-rule))
    (t
      (L)
      (fom/find-matching-rule))))

(defun fmp/match-rule ()
  ;; There seems to be a mistake here:
  ;; On error the machine should jump to 'anf' state
  ;; rather than 'fom' as 'y' marks have been erased
  (cpe/compare-sequence-erase ^(e/erase-all-2 ^(anf/advance-to-next) #\x #\y)
    ^(sim/simulate-rule) #\x #\y))

(defun sim/simulate-rule ()
  (f/find-first-marked ^(sim1/simulate) ^(sim1/simulate) #\z))

(defun sim1/simulate ()
  (con/mark-config ^(sim2/mark-operation) #\Space))

(defun sim2/mark-operation ()
  (cond
    ((eq (cur-sym) #\A)
      (sim3/mark-next-state))
    (t
      ;; The paper says RPRRR, seems to be a mistake ??
      (L) (P #\u) (R)
      (R) (R)
      (sim2/mark-operation))))

(defun sim3/mark-next-state ()
  (cond
    ((not (eq (cur-sym) #\A))
      (L) (P #\y)
      (e/erase-all ^(mf/mark-full-config) #\z))
    (t
      (L) (P #\y) (R)
      (R) (R)
      (sim3/mark-next-state))))

(defun mf/mark-full-config ()
  ;; typo - 'mf' should be 'mf1'
  (g/find-last ^(mf1/find-cursor) #\:))

(defun mf1/find-cursor ()
  (cond
    ((eq (cur-sym) #\A)
      (L) (L) (L) (L)
      (mf2/mark-prev-symbols))
    (t
      (R) (R)
      (mf1/find-cursor))))

(defun mf2/mark-prev-symbols ()
  (cond
    ((eq (cur-sym) #\:)
      (mf4/mark-next-symbols))
    ((eq (cur-sym) #\C)
      (R) (P #\x) (L)
      (L) (L)
      (mf2/mark-prev-symbols))
    ((eq (cur-sym) #\D)
      (R) (P #\x) (L)
      (L) (L)
      (mf3/mark-prev-symbols))))

(defun mf3/mark-prev-symbols ()
  (cond
    ((eq (cur-sym) #\:)
      (mf4/mark-next-symbols))
    (t
      (R) (P #\v) (L)
      (L) (L)
      (mf3/mark-prev-symbols))))

(defun mf4/mark-next-symbols ()
  (con/mark-config ^(l/move-left ^(l/move-left ^(mf5/mark-next-symbols))) #\Space))

(defun mf5/mark-next-symbols ()
  (cond
    ((none (cur-sym))
      (P #\:)
      (sh/print-output))
    (t
      (R) (P #\w) (R)
      (mf5/mark-next-symbols))))

(defun sh/print-output ()
  (f/find-first ^(sh1/print-output) ^(inst/print-full-config) #\u))

(defun sh1/print-output ()
  (L) ; LLL in the paper ??
  (sh2/print-output))

(defun sh2/print-output ()
  (cond
    ((eq (cur-sym) #\D)
      (R) (R) ; RRRR in the paper ??
      (sh3/print-output)) ; typo: 'sh2' should be 'sh3'
    (t
      (inst/print-full-config))))

(defun sh3/print-output ()
  (cond
    ((not (eq (cur-sym) #\C))
      (inst/print-full-config))
    (t ; DC..
      (R) (R)
      (sh4/print-output))))

(defun sh4/print-output ()
  (cond
    ((not (eq (cur-sym) #\C)) ; DC
      (pe2/append-2 ^(inst/print-full-config) #\0 #\:))
    (t ; DCC..
      (R) (R)
      (sh5/print-output))))

(defun sh5/print-output ()
  (cond
    ((not (eq (cur-sym) #\C)) ; DCC
      (pe2/append-2 ^(inst/print-full-config) #\1 #\:))
    (t
      (inst/print-full-config))))

(defun inst/print-full-config ()
  (g/find-last ^(l/move-left ^(inst1/print-full-config)) #\u))

(defun inst1/print-full-config ()
  (let ((a (cur-sym)))
    (R) (P #\Space)
    (case a
      (#\L (ce5/copy-all-erase ^(ov/over) #\v #\y #\x #\u #\w))
      (#\R (ce5/copy-all-erase ^(ov/over) #\v #\x #\u #\y #\w))
      (#\N (ce5/copy-all-erase ^(ov/over) #\v #\x #\y #\u #\w)))))

(defun ov/over ()
  (e/reset ^(pe/append ^(anf/advance-to-next) #\D)))

(b/begin)

(close *t*)

