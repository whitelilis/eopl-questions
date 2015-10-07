;;; 1.7 The error message from nth-element is uninformative. Rewrite nth-element so that it produces a more informative error message, such as "a b c does not have 8 elements"

(defun report-error (msg)
  (format t "~a" msg))

(defun nth-element (lst n)
  (defun nth-helper (_lst _n _lst_raw _n_raw)
    (if (null _lst)
        (report-error (format nil "~a does not have ~a elements" _lst_raw (+ _n_raw 1)))
        (progn
          (if (= 0 _n)
              (car _lst)
              (nth-helper (cdr _lst) (- _n 1) _lst_raw _n_raw)))))
  (nth-helper lst n lst n))


;;; 1.8 In the definition of remove-first, if the last line were replaced by (remove-first s (cdr los)), what function would the resulting procedure compute? Give the contract, including the usage statement, for the revised procedure.

;;  remove-until-first : Sym × Listof(Sym) → Listof(Sym)
;;  (remove-until-first s los) removing from beginning until first occurrence of the symbol s, then remove s, return others with the same elements arranged in the same order as los.


;;; 1.9 Define remove, which is like remove-first, except that it removes all occurrences of a given symbol from a list of symbols, not just the first.
;; remove is defined in cl, so define remove-wizard

(defun remove-wizard (s los)
  (if (null los)
      los
      (progn
        (if (eql s (car los))
            (remove-wizard s (cdr los))
            (cons (car los) (remove-wizard s (cdr los)))))))
