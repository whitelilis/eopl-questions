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
