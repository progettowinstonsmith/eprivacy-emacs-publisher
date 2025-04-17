;; -*- lexical-binding: t; -*-

(defconst roman-numerals--table
  '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
    (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
    (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I"))
  "Alist of decimal values mapped to Roman numeral symbols.")

(defun roman-numeral (n &rest options)
  "Convert integer N to a Roman numeral string.

Optional keyword OPTIONS:
 - :case (upper|lower)
 - :suffix (none|masculine|feminine)"
  (unless (and (integerp n) (> n 0) (<= n 3999))
    (error "N must be an integer between 1 and 3999"))
  (let ((table roman-numerals--table)
        (roman "")
        (case-option (plist-get options :case))
        (suffix (plist-get options :suffix)))
    (dolist (pair table)
      (let ((value (car pair))
            (symbol (cdr pair)))
        (while (>= n value)
          (setq roman (concat roman symbol))
          (setq n (- n value)))))
    (when (eq case-option 'lower)
      (setq roman (downcase roman)))
    (cond
     ((eq suffix 'masculine) (setq roman (concat roman "º"))) ;; º
     ((eq suffix 'feminine)  (setq roman (concat roman "ª")))) ;; ª
    roman))

(defun roman-numeral-parse (s)
  "Parse a Roman numeral string S and return its integer value.
Ignores case and trailing suffixes (º, ª)."
  (let ((input (upcase (replace-regexp-in-string "[ªº]" "" s)))
        (table roman-numerals--table)
        (result 0))
    (while (> (length input) 0)
      (let ((matched nil))
        (dolist (pair table)
          (let ((value (car pair))
                (symbol (cdr pair)))
            (when (string-prefix-p symbol input)
              (setq result (+ result value))
              (setq input (substring input (length symbol)))
              (setq matched t))))
      (unless matched
        (error "Invalid Roman numeral sequence: %s" s))))
    result))

(provide 'roman-numerals)
;;; roman-numerals.el ends here
