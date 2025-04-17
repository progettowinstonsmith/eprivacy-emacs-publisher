;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'roman-numerals)

(ert-deftest roman-numeral-basic ()
  (should (equal (roman-numeral 1) "I"))
  (should (equal (roman-numeral 4) "IV"))
  (should (equal (roman-numeral 36) "XXXVI"))
  (should (equal (roman-numeral 2025) "MMXXV")))

(ert-deftest roman-numeral-options ()
  (should (equal (roman-numeral 3 :case 'lower) "iii"))
  (should (equal (roman-numeral 2 :suffix 'masculine) "IIº"))
  (should (equal (roman-numeral 2 :suffix 'feminine :case 'lower) "iiª")))

(ert-deftest roman-numeral-parse-basic ()
  (should (= (roman-numeral-parse "X") 10))
  (should (= (roman-numeral-parse "xxxvi") 36))
  (should (= (roman-numeral-parse "MMXXV") 2025))
  (should (= (roman-numeral-parse "iiiº") 3)))
