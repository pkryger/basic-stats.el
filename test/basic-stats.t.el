;;; basic-stats.t.el --- Unit tests for basic-stats.el -*- lexical-binding: t -*-

;;; Author: Przemysław Kryger <pkryger@gmail.com>

;;; This file is not a part of GNU Emacs.

;;; Commentary:

;;; A suite of tests for functions defined in `basic-stats.el' to ensure
;;; implementation validity.

;;; License:

;;; MIT License

;;; Copyright (c) 2020 Przemysław Kryger

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; Code:

(when-let* ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file
                                          buffer-file-name))))
  (load-file (file-name-concat dir "undercover-init.el")))

(require 'basic-stats)
(require 'ert)

(ert-deftest basic-stats-test-quartile-1-3-elts ()
  (should (=
           (basic-stats-quartile '(1 2 3) 1)
           1)))

(ert-deftest basic-stats-test-quartile-2-3-elts ()
  (should (=
           (basic-stats-quartile '(1 2 3) 2)
           2)))

(ert-deftest basic-stats-test-quartile-3-3-elts ()
  (should (=
           (basic-stats-quartile '(1 2 3) 3)
           3)))

(ert-deftest basic-stats-test-quartile-1-3-elts-include ()
  (should (=
           (basic-stats-quartile '(1 2 3) 1 :include)
           1.5)))

(ert-deftest basic-stats-test-quartile-2-3-elts-include ()
  (should (=
           (basic-stats-quartile '(1 2 3) 2 :include)
           2)))

(ert-deftest basic-stats-test-quartile-3-3-elts-include ()
  (should (=
           (basic-stats-quartile '(1 2 3) 3 :include)
           2.5)))

(ert-deftest basic-stats-test-quartile-2-2-elts ()
  (should (=
           (basic-stats-quartile '(1 2) 2)
           1.5)))

(ert-deftest basic-stats-test-quartile-2-2-elts-include ()
  (should (=
           (basic-stats-quartile '(1 2) 2 :include)
           1.5)))

(ert-deftest basic-stats-test-quartile-1-0-elts ()
  (should-not (basic-stats-quartile '() 1)))

(ert-deftest basic-stats-test-quartile-1-0-elts-include ()
  (should-not (basic-stats-quartile '() 1 :include)))

(ert-deftest basic-stats-test-quartile-2-0-elts ()
  (should-not (basic-stats-quartile '() 2)))

(ert-deftest basic-stats-test-quartile-2-0-elts-include ()
  (should-not (basic-stats-quartile '() 2 :include)))

(ert-deftest basic-stats-test-quartile-3-0-elts ()
  (should-not (basic-stats-quartile '() 3)))

(ert-deftest basic-stats-test-quartile-3-0-elts-include ()
  (should-not (basic-stats-quartile '() 3 :include)))

(ert-deftest basic-stats-test-quartile-1-1-elt ()
  (should-not (basic-stats-quartile '(1) 1)))

(ert-deftest basic-stats-test-quartile-1-1-elt-include ()
  (should-not (basic-stats-quartile '(1) 1 :include)))

(ert-deftest basic-stats-test-quartile-2-1-elt ()
  (should-not (basic-stats-quartile '(1) 2)))

(ert-deftest basic-stats-test-quartile-2-1-elt-include ()
  (should-not (basic-stats-quartile '(1) 2 :include)))

(ert-deftest basic-stats-test-quartile-3-1-elt ()
  (should-not (basic-stats-quartile '(1) 3)))

(ert-deftest basic-stats-test-quartile-3-1-elt-include ()
  (should-not (basic-stats-quartile '(1) 3 :include)))

(ert-deftest basic-stats-test-quartile-1-2-elts ()
  (should-not (basic-stats-quartile '(1 2) 1)))

(ert-deftest basic-stats-test-quartile-1-2-elts-include ()
  (should-not (basic-stats-quartile '(1 2) 1 :include)))

(ert-deftest basic-stats-test-quartile-3-2-elts ()
  (should-not (basic-stats-quartile '(1 2) 3)))

(ert-deftest basic-stats-test-quartile-3-2-elts-include ()
  (should-not (basic-stats-quartile '(1 2) 3 :include)))

(ert-deftest basic-stats-test-quartile-1-wiki-sample ()
  (should (=
           (basic-stats-quartile '(6 7 36 15 39 40 41 42 43 47 49) 1)
           15)))

(ert-deftest basic-stats-test-quartile-2-wiki-sample ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 43 49 47) 2)
           40)))

(ert-deftest basic-stats-test-quartile-3-wiki-sample ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 47 43 49) 3)
           43)))

(ert-deftest basic-stats-test-quartile-1-wiki-sample-include ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 43 42 47 49) 1 :include)
           25.5)))

(ert-deftest basic-stats-test-quartile-2-wiki-sample-include ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 42 41 43 47 49) 2 :include)
           40)))

(ert-deftest basic-stats-test-quartile-3-wiki-sample-include ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 41 40 42 43 47 49) 3 :include)
           42.5)))

(ert-deftest basic-stats-test-median-wiki-sample ()
  (should (=
           (basic-stats-median '(6 7 15 39 36 40 41 42 43 47 49))
           40)))

(ert-deftest basic-stats-test-five-nums-wiki-sample ()
  (should (equal
           (basic-stats-five-nums '(7 6 15 36 39 40 41 42 43 49 47))
           '(6 15 40 43 49))))

(ert-deftest basic-stats-test-five-nums-wiki-sample-include ()
  (should (equal
           (basic-stats-five-nums '(6 7 36 15 39 40 41 42 43 47 49) :include)
           '(6 25.5 40 42.5 49))))

(ert-deftest basic-stats-tests-five-nums-with-header ()
  (should (equal
           (basic-stats-five-nums-with-header '(6 7 15 36 39 40 41 43 42 47 49))
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 15 40 43 49)))))

(ert-deftest basic-stats-tests-five-nums-with-header-include ()
  (should (equal
           (basic-stats-five-nums-with-header '(6 7 15 36 40 39 41 42 43 47 49) :include)
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 25.5 40 42.5 49)))))


(ert-deftest basic-stats-test-quartile-1-wiki-sample-sorted ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 43 47 49) 1 :exclude t)
           15)))

(ert-deftest basic-stats-test-quartile-2-wiki-sample-sorted ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 43 47 49) 2 :exclude t)
           40)))

(ert-deftest basic-stats-test-quartile-3-wiki-sample-sorted ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 43 47 49) 3 :exclude t)
           43)))

(ert-deftest basic-stats-test-quartile-1-wiki-sample-include-sorted ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 43 47 49) 1 :include t)
           25.5)))

(ert-deftest basic-stats-test-quartile-2-wiki-sample-include-sorted ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 43 47 49) 2 :include t)
           40)))

(ert-deftest basic-stats-test-quartile-3-wiki-sample-include-sorted ()
  (should (=
           (basic-stats-quartile '(6 7 15 36 39 40 41 42 43 47 49) 3 :include t)
           42.5)))

(ert-deftest basic-stats-test-median-wiki-sample-sorted ()
  (should (=
           (basic-stats-median '(6 7 15 36 39 40 41 42 43 47 49) t)
           40)))

(ert-deftest basic-stats-test-five-nums-wiki-sample-sorted ()
  (should (equal
           (basic-stats-five-nums '(6 7 15 36 39 40 41 42 43 47 49) :exclude t)
           '(6 15 40 43 49))))

(ert-deftest basic-stats-test-five-nums-wiki-sample-include-sorted ()
  (should (equal
           (basic-stats-five-nums '(6 7 15 36 39 40 41 42 43 47 49) :include t)
           '(6 25.5 40 42.5 49))))

(ert-deftest basic-stats-tests-five-nums-with-header-sorted ()
  (should (equal
           (basic-stats-five-nums-with-header '(6 7 15 36 39 40 41 42 43 47 49) :exclude t)
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 15 40 43 49)))))

(ert-deftest basic-stats-tests-five-nums-with-header-include-sorted ()
  (should (equal
           (basic-stats-five-nums-with-header '(6 7 15 36 39 40 41 42 43 47 49) :include t)
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 25.5 40 42.5 49)))))

(ert-deftest basic-stats-tests-convert-time ()
  (should-not (basic-stats--convert-time nil))
  (should (equal (basic-stats--convert-time 0)
                 "0.000s"))
  (should (equal (basic-stats--convert-time 9876.54321)
                 "2h 44min 36.543s"))
  (should (equal (basic-stats--convert-time 3600)
                 "1h 0min 0.000s"))
  (should (equal (basic-stats--convert-time 1234.56789)
                 "20min 34.567s"))
  (should (equal (basic-stats--convert-time 60)
                 "1min 0.000s"))
  (should (equal (basic-stats--convert-time 12.3456789)
                 "12.346s"))
  (should (equal (basic-stats--convert-time 1)
                 "1.000s"))
  (should (equal (basic-stats--convert-time 1.23456789e-1)
                 "123.457ms"))
  (should (equal (basic-stats--convert-time 1.23456789e-3)
                 "1.235ms"))
  (should (equal (basic-stats--convert-time 1e-3)
                 "1.000ms"))
  (should (equal (basic-stats--convert-time 1.23456789e-4)
                 "123.457µs"))
  (should (equal (basic-stats--convert-time 1.23456789e-6)
                 "1.235µs"))
  (should (equal (basic-stats--convert-time 1e-6)
                 "1.000µs"))
  (should (equal (basic-stats--convert-time 1.23456789e-7)
                 "123.457ns"))
  (should (equal (basic-stats--convert-time 1.23456789e-9)
                 "1.235ns"))
  (should (equal (basic-stats--convert-time 1.e-9)
                 "1.000ns"))
  (should (equal (basic-stats--convert-time 1.23456789e-10)
                 "123.457ps"))
  (should (equal (basic-stats--convert-time 1.23456789e-12)
                 "1.235ps"))
  (should (equal (basic-stats--convert-time 1e-12)
                 "1.000ps")))

(ert-deftest basic-stats-tests-convert-times ()
  (should (equal (basic-stats--convert-times '(1 2 3) t)
                 '((min . "1.000s")
                   (q1 . "1.000s")
                   (med . "2.000s")
                   (q3 . "3.000s")
                   (max . "3.000s"))))
  (should (equal (basic-stats--convert-times '(1) t)
                 '((min . "1.000s")
                   (max . "1.000s"))))
  (should (equal (basic-stats--convert-times '(1 2 3) nil)
                 '((min . 1)
                   (q1 . 1)
                   (med . 2)
                   (q3 . 3)
                   (max . 3))))
  (should (equal (basic-stats--convert-times '(1) nil)
                 '((min . 1)
                   (max . 1)))))

(ert-deftest basic-stats-tests-benchmark-0 ()
  (eval
   `(let* ((result 0)
           (report (basic-stats-benchmark ((f1 (cl-incf result))
                                           (f2 (format "%s" result)))
                     :samples 0
                     :time .0001)))
      (should (= 0 result))
      (should-not report))))

(ert-deftest basic-stats-tests-benchmark ()
  (eval
   `(let* ((result 0)
           (report (basic-stats-benchmark ((f1 (cl-incf result))
                                           (f2 (format "%s" result)))
                     :time .0001)))
      (should (< 0 result))
      (should (assq 'f1 report))
      (should (assq 'f2 report))
      (let-alist (alist-get 'f1 report)
        (should .repetitions)
        (should (stringp .total-time))
        (should (stringp .mean-time))
        (should .gc)
        (should (stringp .gc-time))
        (let-alist .times
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))
        (let-alist .times-sans-gc
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max))))
      (let-alist (alist-get 'f2 report)
        (should .repetitions)
        (should (stringp .total-time))
        (should (stringp .mean-time))
        (should .gc)
        (should (stringp .gc-time))
        (let-alist .times
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))
        (let-alist .times-sans-gc
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))))))

(ert-deftest basic-stats-tests-benchmark-non-interleave ()
  (eval
   `(let* ((result 0)
           (report (basic-stats-benchmark ((f1 (cl-incf result))
                                           (f2 (format "%s" result)))
                     :interleave nil
                     :time .0001)))
      (should (< 0 result))
      (should (assq 'f1 report))
      (should (assq 'f2 report))
      (let-alist (alist-get 'f1 report)
        (should .repetitions)
        (should (stringp .total-time))
        (should (stringp .mean-time))
        (should .gc)
        (should (stringp .gc-time))
        (let-alist .times
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))
        (let-alist .times-sans-gc
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max))))
      (let-alist (alist-get 'f2 report)
        (should .repetitions)
        (should (stringp .total-time))
        (should (stringp .mean-time))
        (should .gc)
        (should (stringp .gc-time))
        (let-alist .times
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))
        (let-alist .times-sans-gc
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))))))

(ert-deftest basic-stats-tests-benchmark-1 ()
  (eval
   `(let* ((result 0)
           (report (basic-stats-benchmark ((f1 (cl-incf result))
                                           (f2 (format "%s" result)))
                     :samples 1
                     :time .0001)))
      (should (< 0 result))
      (should (assq 'f1 report))
      (should (assq 'f2 report))
      (let-alist (alist-get 'f1 report)
        (should .repetitions)
        (should (stringp .total-time))
        (should (stringp .mean-time))
        (should .gc)
        (should (stringp .gc-time))
        (let-alist .times
          (should (stringp .min))
          (should-not .q1)
          (should-not .med)
          (should-not .q3)
          (should (stringp .max)))
        (let-alist .times-sans-gc
          (should-not .q1)
          (should-not .med)
          (should-not .q3)
          (should (stringp .max))))
      (let-alist (alist-get 'f2 report)
        (should .repetitions)
        (should (stringp .total-time))
        (should (stringp .mean-time))
        (should .gc)
        (should (stringp .gc-time))
        (let-alist .times
          (should (stringp .min))
          (should-not .q1)
          (should-not .med)
          (should-not .q3)
          (should (stringp .max)))
        (let-alist .times-sans-gc
          (should-not .q1)
          (should-not .med)
          (should-not .q3)
          (should (stringp .max)))))))

(ert-deftest basic-stats-tests-benchmark-non-human ()
  (eval
   `(let* ((result 0)
           (report (basic-stats-benchmark ((f1 (cl-incf result))
                                           (f2 (format "%s" result)))
                     :human-readable nil
                     :time .0001)))
      (should (< 0 result))
      (should (assq 'f1 report))
      (should (assq 'f2 report))
      (let-alist (alist-get 'f1 report)
        (should .repetitions)
        (should (numberp .total-time))
        (should (numberp .mean-time))
        (should .gc)
        (should (numberp .gc-time))
        (let-alist .times
          (should (numberp .min))
          (should (numberp .q1))
          (should (numberp .med))
          (should (numberp .q3))
          (should (numberp .max)))
        (let-alist .times-sans-gc
          (should (numberp .min))
          (should (numberp .q1))
          (should (numberp .med))
          (should (numberp .q3))
          (should (numberp .max))))
      (let-alist (alist-get 'f2 report)
        (should .repetitions)
        (should (numberp .total-time))
        (should (numberp .mean-time))
        (should .gc)
        (should (numberp .gc-time))
        (let-alist .times
          (should (numberp .min))
          (should (numberp .q1))
          (should (numberp .med))
          (should (numberp .q3))
          (should (numberp .max)))
        (let-alist .times-sans-gc
          (should (numberp .min))
          (should (numberp .q1))
          (should (numberp .med))
          (should (numberp .q3))
          (should (numberp .max)))))))

(ert-deftest basic-stats-tests-benchmark-progn ()
  (eval
   `(let* ((result 0)
           (report (basic-stats-benchmark ((f1 (progn (cl-incf result)
                                                      (format "%s" result))))
                     :time .0001)))
      (should (< 0 result))
      (should (assq 'f1 report))
      (let-alist (alist-get 'f1 report)
        (should .repetitions)
        (should (stringp .total-time))
        (should (stringp .mean-time))
        (should .gc)
        (should (stringp .gc-time))
        (let-alist .times
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))
        (let-alist .times-sans-gc
          (should (stringp .min))
          (should (stringp .q1))
          (should (stringp .med))
          (should (stringp .q3))
          (should (stringp .max)))))))

;; LocalWords: el

(provide 'basic-stats.t)

;;; basic-stats.t.el ends here
