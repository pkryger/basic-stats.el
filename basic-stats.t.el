;;; basic-stats.t.el --- Unit tests for basic-stats.el

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

(require 'basic-stats)
(require 'ert)

(ert-deftest stats-test-quartile-1-3-elts ()
  (should (=
           (pk/quartile '(1 2 3) 1)
           1)))

(ert-deftest stats-test-quartile-2-3-elts ()
  (should (=
           (pk/quartile '(1 2 3) 2)
           2)))

(ert-deftest stats-test-quartile-3-3-elts ()
  (should (=
           (pk/quartile '(1 2 3) 3)
           3)))

(ert-deftest stats-test-quartile-1-3-elts-include ()
  (should (=
           (pk/quartile '(1 2 3) 1 :include)
           1.5)))

(ert-deftest stats-test-quartile-2-3-elts-include ()
  (should (=
           (pk/quartile '(1 2 3) 2 :include)
           2)))

(ert-deftest stats-test-quartile-3-3-elts-include ()
  (should (=
           (pk/quartile '(1 2 3) 3 :include)
           2.5)))

(ert-deftest stats-test-quartile-2-2-elts ()
  (should (=
           (pk/quartile '(1 2) 2)
           1.5)))

(ert-deftest stats-test-quartile-2-2-elts-include ()
  (should (=
           (pk/quartile '(1 2) 2 :include)
           1.5)))

(ert-deftest stats-test-quartile-1-0-elts ()
  (should-not (pk/quartile '() 1)))

(ert-deftest stats-test-quartile-1-0-elts-include ()
  (should-not (pk/quartile '() 1 :include)))

(ert-deftest stats-test-quartile-2-0-elts ()
  (should-not (pk/quartile '() 2)))

(ert-deftest stats-test-quartile-2-0-elts-include ()
  (should-not (pk/quartile '() 2 :include)))

(ert-deftest stats-test-quartile-3-0-elts ()
  (should-not (pk/quartile '() 3)))

(ert-deftest stats-test-quartile-3-0-elts-include ()
  (should-not (pk/quartile '() 3 :include)))

(ert-deftest stats-test-quartile-1-1-elt ()
  (should-not (pk/quartile '(1) 1)))

(ert-deftest stats-test-quartile-1-1-elt-include ()
  (should-not (pk/quartile '(1) 1 :include)))

(ert-deftest stats-test-quartile-2-1-elt ()
  (should-not (pk/quartile '(1) 2)))

(ert-deftest stats-test-quartile-2-1-elt-include ()
  (should-not (pk/quartile '(1) 2 :include)))

(ert-deftest stats-test-quartile-3-1-elt ()
  (should-not (pk/quartile '(1) 3)))

(ert-deftest stats-test-quartile-3-1-elt-include ()
  (should-not (pk/quartile '(1) 3 :include)))

(ert-deftest stats-test-quartile-1-2-elts ()
  (should-not (pk/quartile '(1 2) 1)))

(ert-deftest stats-test-quartile-1-2-elts-include ()
  (should-not (pk/quartile '(1 2) 1 :include)))

(ert-deftest stats-test-quartile-3-2-elts ()
  (should-not (pk/quartile '(1 2) 3)))

(ert-deftest stats-test-quartile-3-2-elts-include ()
  (should-not (pk/quartile '(1 2) 3 :include)))

(ert-deftest stats-test-quartile-1-wiki-sample ()
  (should (=
           (pk/quartile '(6 7 36 15 39 40 41 42 43 47 49) 1)
           15)))

(ert-deftest stats-test-quartile-2-wiki-sample ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 49 47) 2)
           40)))

(ert-deftest stats-test-quartile-3-wiki-sample ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 47 43 49) 3)
           43)))

(ert-deftest stats-test-quartile-1-wiki-sample-include ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 43 42 47 49) 1 :include)
           25.5)))

(ert-deftest stats-test-quartile-2-wiki-sample-include ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 42 41 43 47 49) 2 :include)
           40)))

(ert-deftest stats-test-quartile-3-wiki-sample-include ()
  (should (=
           (pk/quartile '(6 7 15 36 39 41 40 42 43 47 49) 3 :include)
           42.5)))

(ert-deftest stats-test-median-wiki-sample ()
  (should (=
           (pk/median '(6 7 15 39 36 40 41 42 43 47 49))
           40)))

(ert-deftest stats-test-five-nums-wiki-sample ()
  (should (equal
           (pk/five-nums '(7 6 15 36 39 40 41 42 43 49 47))
           '(6 15 40 43 49))))

(ert-deftest stats-test-five-nums-wiki-sample-include ()
  (should (equal
           (pk/five-nums '(6 7 36 15 39 40 41 42 43 47 49) :include)
           '(6 25.5 40 42.5 49))))

(ert-deftest stats-tests-five-nums-with-header ()
  (should (equal
           (pk/five-nums-with-header '(6 7 15 36 39 40 41 43 42 47 49))
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 15 40 43 49)))))

(ert-deftest stats-tests-five-nums-with-header-include ()
  (should (equal
           (pk/five-nums-with-header '(6 7 15 36 40 39 41 42 43 47 49) :include)
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 25.5 40 42.5 49)))))


(ert-deftest stats-test-quartile-1-wiki-sample-sorted ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 1 :exclude t)
           15)))

(ert-deftest stats-test-quartile-2-wiki-sample-sorted ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 2 :exclude t)
           40)))

(ert-deftest stats-test-quartile-3-wiki-sample-sorted ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 3 :exclude t)
           43)))

(ert-deftest stats-test-quartile-1-wiki-sample-include-sorted ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 1 :include t)
           25.5)))

(ert-deftest stats-test-quartile-2-wiki-sample-include-sorted ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 2 :include t)
           40)))

(ert-deftest stats-test-quartile-3-wiki-sample-include-sorted ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 3 :include t)
           42.5)))

(ert-deftest stats-test-median-wiki-sample-sorted ()
  (should (=
           (pk/median '(6 7 15 36 39 40 41 42 43 47 49) t)
           40)))

(ert-deftest stats-test-five-nums-wiki-sample-sorted ()
  (should (equal
           (pk/five-nums '(6 7 15 36 39 40 41 42 43 47 49) :exclude t)
           '(6 15 40 43 49))))

(ert-deftest stats-test-five-nums-wiki-sample-include-sorted ()
  (should (equal
           (pk/five-nums '(6 7 15 36 39 40 41 42 43 47 49) :include t)
           '(6 25.5 40 42.5 49))))

(ert-deftest stats-tests-five-nums-with-header-sorted ()
  (should (equal
           (pk/five-nums-with-header '(6 7 15 36 39 40 41 42 43 47 49) :exclude t)
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 15 40 43 49)))))

(ert-deftest stats-tests-five-nums-with-header-include-sorted ()
  (should (equal
           (pk/five-nums-with-header '(6 7 15 36 39 40 41 42 43 47 49) :include t)
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 25.5 40 42.5 49)))))

(provide 'basic-stats.t)
;;; basic-stats.t ends here
