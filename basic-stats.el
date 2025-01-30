;;; basic-stats.el --- Basic statistic functions     -*- lexical-binding: t -*-


;; Author: Przemyslaw Kryger <pkryger@gmail.com>
;; Keywords: tools statistics
;; Homepage: https://github.com/pkryger/basic-stats.el
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.0.0

;;; This file is not a part of GNU Emacs.

;;; Commentary:

;; Provide a few basic functions for statistics.
;; Quartiles are implemented as per https://en.wikipedia.org/wiki/Quartile.

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

(require 'cl-lib)
(require 'seq)

(defun basic-stats-quartile--internal (sequence quartile &optional method)
  "Return a given QUARTILE of a sorted SEQUENCE.
The optional METHOD is the same as in `basic-stats-quartile'."
  (let* ((length (length sequence))
         (offset (if (and (cl-oddp length)
                          (eq method :include))
                     1 0)))
    (cl-flet ((median (sequence)
                      (let* ((length (length sequence))
                             (mid (/ length 2)))
                        (if (cl-oddp length)
                            (seq-elt sequence mid)
                          (/ (+ (seq-elt sequence (1- mid))
                                (seq-elt sequence mid))
                             2.0)))))
        (cond
         ((and (< 1 length) (eq quartile 2))
          (median sequence))
         ((and (< 2 length) (eq quartile 1))
          (median (seq-subseq sequence
                              0
                              (+ (/ length 2)
                                 offset))))
         ((and (< 2 length) (eq quartile 3))
          (median (seq-subseq sequence
                              (- (1+ (/ length 2))
                                 offset))))))))

;;;###autoload
(defun basic-stats-quartile (sequence quartile &optional method sorted)
  "Return a given QUARTILE for the specified SEQUENCE.

When the optional METHOD is nil or `:exclude', the value is returned according
to Method 1 from Wiki: `https://en.wikipedia.org/wiki/Quartile'.

The METHOD can be `:include' to use Method 2 instead.

When SORTED is t it indicates the sequence is already sorted.

Return nil unless one of:
- QUARTILE is one of 1, 2, or 3,
- SEQUENCE length is >=1 and QUARTILE is 2,
- SEQUENCE length is >=2 and QUARTILE is one of 1 or 3."
  (basic-stats-quartile--internal (if sorted
                             sequence
                           (cl-sort sequence '<))
                         quartile method))

;;;###autoload
(defun basic-stats-median (sequence &optional sorted)
  "Return a median for the specified SEQUENCE.
The optional SORTED is the same as in `basic-stats-quartile'."
  (basic-stats-quartile sequence 2 nil sorted))

;;;###autoload
(defun basic-stats-five-nums (sequence &optional method sorted)
  "Return a list consisting of (min q1 med q3 max) for the specified SEQUENCE.
The optional METHOD and SORTED are the same as in `basic-stats-quartile'.
When some values cannot be calculated they are set to nil."
  (if (and (not sorted) sequence)
      (setq sequence (cl-sort sequence '<)))
  (list (when sequence (seq-min sequence))
        (basic-stats-quartile sequence 1 method t)
        (basic-stats-median sequence t)
        (basic-stats-quartile sequence 3 method t)
        (when sequence (seq-max sequence))))

;;;###autoload
(defun basic-stats-five-nums-with-header (sequence &optional method sorted)
  "Return`basic-stats-five-nums' for the specified SEQUENCE with a header.
This is meant as a convenience function for `org-mode' code block to be used
with ':output table'.  The optional METHOD and SORTED are the same as in
`basic-stats-quartile'."
  (list (list "min" "q1" "med" "q3" "max")
        'hline
        (basic-stats-five-nums sequence method sorted)))


;; LocalWords: quartile el

(provide 'basic-stats)

;;; basic-stats.el ends here
