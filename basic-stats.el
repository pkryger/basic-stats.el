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

(defun basic-stats--convert-time (time)
  "Convert TIME to a human readable format."
  (cond
   ((null time) nil)
   ((= 0 time) "0")
   ((<= 3600 time)
    (let* ((hours (floor (/ time 3600)))
           (fseconds (- time (* hours 3600)))
           (seconds (floor fseconds)))
      (format "%dh %dmin %d.%ds"
              hours
              (/ seconds 60)
              (% seconds 60)
              (* 1000 (- fseconds seconds)))))
   ((<= 60 time)
    (let ((seconds (floor time)))
      (format "%dmin %d.%ds"
              (/ seconds 60)
              (% seconds 60)
              (* 1000 (- time seconds)))))
   ((<= 1 time)
    (format "%.3fs" time))
   ((<= 1e-3 time)
    (format "%.3fms" (* 1000 time)))
   ((<= 1e-6 time)
    (format "%.3fµs" (* 1000 1000 time)))
   ((<= 1e-9 time)
    (format "%.3fns" (* 1000 1000 1000 time)))
   (t
    (format "%.3fps" (* 1000 1000 1000 1000 time)))))

(defun basic-stats--convert-times (times human-readable)
  "Calculate five numbers for  TIMES.
TIMES is a list of numbers to calculate `basic-stats-five-nums'
on.  When HUMAN-READABLE is non-nil, duration are reported as
strings with time units."
  (cl-remove-if (lambda (elt)
                  (null (cdr elt)))
                (cl-mapcar (lambda (label num)
                             (cons label
                                   (if human-readable
                                       (basic-stats--convert-time num)
                                     num)))
                           '(min q1 med q3 max)
                           (basic-stats-five-nums times))))

(defun basic-stats--convert-result (result human-readable)
  "Convert RESULT to a reportable form.
RESULT is a list where each element is in a from (REPETITIONS
TIME GC GC-TIME), like a value returned from `benchmark-call',
which see.  When HUMAN-READABLE is non-nil, duration are reported
as strings with time units."
  (let ((total (cl-reduce
                (lambda (acc res)
                  (list (+ (nth 0 acc) (nth 0 res))
                        (+ (nth 1 acc) (nth 1 res))
                        (+ (nth 2 acc) (nth 2 res))
                        (+ (nth 3 acc) (nth 3 res))))
                result))
        (times  (basic-stats--convert-times
                 (mapcar (lambda (res)
                           (/ (nth 1 res)
                              (nth 0 res)))
                         result)
                 human-readable))
        (times-sans-gc (basic-stats--convert-times
                        (mapcar (lambda (res)
                                  (/ (- (nth 1 res) (nth 3 res))
                                     (nth 0 res)))
                                result)
                        human-readable)))
    (list
     (cons 'repetitions (nth 0 total))
     (cons 'total-time
           (let ((total-time (nth 1 total)))
             (if human-readable
                 (basic-stats--convert-time total-time)
               total-time)))
     (cons 'mean-time
           (let ((mean-time (/ (nth 1 total)
                               (nth 0 total))))
           (if human-readable
               (basic-stats--convert-time mean-time)
             mean-time)))
     (cons 'gc (nth 2 total))
     (cons 'gc-time
           (let ((gc-time (nth 3 total)))
             (if human-readable
                 (basic-stats--convert-time gc-time)
               gc-time)))
     (cons 'mean-time-sans-gc
           (let ((mean-time-sans-gc (/ (- (nth 1 total) (nth 3 total))
                                       (nth 0 total))))
             (if human-readable
                 (basic-stats--convert-time mean-time-sans-gc)
               mean-time-sans-gc)))
     (cons 'times times)
     (cons 'times-sans-gc times-sans-gc))))

;;;###autoload
(cl-defmacro basic-stats-benchmark
    (specs &key (samples 12) (time .5) (interleave t) (human-readable t))
  "Perform benchmark of specified SPECS.
Each element of SPECS is a list (SYMBOL FORM) where symbol is
a user specified name and FORM is the form to be benchmarked.  If
first element of FORM is `eq' to `progn' it will be skipped in
benchmarked code.  For example the following FORM:
  (progn
    (foo)
    (bar))
will result with the following code being benchmarked:
  (foo)
  (bar)

The argument SAMPLES is the number of times each FORM is
sampled (note, there may be many repetitions of FORM in a single
sample).  The argument TIME is for how long to keep collecting
samples (in seconds).  In other words each sample being measured
for approximately TIME/REPETITIONS seconds and the total
execution time being approximately (length SPECS)*TIME seconds.

When INTERLEAVE is non-nil benchmarking of consecutive samples of
a given FORM is randomly interleaved with benchmarking of other
FORMs.  For more rationale, please see
https://github.com/google/benchmark/issues/1051.

When HUMAN-READABLE is non-nil duration values in the returned
value are strings with unit suffixes, for example h, min, s, ms,
µs, ns, ps.

The value returned is an alist where each element is in a form
of (SYMBOL . REPORT).  The REPORT is an alist with the following
keys:
 - repetitions: how many times the FORM has been repeated (total
   fol all samples),
 - total-time: how long it took to execute FORM  (total for all
   samples),
 - mean-time: the mean repetition time (across all samples),
 - gc: how many times garbage collection was run (total for all
   samples),
 - gc-time: how long it took to execute garbage collection (total
   for all samples),
 - times and times-sans-gc: an alist with statistics computed for
   average times of collected samples, where times include total
   run time, while times-sans-gc substracts garbage collection
   running time for a given sample from a time of a given sample."
  (declare (indent 1))
  (let ((max-time (/ (float time) samples))
        (plan (apply #'vconcat
                     (mapcar (lambda (binding)
                               (make-vector samples (car binding)))
                             specs)))
        (resultsvar (make-symbol "results")))
    (when (and interleave
               (< 1 (length specs)))
      ;; https://en.wikipedia.org/wiki/Fisher–Yates_shuffle#The_modern_algorithm
      (dotimes (n (1- (length plan)))
        (let* ((i (- (length plan) n 1))
               (j (random (1+ i)))
               (tmp (aref plan j)))
          (aset plan j (aref plan i))
          (aset plan i tmp))))
    (append `(let (,resultsvar))
            (mapcar (lambda (elt)
                      `(push
                        (benchmark-call
                         (lambda ()
                           ,@(let ((func (cadr (assq elt specs))))
                               (if (and (listp func) (eq 'progn (car func)))
                                   (cdr func)
                                 (list func))))
                         ,max-time)
                        (alist-get ',elt ,resultsvar)))
                    plan)
            `((mapcar (lambda (result)
                        (cons (car result)
                              (basic-stats--convert-result (cdr result)
                                                           ,human-readable)))
                      ,resultsvar)))))

;; LocalWords: quartile el

(provide 'basic-stats)

;;; basic-stats.el ends here
