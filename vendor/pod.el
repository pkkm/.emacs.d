; -*- auto-recompile: t -*-
;;; pod.el --- Profile Of Dotemacs .. A per-sexp-evaltime profiler.
;; Time-stamp: <2002-06-19 01:59:01 deego>
;; Copyright (C) 2002 D. Goel
;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Emacs Lisp Archive entry
;; Filename: pod.el
;; Package: pod
;; Author: D. Goel <deego@glue.umd.edu>
;; Version: 0.0
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar pod-home-page
  "http://www.glue.umd.edu/~deego")

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; See also:

;; Quick start:
(defvar pod-quick-start
  "Place this file somewhere in your load-path.  Compile it.

For a general file.el loadtime-profiling:
 (M-x pod-reset-results)
 M-x pod-load-file foo/bar/file.el
 M-x pod-display-results

For dot-emacs profiling:

 Start emacs -q.
 M-x load foo/bar/pod.elc
 (M-x pod-reset-results)
 M-x pod-load-file ~/.emacs.
 M-x pod-display-results.
 C-x C-c.

If your .emacs is split into multiple files and you want to profile
them all, that is more complicated---
Create a file ~/.emacs.pod.  Here's a sample .emacs.pod:
\(load foo/bar/\"pod.elc\"\) ;; saves you having to load it by hand.
;; \(setq pod-files
;;      \(\"~/.emacs\" \"~/.emacs.console\" \"~/.emacs.colors\"\)\)
;; this variable contains the names of the files that your .emacs may
;; load.. and are to to be profiled..  these names should match
;; exactly the ones called from your dot-emacs.  Then, start emacs -q
and

    M-x load-file ~/.emacs.pod
    (M-x pod-reset-results)
    M-x pod-activate-advice
    M-x pod-load-file ~/.emacs
    M-x pod-display-results
    M-x pod-deactivate-advice <-- don't forget this!!
    C-x C-c

(or, if you prefer, put these into your .emacs.pod instead of the M-x
     calls..)

")

;;;###autoload
(defun pod-quick-start ()
  "Provides electric help regarding `pod-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert pod-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar pod-introduction
  "Pod is a per-sexp-evaltime-profile.  Pod profiles the .emacs
\(rather any file\) ...  and helps you identify the sexp taking the most
*loading* time--- this is very different from the usual 'profilers'.
So, Pod's 'load-time-profiling' is best-suited to profiling a .emacs.

If your .emacs is split into multiple files, pod can handle that.

Type M-x pod-quick-start, M-x pod-commentary and M-x pod-introduction.
Once you identify the worst offendors, the commentary has some ideas
on how to shorten the start time..   See all the defcustoms for more
customization, stepthrough provisions etc.

POD does NOT profile the loading of byte-compiled .emacs. IMHO, the
*loading* time for .emacs.el and for .emacs.elc don't differ much, so it
should not matter.   And i guess, if they do differ then each
sexp should hopefully inherit the same constant factor.. So, if you
want to profile an .emacs.elc, simply profile the corresponding
.emacs.el---your aim, after all, is to identify the 'worst
offendors'.

Caution: Does a \(require 'cl\) in it.  This should go away one day.
Additional Suggestions, patches, improvements and code-cricisms
welcome.. Specially code-suggestions on how to get rid of \(require
'cl\).  FUP set to gnu.emacs.help.  Thanks

"
)

;;;###autoload
(defun pod-introduction ()
  "Provides electric help regarding `pod-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert pod-introduction) nil) "*doc*"))

;;; Commentary:
(defvar pod-commentary
  "Pod is a different kind of profiler.  (You emacs comes with
profile.el--the regular profiler..).  While profile.el profiles the
time it takes to *call* a function, pod profiles the time taken to
*define* the function.  Thus, it is most suitable for profiling .emacs
and shortening the time.

Pod tries to be careful to not leave any advise enabled, or to
evaluate any expression of your .emacs twice, but that is not the main
aim.  We assume that you will use it once in a while, to profile, and
then restart your .emacs.  Nonetheless, please do report any such bugs
you find.

Most likely, there will be (require)'s or function-calls in your
.emacs that will be the main culprits.  Once you discover them, you
will want to eliminate most of those.  Moreover, functions and modes
that require loading of files should be moved out of emacs.. they
should be added as hooks to other functions.  Plug: When not moving
his require's into hooks, this author moves them into idledo
(idledo.el).  You emacs should ideally autoload all required
files---even the ones you have downloaded should be autoloaded.
(Plug2: genauto.el).

This author's .emacs currently takes 2.9 seconds to load, but
when/while emacs is idle. it will (require) almost all libraries i
ever need.  This is because i have already transferred all the
(require)'s into idledo's.  I doubt pod will make much difference to
me, but i would like to hear of people's experiences with
pod---whether or not it helped save time.

Note that it is perfectly safe to let this file reside in your
load-path.  Even if this file gets loaded into a running dot-emacs, it
doesn't change any behavior of dot-emacs.  It does define advices, but
those advices are disabled and not activated unless you specifically
request.  Those advices will be useful in the special case when you
want to profile a multiply-split dot-emacs.

The reason i asked you to compile pod.el is so that whatever filed pod
uses: advice.el for instancer, do not appear as 'loaded' to your
.emacs--- so that if your .emacs calls them, hopefully the right
amount of time gets recorded.

The file should be paren-balanced.  Other than that, for the purpose
of pod-loading a file, any errors due to bad expressions in the file
are skipped after a 1-sec pause and a beep.

Also note that pod works by 'find-file ing' your .emacsen buffer,
which is not really neat, and can break some of your .emacs code..
what is a better way to do things?

Note that according to my observation, the results of
 time emacs21 -q -l ~/.emacs.editor  --eval \"(kill-emacs)\"
on tcsh (where my .emacs.editor loads several other files, which were
also profiled..)  have been seen to be smaller by as much as a factor
of 2--3 from the total time indicated by pod.el.  That, IMHO, is not
unreasonable given the difference between a bunch of eval-sexp's and
the time taken to get 2 (current-time)'s surrounding it.. and that of
evaling a file.  In other words, there's overhead.   But again, this
shouldn't affect the *relative* time taken by your individual
instructions ---- that is what matters, right?

See also the bugs and drawbacks mentioned elsewhere.
"  )

;;;###autoload
(defun pod-commentary ()
  "Provides electric help regarding `pod-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert pod-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:
 ;; Drawbacks:
 ;;  The author can't get pod to work without loading advice and cl.
 ;;  This means that if you (require) them in your .emacs, the times
 ;;   they will seem to pod to load quicker (because pod has already
 ;;   loaded them)..

;;; New features:
(defvar pod-new-features
  "none.. first release"
)

;;;###autoload
(defun pod-new-features ()
  "Provides electric help regarding `pod-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert pod-new-features) nil) "*doc*"))

;;; TO DO:
(defvar pod-todo
  "Help..."
)

;;;###autoload
(defun pod-todo ()
  "Provides electric help regarding `pod-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert pod-todo) nil) "*doc*"))

(defvar pod-version "0.0")

;;==========================================
;;; Code:
(require 'cl)
(defcustom pod-before-load-hooks nil "")
(defcustom pod-after-load-hooks nil "")
(run-hooks 'pod-before-load-hooks)

(defcustom pod-max-display 2000 "")

(defcustom pod-files nil "See pod-quick-start")

(defmacro pod-ignore-errors (&rest body)
  "PROGRAMMER: should track ignore-errors-my

Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i was too lazy to convert my macro to..
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
 (ding t)
 (ding t)
 (ding t)
 (message "IGNORED ERROR: %s" (error-message-string ,err))
 (sit-for 1)
 nil))))

(defvar pod-current-file "dummy-file")
(defvar pod-current-point nil)
(defvar pod-current-depth -1 "please do not change this..")
(defvar pod-results nil
  "List of results... Each result is of the form
  (time-taken depth file line-no expression)")

(defvar pod-file-results)
(defvar pod-sorted-results)

(defvar pod-filewise-results nil
  "List of (file total-time num-of-sexps)'s..")

(defadvice load (around pod-advice-load
   (file &optional noerror nomessage nosuffix
         mustsuffix))
  (if (member file pod-files)
      (pod-load file noerror nomessage nosuffix mustsuffix)
    (progn ad-do-it)))
(ad-disable-advice 'load 'around 'pod-advice-load)

(defadvice load-file (around pod-advice-load-file
        (file))
  (if (member file pod-files)
      (pod-load-file file)
    (progn ad-do-it)))
(ad-disable-advice 'load-file 'around 'pod-advice-load-file)

;;;###autoload
(defun pod-reset-results ()
  (interactive)
  (setq pod-results nil)
  ;;(pod-deactivate-advice)
  )

;;;###autoload
(defun pod-deactivate-advice ()
  (interactive)
  (ad-disable-advice 'load-file 'around 'pod-advice-load-file)
  (ad-disable-advice 'load 'around 'pod-advice-load)
  ;; disactivates..
  (ad-activate 'load)
  (ad-activate 'load-file))

;;;###autoload
(defun pod-activate-advice ()
  (interactive)
  (ad-enable-advice 'load-file 'around 'pod-advice-load-file)
  (ad-enable-advice 'load 'around 'pod-advice-load)
  ;; disactivates..
  (ad-activate 'load)
  (ad-activate 'load-file))

;;;###autoload
(defun pod-load (file &rest args)
  "Kinda like load.."
  (let* ((fileel (concat file ".el"))
  (locfileel (locate-library fileel nil))
  (locfile (locate-library file nil)))
    (if locfileel
 (pod-load-file fileel)
      (pod-load-file file))))

;;;###autoload
(defun pod-load-file (file)
  (interactive "ffile: ")
  (let ((initbuf (buffer-name))
 (pod-current-file file)
 (pod-current-point (point-min))
 (pod-started-p t)
 (pod-current-depth (+ 1 pod-current-depth))
 )
    (find-file file)
    (setq pod-current-file file)
    (setq pod-current-point (point-min))
    (goto-char pod-current-point)
    (while
 (progn
   ;; doing this jazz coz. some user-expression may have caused
   ;; us to lose this filename...
   (find-file pod-current-file)
   (goto-char pod-current-point)

   (setq pod-current-point (scan-sexps pod-current-point 1))
   pod-current-point)
      (goto-char pod-current-point)
      (pod-wait-for-user)
      (save-excursion
 (pod-ignore-errors (pod-eval-last-sexp-stats pod-current-point))))
    (switch-to-buffer initbuf)
    (if (interactive-p)
 (message
  "(Now M-x pod-display-results) Done pod-loading file %s" file))))

(defun pod-eval-last-sexp-stats (&optional pt)
  (interactive)
  (unless pt (setq pt (point)))
  (save-excursion
    (goto-char (point))
    (let ((timm (pod-last-sexp-runtime pt)))
      (push
       (list
 timm
 pod-current-file
 pod-current-depth
 (pod-get-line pt)
 (pod-sexp-at-point))
       pod-results)
      (when (interactive-p) (message "%s" timm))))
  nil)

(defun pod-sexp-at-point ()
  (save-excursion
    (backward-sexp 1)
    (sexp-at-point)))

(defun pod-get-line (pt)
  (unless pt (setq pt (point)))
  (save-excursion
    (backward-sexp 1)
    (funcall pod-what-line-function)))

(defcustom pod-what-line-function 'what-line
  "")

;;;###autoload
(defun pod-last-sexp-runtime (&optional pt)
  (interactive)
  (unless pt (setq pt (point)))
  (let (ta tb tt)
    (setq ta (current-time))
    (eval-last-sexp nil)
    (setq tb (current-time))
    (setq tt (pod-time-diff tb ta))
    (when (interactive-p) (message "%S" tt))
    tt))

(defun pod-time-diff (tb ta)
  "The difference bet tb and ta, in milliseconds.. a float"
  (+
   (* 0.001 (- (caddr tb) (caddr ta)))
   (* 1000.0
      (+
       (- (second tb) (second ta))
       (* 65530.
   (- (car tb) (car ta)))))))

(defun pod-total-time (results)
  (let ((tot 0))
    (while results
      (if (= (cadr (cdar results)) 0)
   (setq tot (+ tot (caar results))))
      (pop results))
    tot))

(defun pod-display-results ()
  (interactive)
  ;;(setq pod-file-results nil)
  (let ((pod-copy (copy-sequence pod-results)))
    (with-electric-help
     '(lambda ()
 (setq pod-sorted-results
       (sort pod-copy
      #'(lambda (a b)
   (> (car a) (car b)))))
 (delete-region (point-min) (point-max))
 (goto-char (point-min))
 (insert "Load-time (0th level): "
  (format "%.2f"
   (pod-total-time
    pod-sorted-results))
  "    Net Number of sexps (all levels): "
  (format "%s" (length pod-sorted-results)))

 (let ((ctr 0) (ress pod-sorted-results))
   (while (and (<= ctr pod-max-display) ress)
     ;; can i use pop here??
     (funcall pod-display-one-result-function  (car
             ress))
     (setq ress (cdr ress))
     (incf ctr))))
     "*pod*")))

(defcustom pod-display-one-result-function 'pod-display-one-result "")

(defun pod-display-one-result (result)
  (require 'pp)
  ;;(let ((standard-output (current-buffer)))
  (insert "\n=======================================================\n")
  (insert (pod-limit-string ;;(pp-to-string
    (format "%S"
     (cons
      (format "%.2fms" (car result))
      (pod-subseq (cdr result) 0 3)))
    75)
   "\n  "
   (pod-limit-string (format "%S" (car (last result))) 70)))

(defun pod-limit-string (str len)
  (let ((str (replace-regexp-in-string "$^" " J" str) ))
    (if (> (length str) len)
 (concat (substring str 0 (- len 3)) " ..")
      str)))

(defcustom pod-wait-for-user-p nil "If t, will wait after eacxh instruction..")
(defun pod-wait-for-user ()
  (if pod-wait-for-user-p
      (while (not (input-pending-p))
 (message "Press a key")
 (sit-for 1)
 0)
    (discard-input)))

;;; 2002-06-19 T00:27:57-0400 (Wednesday)    D. Goel
;; this fcn lifted verbatim from cl-extra's subseq.
(defun pod-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
      (if (> start 0) (setq seq (nthcdr start seq)))
      (if end
   (let ((res nil))
     (while (>= (setq end (1- end)) start)
       (push (pop seq) res))
     (nreverse res))
        (copy-sequence seq)))
     (t
      (or end (setq end (or len (length seq))))
      (let ((res (make-vector (max (- end start) 0) nil))
     (i 0))
        (while (< start end)
   (aset res i (aref seq start))
   (setq i (1+ i) start (1+ start)))
        res))))))

(provide 'pod)
(run-hooks 'pod-after-load-hooks)

;;; pod.el ends here