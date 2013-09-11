;;; Display an error if Emacs is older than 24.

(defconst nuclear-error-ascii-art ";;
;;                _.-^^---....,,--
;;            _--                  --_
;;           <                        >)
;;           |          ERROR          |
;;            \._                   _./
;;               ```--. . , ; .--'''
;;                     | |   |
;;                  .-=||  | |=-.
;;                  `-=#$%&%$#=-'
;;                     | ;  :|
;;            _____.,-#%&$@%#&#~,._____
;;
") ; Taken from Emacs Live <https://github.com/overtone/emacs-live/blob/master/init.el>.

(defconst nuclear-error-ascii-art-center-column 24
  "The center column of `nuclear-error-ascii-art'.")

(defun center-with-spaces (line center-column)
  "Pad LINE with spaces so that its center is in column CENTER-COLUMN."
  (concat
   (make-string (- center-column (/ (length line) 2)) ?\s)
   line))

(defun nuclear-error (&rest messages)
  "Display a nuclear explosion ASCII art with an error message in the scratch buffer (inhibit the startup message)."
  (setq inhibit-startup-message t)
  (setq initial-scratch-message
        (concat
         nuclear-error-ascii-art
         (mapconcat
          (lambda (message) ; Center the messages and prepend them with ";;".
            (concat ";;"
                    (center-with-spaces message
                                        nuclear-error-ascii-art-center-column)))
          messages
          "\n"))))

(when (version< emacs-version "24.3")
  (nuclear-error "This config requires Emacs 24.3+." (concat "Current version: " emacs-version "."))
  (error (concat "This config requires Emacs 24.3+. Current version: " emacs-version)))

(provide 'conf/init/check-version)
