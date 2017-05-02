;;; Org-mode. -*- lexical-binding: t -*-

(use-package org
  :ensure t
  :config


  ;;; Keybindings.

  ;; Move/promote/demote headline.
  (bind-key "M-h" #'org-metaleft org-mode-map)
  (bind-key "M-j" #'org-metadown org-mode-map)
  (bind-key "M-k" #'org-metaup org-mode-map)
  (bind-key "M-l" #'org-metaright org-mode-map)

  ;; Move/promote/demote subtree.
  (bind-key "M-H" #'org-shiftmetaleft org-mode-map)
  (bind-key "M-J" #'org-shiftmetadown org-mode-map)
  (bind-key "M-K" #'org-shiftmetaup org-mode-map)
  (bind-key "M-L" #'org-shiftmetaright org-mode-map)

  ;; Set state/priority.
  (bind-key "C-M-h" #'org-shiftleft org-mode-map)
  (bind-key "C-M-j" #'org-shiftdown org-mode-map)
  (bind-key "C-M-k" #'org-shiftup org-mode-map)
  (bind-key "C-M-l" #'org-shiftright org-mode-map)

  ;; Mark element (to mark a subtree, use C-c @).
  (bind-key "M-v" #'org-mark-element org-mode-map) ; Normally would be M-h, but shadowed by previous binding.

  ;; Show all TODOs.
  (bind-key "C-c M-t" #'org-show-todo-tree org-mode-map)

  ;; Navigation (with conf/evil-specific/convenient-prefix-keys active):
  ;;   SPC u -- up heading.
  ;;   SPC f -- forward heading (same level).
  ;;   SPC b -- backward heading (same level).

  ;; Make RET also indent.
  (bind-key [remap org-return] #'org-return-indent org-mode-map)

  (with-eval-after-load 'evil
    ;; Insert heading.
    (evil-define-key 'normal org-mode-map (kbd "C-c RET") #'evil-org-insert-heading)

    ;; Replace the normal Evil end-of-line with an org-specific one.
    (evil-define-key 'motion org-mode-map [remap evil-end-of-line] #'org-end-of-line)

    (defun evil-org-insert-heading ()
      "Insert a heading in Org-Mode and switch to Evil's insert state."
      (interactive)
      (org-end-of-line)
      (call-interactively #'org-insert-heading)
      (unless (evil-insert-state-p)
        (evil-append 1))))


  ;;; Other.

  ;; Display.
  (setq org-startup-truncated nil) ; Wrap long lines instead of truncating them (toggle with `toggle-truncate-lines').
  (setq org-startup-folded nil) ; Start with all headlines expanded.
  (setq org-src-fontify-natively t) ; Fontify code blocks.
  (setq org-hide-leading-stars t) ; Make leading stars of headlines background color.
  (setq org-fontify-done-headline t) ; Mark the whole headline of a DONE task with a different face.

  ;; Indent guides.
  ;; We don't use `with-eval-after-load' because `indent-guide-mode' is autoloaded, so it would never load.
  (require 'conf/view/indent-guides)
  (add-hook 'org-mode-hook #'indent-guide-mode)

  ;; Inline LaTeX formula rendering (Org recognizes "\(", "\[", etc.).
  ;; To render: C-c C-x C-l, to undo: C-c C-c. To render on startup: "#+STARTUP: latexpreview".
  (plist-put org-format-latex-options :scale 1.8) ; Larger formulas.

  ;; Ellipsis style for folded sections.
  (require 'conf/utils/colors) ; Used: color-mix.
  (setq org-ellipsis 'org-ellipsis)
  (defun set-org-ellipsis-style ()
    "Calculate the modeline backgrounds for various Evil states."
    (let* ((base-color "cyan")
           (color (color-mix base-color 0.4 (face-attribute 'default :foreground) 0.6))
           (box-color (color-mix base-color 0.15 (face-attribute 'default :background) 0.85)))
      (face-spec-set 'org-ellipsis
                     `((t (:foreground ,color :box (:line-width 1 :color ,box-color :style nil)))))))
  (add-hook 'after-load-theme-hook #'set-org-ellipsis-style)
  (set-org-ellipsis-style)

  ;; Logging.
  (setq org-log-repeat nil) ; Don't log shifting forward the date of a repeating task.

  ;; Completion.
  (setq org-completion-use-ido t)


  ;;; Automatic link descriptions.

  (use-package s :ensure t)

  (autoload 'mm-url-decode-entities-string "mm-url")
  (defun get-url-html-title (url &rest _)
    "Return the title of the HTML page at URL."
    (require 's)
    (let ((download-buffer (url-retrieve-synchronously url))
          title-start title-end)
      (save-excursion
        (set-buffer download-buffer)
        (beginning-of-buffer)
        (setq title-start (search-forward "<title>"))
        (search-forward "</title>")
        (setq title-end (search-backward "<"))
        (s-trim
         (s-collapse-whitespace
          (mm-url-decode-entities-string
           (buffer-substring-no-properties title-start title-end)))))))

  (defun my-org-link-description (url &rest _)
    "Return link description for URL in the format I use in my notes."
    (require 's)
    (let ((title (get-url-html-title url))
          (host (url-host (url-generic-parse-url url)))
          match)
      (cond
       ((and (s-matches? "\\(.+\\.\\)?reddit.com" host)
             (s-contains? " : " title))
        (cl-destructuring-bind (_ rest subreddit)
            (s-match "\\(.*\\) : \\([^ :]+\\)" title)
          (concat "Reddit /r/" subreddit ": " rest)))
       ((setq match (s-match "\\(.*\\) - \\(.*\\)" title))
        (cl-destructuring-bind (_ first-part second-part) match
          (let* ((first-longer-p (>= (length first-part) (length second-part)))
                 (longer (if first-longer-p first-part second-part))
                 (shorter (if first-longer-p second-part first-part)))
            (concat shorter ": " longer))))
       (t title))))

  (defun my-org-toggle-auto-link-description ()
    "Toggle automatically downloading link descriptions."
    (interactive)
    (if org-make-link-description-function
        (progn
          (setq org-make-link-description-function nil)
          (message "Automatic link description downloading disabled."))
      (setq org-make-link-description-function #'get-url-html-title)
      (message "Automatic link description downloading enabled.")))

  (defun my-org-insert-link ()
    "Insert link in the format I use in my notes."
    (interactive)
    (let ((org-make-link-description-function #'my-org-link-description))
      (call-interactively #'org-insert-link))
    (insert (format-time-string " [%Y-%m-%d]")))
  (bind-key "C-c M-l" #'my-org-insert-link org-mode-map))


(use-package org-capture
  :init

  (defun my-org-start-receiving-captures ()
    "Prepare for receiving remote captures with `org-protocol'."
    (interactive)
    (require 'server)
    (unless (server-running-p)
      (server-start))
    (require 'org-protocol))

  :config

  (defun my-org-refile-target-files ()
    "Return a list of Org files that I use."
    (mapcan
     (lambda (directory)
       (f-files directory
                (lambda (path) (string-suffix-p ".org" path))
                t))
     '("~/Documents" "~/University")))
  (add-to-list 'org-refile-targets
               (cons #'my-org-refile-target-files '(:maxlevel . 3)))

  (setq org-default-notes-file "~/Documents/Inbox.org")
  (add-to-list 'org-capture-templates
               `("n" "Quote in org-default-notes-file" plain
                 (file "")
                 ,(concat "#+BEGIN_QUOTE\n"
                          "%i%?\n\n" ; %i -- initial content (see also %x -- X clipboard content); %? -- cursor position after inserting.
                          "-- %a [%<%Y-%m-%d>]\n" ; %a -- link with description.
                          "#+END_QUOTE")
                 :empty-lines 1)))

(provide 'conf/mode-specific/org)
