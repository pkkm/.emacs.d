;;; Org-mode. -*- lexical-binding: t -*-

(use-package org
  :preface

  ;; Postponed until the release after 9.4.4 due to a bug: <https://www.mail-archive.com/emacs-orgmode@gnu.org/msg133604.html>.
  ;; Ensure that we're using an external org version instead of the built-in one. (One of the workarounds from <https://github.com/jwiegley/use-package/issues/319>.)
  ;; (unless (file-expand-wildcards
  ;;          (concat (file-name-as-directory package-user-dir) "org-[0-9]*"))
  ;;   (unless packages-refreshed-this-session-p ; Defined in `init.el'.
  ;;     (package-refresh-contents))
  ;;   (package-install (cadr (assoc 'org package-archive-contents)))) ; TODO Handle my pinned personal repo properly; check the link above for possible solutions.

  (setq org-export-backends '(ascii html icalendar latex odt md)) ; Default value (as of Org 9.1) with `md' added.

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
  (setq org-src-fontify-natively t) ; Syntax-highlight code blocks.
  (setq org-hide-leading-stars t) ; Deemphasize leading stars of headlines.
  (setq org-fontify-done-headline t) ; Mark the whole headline of a DONE task with a different face.
  (setq org-highlight-latex-and-related '(latex entities)) ; Highlight LaTeX fragments and symbols (e.g. \alpha).

  ;; Don't use additional indentation for code blocks.
  (setq org-edit-src-content-indentation 0)

  ;; Inline LaTeX formula rendering (Org recognizes "\(", "\[", etc.).
  ;; To render: C-c C-x C-l, to undo: C-c C-c. To render on startup: "#+STARTUP: latexpreview".
  ;; To change loaded packages, modify `org-latex-packages-alist' or `org-latex-default-packages-alist'.
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8)) ; Larger formulas.
  (when-let (dir (getenv "XDG_RUNTIME_DIR")) ; Store all previews in one place.
    (setq org-latex-preview-ltxpng-directory (expand-file-name "emacs-org-ltxpng/" dir)))

  ;; Ellipsis style for folded sections.
  (require 'conf/utils/colors) ; Used: color-mix.
  (defun set-org-ellipsis-style ()
    "Calculate the modeline backgrounds for various Evil states."
    (let* ((base-color "cyan")
           (color (color-mix base-color 0.4 (face-attribute 'default :foreground) 0.6))
           (box-color (color-mix base-color 0.15 (face-attribute 'default :background) 0.85)))
      (face-spec-set 'org-ellipsis
                     `((t (:foreground ,color :box (:line-width 1 :color ,box-color :style nil)))))))
  (add-hook 'after-load-theme-hook #'set-org-ellipsis-style)
  (set-org-ellipsis-style)
  (setq org-ellipsis
        (if (version< (org-version) "9.0") ; Backwards-incompatible change in Org.
            'org-ellipsis
          (propertize "..." 'face 'org-ellipsis)))

  ;; Logging.
  (setq org-log-repeat nil) ; Don't log shifting forward the date of a repeating task.

  ;; Completion.
  (setq org-completion-use-ido t)

  ;; Increase the depth of headlines shown in imenu.
  (setq org-imenu-depth 3)

  ;; Don't prepend "Function /" to top-level headlines in helm-imenu.
  ;; (The unwanted behavior is actually in `helm-imenu-transformer', but this way of disabling it is much less complex.)
  (defadvice helm-imenu--get-prop
      (after return-bare-item-in-org-mode (item) activate)
    (when (and (eq (buffer-local-value 'major-mode helm-current-buffer) 'org-mode)
               (null ad-return-value))
      (setq ad-return-value (list item))))

  ;; Function to list remote inline images. Useful when downloading images so that the document works without an Internet connection.
  (defun my-org-remote-inline-images-in-buffer ()
    "Display the URLs of HTTP and HTTPS inline images in the current buffer."
    ;; Inspired by <https://emacs.stackexchange.com/a/26638>.
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (and (member (org-element-property :type link) '("http" "https"))
                   (null (org-element-property :contents-begin link))
                   (--any (string-suffix-p (concat "." it)
                                           (org-element-property :raw-link link)
                                           t)
                          '("jpg" "jpeg" "png" "gif" "tiff" "bmp")))
          (org-element-property :raw-link link)))))

  ;; Variable to override timestamp format on export.
  ;; Example usage: -*- my-org-export-timestamp-formats: ("%Y-%m-%d" . "%Y-%m-%d %H:%M") -*-
  (defvar my-org-export-timestamp-formats nil
    "The value of `org-time-stamp-custom-formats' to use during export.")
  (make-variable-buffer-local 'my-org-export-timestamp-formats)
  (defadvice org-export-as (around my-org-export-timestamp-formats activate)
    (if my-org-export-timestamp-formats
        (let ((org-display-custom-times t)
              (org-time-stamp-custom-formats my-org-export-timestamp-formats))
          ad-do-it)
      ad-do-it))

  ;; TODO: after Org 9.2+ becomes bundled with Emacs, restore Easy Templates (e.g. "<q" -> "#+BEGIN_QUOTE").


  ;;; Automatic link descriptions.

  (use-package s :ensure t)

  (defun my-org-link-description (url &rest _)
    "Return link description for URL in the format I use in my notes."
    (require 's)
    (require 'dom)
    (let* ((url (replace-regexp-in-string ; Use old.reddit.com instead of reddit.com since the new version is a hard to parse JavaScript mess.
                 "^\\(https?://\\)\\(?:www\\.\\)?\\(reddit.com/r/.+\\)$" "\\1old.\\2" url))
           (html-buffer (url-retrieve-synchronously url))
           (dom (with-current-buffer html-buffer
                  (libxml-parse-html-region (point-min) (point-max) url t)))
           (title (s-trim (dom-text (car (dom-by-tag dom 'title)))))
           (parsed-url (url-generic-parse-url url))
           (host (url-host parsed-url))
           (domain-levels ; E.g. '("com" "ycombinator.com" "news.ycombinator.com")
            (nreverse (-reduce-r-from
                       (lambda (new-part accum)
                         (cons (if accum
                                   (concat new-part "." (car accum))
                                 new-part)
                               accum))
                       nil (s-split "\\." host))))
           (path (url-filename parsed-url))
           match-1 match-2)
      (cond

       ;; Reddit wiki.
       ((and (string-equal "reddit.com" (nth 1 domain-levels))
             (setq match-1 (s-match "^/r/\\([^/]+\\)/wiki\\(/\\|$\\)" path))
             (setq match-2 (s-match "^\\(.+\\) - \\([^ ]+\\)$" title)))
        (let ((wiki-page-title (nth 1 match-2))
              (subreddit (nth 2 match-2)))
          (concat "Reddit /r/" subreddit " wiki: " wiki-page-title)))

       ;; Reddit comment thread.
       ((and (string-equal "reddit.com" (nth 1 domain-levels))
             (setq match-1 (s-match "^/r/\\([^/]+\\)/comments/" path))
             (setq match-2 (s-match "^\\([^ ]+\\) comments on \\(.+\\)$" title)))
        (let ((subreddit (nth 1 match-1))
              (commenter (nth 1 match-2))
              (top-level-title (nth 2 match-2)))
          (concat "Reddit /r/" subreddit ": " commenter " on " top-level-title)))

       ;; Reddit top-level post.
       ((and (string-equal "reddit.com" (nth 1 domain-levels))
             (s-contains? " : " title))
        (cl-destructuring-bind (_ rest subreddit)
            (s-match "^\\(.*\\) : \\([^ :]+\\)$" title)
          (concat "Reddit /r/" subreddit ": " rest)))

       ;; Hacker News comment thread.
       ((and (string-equal "news.ycombinator.com" (nth 2 domain-levels))
         (setq match-1 (dom-by-tag (dom-by-class dom "storyon") 'a)))
        (let ((parent-title (dom-text (car match-1)))
              (user (dom-text (car (dom-by-class dom "hnuser")))))
          (concat "Hacker News: " user " on " parent-title)))

       ;; Less Wrong.
       ((string-equal "lesswrong.com" (nth 1 domain-levels))
        (let ((pure-title (s-chop-suffix " - LessWrong 2.0" title)))
          (if-let ((comment-id (nth 1 (s-match "#\\([a-zA-Z0-9]+\\)$" url))))
              ;; Get author from GreaterWrong (a LessWrong viewer that uses plain HTML rather than a huge JavaScript blob).
              (let* ((gw-url (replace-regexp-in-string
                              "\\(lesswrong.com\\).*\\'" "greaterwrong.com" url nil nil 1))
                     (gw-dom (with-current-buffer (url-retrieve-synchronously gw-url)
                               (libxml-parse-html-region (point-min) (point-max) url t)))
                     (comment-author (-> gw-dom
                                         (dom-by-id (regexp-quote comment-id))
                                         (dom-by-class "author")
                                         (car)
                                         (dom-text))))
                (concat "Less Wrong: " comment-author " on " pure-title))
            (concat "Less Wrong: " pure-title))))

       ;; Pages whose title probably contains the website's name.
       ((setq match-1 (s-match "^\\(.*\\) [-–|:•#·»←] \\(.*\\)$" title))
        (cl-destructuring-bind (_ first-part second-part) match-1
          (let* ((first-longer-p (>= (length first-part) (length second-part)))
                 (longer (if first-longer-p first-part second-part))
                 (shorter (if first-longer-p second-part first-part)))
            (concat shorter ": " longer))))

       ;; Others.
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


;;; Capturing.

(use-package org-capture
  :init

  (defun my-org-start-receiving-captures ()
    "Prepare for receiving remote captures with `org-protocol'."
    (interactive)

    (require 'server)
    (unless (server-running-p)
      (server-start))
    (require 'org-protocol)

    ;; Ensure that org-protocol is registered in the desktop environment.
    (when (eq window-system 'x)
      (let ((desktop-file (expand-file-name "~/.local/share/applications/org-protocol.desktop")))
        (unless (file-exists-p desktop-file)
          (with-temp-file desktop-file
            (insert "[Desktop Entry]\n"
                    "Name=org-protocol\n"
                    "Exec=emacsclient %u\n"
                    "Type=Application\n"
                    "Terminal=false\n"
                    "Categories=System;\n"
                    "MimeType=x-scheme-handler/org-protocol;\n"))
          (start-process
           "update-desktop-database" "*update-desktop-database*"
           "update-desktop-database" (expand-file-name "~/.local/share/applications"))))))

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

  ;; Capture template.
  ;; It would be useful to process the captured string and link (using my-org-link-description). This could be done with %-escapes inside %(sexp) expressions in `org-capture-templates', but they are handled with a string replacement rather than proper parsing so it would be buggy and a security risk (as of Org 9.1.1).
  ;; TODO submit an Org bug report. When it's fixed, finish writing the processing functionality (see commit 45c083f2516c066fd58af6b0261faeb1f1c29ea1 in this repo).
  (setq org-default-notes-file "~/Documents/Inbox/Inbox.org")
  (add-to-list 'org-capture-templates
               `("n" "Quote in org-default-notes-file" plain
                 (file "")
                 ,(concat "#+BEGIN_QUOTE\n"
                          "%:initial%?\n\n" ; %:initial -- initial content (alternative: %i); %? -- cursor position after inserting.
                          "-- %a [%<%Y-%m-%d>]\n" ; %a -- link with description.
                          "#+END_QUOTE")
                 :empty-lines 1)))

;; Process captured HTML with Pandoc.
;; See <https://github.com/alphapapa/org-protocol-capture-html> for the bookmarklet to use.
(use-package org-protocol-capture-html ; Installed in `my-vendor-dir' (not on MELPA as of 2019-09).
  :init
  (with-eval-after-load 'org-protocol
    (require 'org-protocol-capture-html)))


(use-package org-download
  :ensure t
  :init

  (with-eval-after-load 'org
    (org-download-enable)) ; Add handlers for drag-and-drop.

  :config

  ;; Don't add a "#+DOWNLOADED" annotation above the image.
  ;; TODO: feature request to avoid inserting "\n" when this is empty (maybe move the "\n" into this function).
  (setq org-download-annotate-function (lambda (&rest _) ""))

  ;; Use my custom directory structure for downloaded images (e.g. https://i.imgur.com/jiu2vmm.png -> ./assets/Programming.org/imgur.com/jiu2vmm.png).
  (defun org-download--fullname (link &optional ext)
    "Return the file name LINK will be saved to."
    (let* ((parsed-url (url-generic-parse-url link))
           (filename-from-url (file-name-nondirectory
                               (car (url-path-and-query parsed-url))))
           (filename ; If org-download provided a file extension, use it.
            (format "%s.%s"
                    (file-name-sans-extension filename-from-url)
                    (or ext (file-name-extension filename-from-url))))
           (dir-for-all-hosts ; Directory that contains per-host directories.
            (or (and (eq major-mode 'org-mode)
                     (not (local-variable-p 'org-download-image-dir))
                     (buffer-file-name)
                     (expand-file-name
                      (file-name-nondirectory (buffer-file-name))
                      "./assets"))
                org-download-image-dir))
           (host (url-host parsed-url))
           (dir ; Directory for this host.
            (expand-file-name
             (cond
              ((string= host "i.imgur.com") "imgur.com")
              (t host))
             dir-for-all-hosts))
           (use-file-dialog nil) ; Don't use a GUI dialog (less flexible).
           (result ; Let the user edit the name.
            (read-file-name "Save image as: "
                            (concat (expand-file-name dir) "/")
                            nil nil filename)))
      (make-directory (file-name-directory result) t)
      result))

  ;; Indent text inserted by org-download.
  ;; TODO: feature request for a customizable option for this.
  (defadvice org-download-insert-link (around my-org-download-indent activate)
    (add-hook 'after-change-functions #'my-indent-changed-region)
    (unwind-protect
        (progn ad-do-it)
      (remove-hook 'after-change-functions #'my-indent-changed-region)))
  (defun my-indent-changed-region (start end _)
    (indent-region start end)))

(provide 'conf/mode-specific/org)
