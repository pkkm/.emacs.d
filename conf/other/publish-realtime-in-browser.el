;;; Impatient Mode -- see the contents of a buffer in a browser, real-time.

;; `html-mode' and `web-mode' buffers are published verbatim (configurable in `imp-default-user-filters'; use `imp-toggle-htmlize' to toggle for the current buffer). If another file (e.g. CSS) is referenced in the HTML, you can enable `impatient-mode' in it to see the changes live.
;; Buffers in other modes are run through a filter, by default `htmlize' (to change this, see the README of `impatient-mode').

;; Usage:
;;   * Start the `simple-httpd' web server: M-x httpd-start
;;   * Publish buffers by enabling this minor mode: M-x impatient-mode
;;   * Browse to <http://localhost:8080/imp/>. (The default port is `httpd-port'.)

(use-package impatient-mode
  :ensure impatient-mode
  :defer t
  :config
  (diminish 'impatient-mode " Imp")) ; Default: " imp".

(provide 'conf/other/publish-realtime-in-browser)
