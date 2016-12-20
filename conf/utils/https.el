;;; Test HTTPS verification. -*- lexical-binding: t -*-

(defun test-https-verification ()
  "Test Emacs' HTTPS verification by contacting a few sites. Signal an error if an invalid certificate is accepted."
  (interactive)
  (require 'url)
  (dolist (good-url '("https://github.com/"
                      "https://facebook.com/"))
    (kill-buffer (url-retrieve-synchronously good-url)))
  (dolist (bad-url '("https://self-signed.badssl.com/"
                     "https://wrong.host.badssl.com/"
                     "https://untrusted-root.badssl.com/"
                     "https://revoked.grc.com/"))
    (when (condition-case nil
              (kill-buffer (url-retrieve-synchronously bad-url))
            (error nil))
      (error "Bad HTTPS verification: no error on site with invalid cert: %s" bad-url)))
  (message "HTTPS verification OK"))

(provide 'conf/utils/https)
