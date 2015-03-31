;;; Make Emacs support /drive/* Cygwin paths.

(use-package cygwin-mount ; Installed in `my-vendor-dir'.
  :if (getenv "CYGWIN_ROOT") ; Set by Prepare.sh.
  :demand t
  :config

  ;; For init performance, we pretend that there are no mountpoins (apart from the one for Windows drives).
  ;; Normally, mountpoints would be detected by executing Cygwin's mount.exe.
  (setq cygwin-mount-table nil)
  (setq cygwin-mount-cygwin-bin-directory ; So that we know if a command attempts executing a Cygwin binary.
        "(Disabled for performance, see conf/opening-saving/cygwin.el)")
  ;; To re-enable mountpoint detection, change the above code to this:
  ;;   (setq cygwin-mount-cygwin-bin-directory
  ;;         (expand-file-name "bin" (getenv "CYGWIN_ROOT"))) ; CYGWIN_ROOT may need sanitizing if it begins with "<drive>://" instead of "<drive>:/".

  ;; Hardcode the location of drive mountpoints to /drive.
  ;; (I changed the location from the default /cygdrive; the relevant Cygwin settings are in Prepare.sh.)
  (defadvice cygwin-mount-get-cygdrive-prefix
    (around my-hardcoded-cygdrive-prefix activate)
    "Hardcode the location of drive mountpoints (detection doesn't work, always returns /cygdrive/)."
    (setq ad-return-value "/drive/"))

  (cygwin-mount-activate)) ; To deactivate, (cygwin-mount-deactivate).

(provide 'conf/opening-saving/cygwin)
