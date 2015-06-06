;;; Alternate ways of entering modified keys unavailable in terminals (e.g. C-}).

(bind-key "M-c" nil) ; Otherwise will be shadowed by `capitalize-word' (as of 2015-04).
(bind-key "M-c" #'event-apply-control-modifier function-key-map)

(provide 'conf/other/alternate-modifier-entering)
