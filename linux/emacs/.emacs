(add-to-list 'load-path "~/.emacs.d/vendors/auto-complete-1.3.1")
; Load the default configuration
(require 'auto-complete-config)
; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendors/auto-complete-1.3.1/dict")
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

(add-to-list 'load-path "~/.emacs.d/vendors/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

(add-to-list 'load-path "~/.emacs.d/vendors/lintnode")
(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
(setq lintnode-location "~/.emacs.d/vendors/lintnode")
;; JSLint can be... opinionated
(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))

(add-to-list 'load-path "~/.emacs.d/minor-modes")
;; Nice Flymake minibuffer messages
(require 'flymake-cursor)

(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))

(add-to-list 'load-path "~/.emacs.d/js-comint")
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "node")
 
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))
