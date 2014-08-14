;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;; TODO: Maybe, use this after emacs24 is released
                  ;; (development versions of packages)
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;;; Required packages
;;; everytime emacs starts, it will automatically check if those packages are
;;; missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar gnoremac/packages
  '(js2-mode ac-js2 yasnippet web-mode))
(dolist (p gnoremac/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; yasnippet
;;(require 'yasnippet)
;;(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;;(ac-set-trigger-key "TAB")
;;(ac-set-trigger-key "<tab>")

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; to setup tabs
(setq c-basic-indent 2)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; enable js2-mode for javascript
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)


;; Oh, and here's a cute hack you might want to put in the sample .emacs
;; file: it changes the color of the window if it's not on the local
;; machine, or if it's running as root:

;; local emacs background:  whitesmoke
;; remote emacs background: palegreen1
;; root emacs background:   coral2
(cond
 ((and (string-match "XEmacs" emacs-version)
       (eq window-system 'x)
       (boundp 'emacs-major-version)
       (= emacs-major-version 19)
       (>= emacs-minor-version 12))
  (let* ((root-p (eq 0 (user-uid)))
         (dpy (or (getenv "DISPLAY") ""))
         (remote-p (not
                    (or (string-match "^\\(\\|unix\\|localhost\\):" dpy)
                        (let ((s (system-name)))
                          (if (string-match "\\.\\(netscape\\|mcom\\)\\.com" s)
                              (setq s (substring s 0 (match-beginning 0))))
                          (string-match (concat "^" (regexp-quote s)) dpy)))))
         (bg (cond (root-p "coral2")
                   (remote-p "palegreen1")
                   (t nil))))
    (cond (bg
           (let ((def (color-name (face-background 'default)))
                 (faces (face-list)))
             (while faces
               (let ((obg (face-background (car faces))))
                 (if (and obg (equal def (color-name obg)))
                     (set-face-background (car faces) bg)))
               (setq faces (cdr faces)))))))))
