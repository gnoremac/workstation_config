;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up package.el and use-package for init
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        menu-bar-mode
        scroll-bar-mode))

(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)

(mapc (lambda (p) (push p load-path))
      '("~/.emacs.d/use-package/"
	"~/.emacs.d/gnoremac/"))

(require 'use-package)
(require 'package)

(defun depends--helper (deps body)
  (let ((dep (if (stringp (car deps)) (pop deps) (cons 'quote (list (pop deps))))))
    (list 'eval-after-load dep
          (cons 'lambda (cons nil (if (not deps)
                               body
                             (list (depends--helper deps body))))))))

(defmacro depends (&rest args)
  (declare (indent defun))
  (let ((dependencies nil))
    (while (or (stringp (car args))
              (symbolp (car args)))
      (push (pop args) dependencies))
    (depends--helper dependencies args)))

;;(require 'keys)

;; common lisp
(use-package cl-lib)

(dolist (p '(("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives p))

(when (and (member "--" command-line-args)
         (member "-refresh" command-line-args))
  (delete "-refresh" command-line-args)
  (package-refresh-contents))

(package-initialize)

(unless (package-installed-p 'scala-mode2)
(package-refresh-contents) (package-install 'scala-mode2))

(use-package ample-theme
  :ensure t)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package python
  :mode ("\\<SConstruct\\>$" . python-mode)
  :config (progn
            (use-package elpy
              :config (elpy-enable)
              :ensure t)))

(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config (progn
            (defun web-indirect-this-thing()
              (interactive)
              (let ((beg 0) (end 0))
                (save-excursion
                  (setq beg (progn (web-mode-forward-sexp -1)
                                   (call-interactively 'web-mode-tag-end)
                                   (point)))
                  (setq end (progn  (web-mode-forward-sexp 1)
                                    (point))))
                (indirect-region beg end))))
  :ensure t)

;(use-package sublimity
;  :if (GUI)
;  :config (use-package sublimity-scroll
;            :config (sublimity-global-mode t))
;  :ensure t)

(use-package multiple-cursors
  :config (progn (defun jorbi/mc/mark-until-line-change (&optional up)
                   (interactive "P")
                   (unless (save-excursion
                             (let ((col (current-column)))
                               (forward-line (if up -1 1))
                               (move-to-column col))
                             (looking-at "\\( +\\| *$\\)"))
                     (when up (next-line -1))
                     (mc/mark-next-lines 1)
                     (jorbi/mc/mark-until-line-change up)))

                 (push 'jorbi/mc/mark-until-line-change mc/cmds-to-run-once))

  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c C-m" . jorbi/mc/mark-until-line-change))
  :ensure t)

(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode)
  :ensure t)

(use-package s ;; string lib
  :defer t
  :ensure t)

(use-package dash ;; list lib
  :defer t
  :ensure t)

(use-package expand-region
  :bind ("C-c e" . er/expand-region)
  :ensure t)

(use-package w3m
  :defer t
  :ensure t)

(use-package gh
  :defer t
  :ensure t)

;;(use-package helm
;;  :defer t
;;  :bind ("C-c h" . helm-mini)
;;  :ensure t)

(use-package google-this
  :defer t
  :ensure t)

(use-package company
  :defer t
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package thrift
  :ensure t
  :defer t)

(use-package auto-complete
  :defer t
  :config (progn
            (require 'auto-complete-config)
            (depends "slime"
              (add-to-list 'ac-modes 'slime-repl-mode))
            (depends "js2-mode"
              (add-to-list 'ac-modes 'js2-mode))
            (depends "js-mode"
              (add-to-list 'ac-modes 'js-mode))
	    (depends "emacs-lisp-mode"
	      (add-to-list 'ac-modes 'emacs-lisp-mode))
            (depends "enh-ruby-mode"
              (add-to-list 'ac-modes 'enh-ruby-mode))
            (ac-config-default)
            (global-auto-complete-mode t))
  :ensure t)

(use-package powerline
  :ensure t)

(use-package gnoremac-powerline
  :config (depends "powerline" "cl"
            (setq-default mode-line-format jorbi/powerline-format)))

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package highlight-indentation
  :defer t
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init (setq js2-basic-offset 4)
  :config (progn
            (font-lock-add-keywords
             'js2-mode
             '(("\\(console\\)\\(\.\\)\\(log\\|trace\\)"
                (1 font-lock-warning-face t)
                (3 font-lock-warning-face t))))
            (use-package ac-js2
              :ensure t)

            (use-package js2-refactor
              :ensure t))
  :ensure t)
