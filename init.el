;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;; init.el --- Emacs init setting elisp file

;########################################
; general settings
;########################################

;===================================
; debug on error
(setq debug-on-error t)

;===================================
; package manager: straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;===================================
; language setting
(set-language-environment 'Japanese)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(define-key global-map [?¥] [?\\])

;===================================
; IME setting
(when (eq window-system 'w32) ; windows
  (global-set-key [M-kanji] 'toggle-input-method) ; for us keyboard
  )
;; minibuffer に入った時、IME を OFF にする
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (deactivate-input-method)))
(add-hook 'helm-minibuffer-set-up-hook
          (lambda ()
            (deactivate-input-method)))

;===================================
; color-theme-modern
(use-package color-theme-modern)
(load-theme 'euphoria t t)
(enable-theme 'euphoria)

;===================================
; font setting
(add-to-list 'default-frame-alist '(font . "Myrica M-16"))

;===================================
; current directry setting
(cd "~/")

;===================================
; hide tool/menu bar and
(tool-bar-mode 0)
(menu-bar-mode 0)

;===================================
; ignore startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;===================================
; back space with C-h
(global-set-key "\M-?" 'help-for-help)
(global-set-key "\C-h" 'backward-delete-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;===================================
; paren mode
(show-paren-mode t)

;===================================
; mac meta seting
(when (eq window-system 'ns) ; mac
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super)))

;===================================
; line num and column num
(line-number-mode t)
(column-number-mode t)
(global-display-line-numbers-mode t)

;===================================
; tab as 4 sapaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;===================================
; indiscriminate upper and lower for search
(setq dabbrev-case-fold-search t)

;===================================
; change semanticdb dir
(setq semanticdb-default-save-directory
      "~/.emacs.d/var/semantic/")

;===================================
; change thumbs dir
(defcustom thumbs-thumbsdir (locate-user-emacs-file "var/thumbs")
  "Directory to store thumbnails."
  :type 'directory
  :group 'thumbs)

;===================================
; change backup dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/var/backup/")))
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-")

;########################################
; package settings
;########################################

;===================================
; flycheck
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;===================================
; flycheck-pos-tip
(use-package flycheck-pos-tip)
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;===================================
; auto-complete
(use-package auto-complete)
(ac-config-default)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
;(setq ac-auto-start nil) ; 自動表示の禁止
(defvar ac-use-menu-map t) ; use M-n/M-p
(defvar ac-use-fuzzy t)
(setq ac-dwim t)  ; 空気読んでほしい

;===================================
; git-gutter
(use-package git-gutter)
(global-git-gutter-mode +1)

;===================================
; highlight-symbol
(use-package highlight-symbol)
(setq highlight-symbol-idle-delay 1.0) ; 1秒後自動ハイライト
(add-hook 'prog-mode-hook 'highlight-symbol-mode) ; 自動ハイライト
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode) ; ソースコードでM-p/M-nシンボル間移動
(global-set-key (kbd "M-s M-r") 'highlight-symbol-query-replace) ; シンボル置換

;===================================
; adoc-mode
(use-package adoc-mode)
(autoload 'adoc-mode "adoc-mode" nil t)
