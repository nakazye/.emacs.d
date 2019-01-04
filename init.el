;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;; init.el -- Emacs init setting elisp file

;########################################
; general settings
;########################################

;===================================
; debug on error
(setq debug-on-error t)

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
; font setting
(add-to-list 'default-frame-alist '(font . "Myrica M"))

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

;########################################
; package settings
;########################################

;===================================
; color-theme-modern
(use-package color-theme-modern)
(load-theme 'euphoria t t)
(enable-theme 'euphoria)
