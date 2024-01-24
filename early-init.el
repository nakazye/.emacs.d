;;; early-init.el --- Early initialization file for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;; Emacs Startup File

;;; code:

;; ===================================
;;  debug on error
(setq debug-on-error t)

;; ===================================
;; silent
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (progn
    (set-message-beep 'silent)))
 ((string-equal system-type "gnu/linux") ; Linux
  (progn
    (setq visible-bell t)))
 ((string-equal system-type "darwin") ; mac
  (progn
    (setq visible-bell t))))
(setq ring-bell-function 'ignore)

;; ===================================
;; back space with C-h
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-?") 'help-for-help)

;; ===================================
;; current directry setting
(cd "~/")

;; ===================================
;; window setting
(setq initial-frame-alist
      (append (list
	       '(width . 150)
	       '(height . 60)
	       '(top . 10)
	       '(left . 10)
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; ===================================
;; package manager leaf.el setting
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init))
  )

;;; early-init.el ends here
