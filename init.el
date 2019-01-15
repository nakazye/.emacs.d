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
(use-package color-theme-modern
  :config
  (load-theme 'euphoria t t)
  (enable-theme 'euphoria))

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
(use-package flycheck
  :init (global-flycheck-mode))

; flycheck-pos-tip
(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;===================================
; company-mode
(use-package company
  :config
  (global-company-mode +1)
  (push 'company-lsp company-backends)
  (global-set-key (kbd "C-M-i") 'company-complete)
  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  ;; TABで候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete))

; company-quickhelp
(use-package company-quickhelp
  :config
  (company-quickhelp-mode +1))

;===================================
; lsp-mode
(use-package lsp-mode
  :custom ((lsp-inhibit-message t)
         (lsp-message-project-root-warning t)
         (create-lockfiles nil))
  :hook   (prog-major-mode . lsp-prog-major-mode-enable))

; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :custom (scroll-margin 0)
  :hook   (lsp-mode . lsp-ui-mode))

; company-lsp
(use-package company-lsp
  :after (lsp-mode company yasnippet)
  :defines company-backends
  :functions company-backend-with-yas
  :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))

;===================================
; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  )

;===================================
; highlight-symbol
(use-package highlight-symbol
  :config
  (setq highlight-symbol-idle-delay 1.0) ; 1秒後自動ハイライト
  :hook
  (prog-mode . highlight-symbol-mode) ; 自動ハイライト
  (prog-mode . highlight-symbol-nav-mode) ; ソースコードでM-p/M-nシンボル間移動
  :bind
  (("M-s M-r" . highlight-symbol-query-replace)) ; シンボル置換
  )

;===================================
; multiple-coursors
(use-package multiple-cursors
  :config
  (setq mc/list-file "~/.emacs.d/var/mc-lists.el")
  :bind
  (("<C-M-return>" . mc/edit-lines)
   ("C-*" . mc/mark-all-like-this)))

;===================================
; python-mode
(use-package python-mode
  :config
  (setq py-python-command "python3")
  (setq python-shell-interpreter "python3")
  (add-hook 'python-mode-hook #'lsp))

;===================================
; adoc-mode
(use-package adoc-mode
  :mode
  (("\\.adoc?\\'" . adoc-mode)))
