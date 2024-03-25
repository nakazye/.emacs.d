;;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;; Emacs Startup File

;;; code:

(leaf lang-setting
  :doc "langage setting"
  :config
  (set-language-environment  "Japanese")
  (prefer-coding-system  'utf-8)
  (set-file-name-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-default 'buffer-file-coding-system 'utf-8))

;;; --------------------------------------

(leaf font-setting
  :doc "font setting"
  :when (member "Myrica M" (font-family-list))
  :config
  (add-to-list 'default-frame-alist '(font . "Myrica M-16")))

;;; --------------------------------------

(leaf ddskk
  :ensure t
  :bind ("C-x j" . skk-mode)
  :custom
  (default-input-method . "japanese-skk")
  (skk-preload . t)
  (skk-jisyo-code . 'utf-8)
  (skk-server-host . "localhost")
  (skk-server-portnum . 1178)
  (skk-user-directory . "~/.emacs.d/ddskk")
  (skk-server-report-response . t)
  (skk-share-private-jisyo . t)
  (skk-inhibit-ja-dic-search . t)
  (skk-sticky-key . ";")
  (skk-egg-like-newline . t)
  (skk-verbose . t)
  (skk-delete-implies-kakutei . nil)
  (skk-use-numeric-conversion . t)
  (skk-japanese-message-and-error . t)
  (skk-auto-insert-paren . t)
  (skk-check-okurigana-on-touroku . t)
  (skk-show-tooltip . nil)
  (skk-isearch-start-mode . 'latin))

;;; --------------------------------------

(leaf modus-themes
  :doc "theme setting"
  :ensure t
  :init
  (load-theme 'modus-operandi :no-confirm)
  (modus-themes-toggle)
  :custom
  `((modus-themes-italic-constructs . t)
    (modus-themes-bold-constructs   . nil)
    (modus-themes-region            . '(bg-only no-extend)))
  )

;;; --------------------------------------

(leaf files
  :doc "editing file settings"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)
	    (auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

;;; --------------------------------------

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

;;; --------------------------------------

(leaf delsel
  :doc "delete selection if you insert"
  :global-minor-mode delete-selection-mode)

;;; --------------------------------------

(leaf which-key
  :doc "Display available keybindings in popup"
  :ensure t
  :config
  (which-key-mode)
  )

;;; --------------------------------------

(leaf vertico
  :tag "minibuffer UI"
  :ensure t
  :bind
  (:vertico-map
   ("C-r" . vertico-previous)
   ("C-s" . vertico-next))
  :custom (vertico-count . 10)
  :hook (after-init-hook . vertico-mode)
  :config
  (leaf consult
    :tag "completion command"
    :ensure t
    :package t
    :bind
    (
     ("C-s" . consult-line)
     ("C-x C-b" . switch-to-buffer)))
  (leaf orderless
    :tag "completion style"
    :ensure t
    :custom
    `((completion-styles . '(orderless))
      (orderless-matching-styles
       . '(
           ;; orderless-prefixes
           ;; orderless-flex
           orderless-regexp
           ;; orderless-initialism
	   ;; orderless-literal
	   ))))
  (leaf marginalia
    :tag "minibuffer annotations"
    :ensure t
    :hook
    (after-init-hook . marginalia-mode))
  )

;;; --------------------------------------

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

;;; --------------------------------------

(leaf company
  :doc "Modular text completion framework"
  :ensure t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode
  :config (leaf company-c-headers
	    :doc "Company mode backend for C/C++ header files"
	    :ensure t
	    :after company
	    :defvar company-backends
	    :config
	    (add-to-list 'company-backends 'company-c-headers)))

;;; --------------------------------------

(leaf org
  :doc "org-mode settings"
  :ensure t
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c j" . org-journal-new-entry))
  :setq
  (org-capture-templates
   . '(
       ("t" "Task w/o due"
	entry (file+headline "~/org/inbox.org" "Tasks") "** TODO %? \n")
       ("d" "Task w/ due"
	entry (file+headline "~/org/inbox.org" "Tasks") "** TODO %? \n   DEADLINE: %^t \n")
       ("m" "Memo"
	entry (file+headline "~/org/inbox.org" "Memo") "** %? \n")
       ))
  (org-agenda-files . '("~/org/"))
  :config
  (leaf org-journal
    :doc "org-journal settings"
    :ensure t
    :setq
    (org-journal-dir . "~/org/")
    (org-journal-file-type . 'monthly)
    (org-journal-date-format . "%Y-%m-%d, %A")
    (org-journal-file-format . "journal-%Y%m.org"))
  )

;;; --------------------------------------

(leaf lsp-java
  :ensure t
  :hook (java-mode-hook . (lambda () (lsp))))

;;; --------------------------------------

(provide 'init)

;;; init.el ends here
