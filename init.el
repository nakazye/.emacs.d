;;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;; Emacs Startup File

;;; code:

(leaf font-setting
  :doc "font setting"
  :when (member "Myrica M" (font-family-list))
  :config
  (add-to-list 'default-frame-alist '(font . "Myrica M-16")))

;;; --------------------------------------

(leaf modus-themes
  :doc "theme setting"
  :ensure t
  :custom
  `((modus-themes-italic-constructs . t)
    (modus-themes-bold-constructs   . nil)
    (modus-themes-region            . '(bg-only no-extend)))
  :config
  (eval-when-compile
    (require 'modus-themes)))

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
  :tag "emacs>=24.4"
  :emacs>= 24.4
  :ensure t
  :config
  (which-key-mode)
  )

;;; --------------------------------------

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper))
    :config
    (leaf counsel
      :doc "Various completion functions using Ivy"
      :req "emacs-24.5" "swiper-0.13.0"
      :emacs>= 24.5
      :ensure t
      :blackout t
      :bind (("C-S-s" . counsel-imenu)
             ("C-x C-r" . counsel-recentf))
      :custom `((counsel-yank-pop-separator . "\n----------\n")
		(counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
      :global-minor-mode t)))

;;; --------------------------------------

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode
  :config
  (leaf ivy-prescient
    :doc "prescient.el + Ivy"
    :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
    :emacs>= 25.1
    :ensure t
    :after prescient ivy
    :custom ((ivy-prescient-retain-classic-highlighting . t))
    :global-minor-mode t))

;;; --------------------------------------

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

;;; --------------------------------------

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :emacs>= 24.3
  :ensure t
  :blackout t
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
	    :req "emacs-24.1" "company-0.8"
	    :tag "company" "development" "emacs>=24.1"
	    :added "2020-03-25"
	    :emacs>= 24.1
	    :ensure t
	    :after company
	    :defvar company-backends
	    :config
	    (add-to-list 'company-backends 'company-c-headers)))


(provide 'init)

;;; init.el ends here
