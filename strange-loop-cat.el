;;; strange-loop-cat.el --- Project-specific Emacs configuration for Strange Loop Cat

;;; Commentary:
;; This configuration file sets up Emacs for interactive Scheme development
;; with Guile 3, Geiser, Org-mode, TRAMP, and Paredit support.

;;; Code:

;; Set up package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install required packages if not present
(defvar strange-loop-packages
  '(geiser
    geiser-guile
    paredit
    org
    rainbow-delimiters
    company))

(dolist (pkg strange-loop-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Project configuration
(defvar strange-loop-project-root
  (or (getenv "PROJECT_ROOT")
      (expand-file-name default-directory))
  "Root directory of the Strange Loop Cat project.")

(defvar strange-loop-project-name
  (or (getenv "PROJECT_NAME")
      "strange-loop-cat")
  "Name of the Strange Loop Cat project.")

;; Configure Geiser for Guile 3
(require 'geiser)
(require 'geiser-guile)
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
(setq geiser-repl-query-on-kill-p nil)
(setq geiser-repl-query-on-exit-p nil)

;; Set Guile load path
(setq geiser-guile-load-path
      (list (expand-file-name "src/guile" strange-loop-project-root)
            (expand-file-name "src/generated" strange-loop-project-root)))

;; Configure Scheme mode
(add-hook 'scheme-mode-hook 'geiser-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'company-mode)

;; Configure Paredit
(require 'paredit)
(define-key paredit-mode-map (kbd "M-s") nil) ; Free up M-s for search

;; Configure Org-mode for literate programming
(require 'org)
(require 'ob-scheme)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (shell . t)))

;; Don't ask for confirmation when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

;; Set default headers for Scheme code blocks
(setq org-babel-default-header-args:scheme
      '((:session . "geiser")
        (:results . "output")
        (:exports . "both")))

;; Configure TRAMP for remote editing
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-verbose 3) ; Increase for debugging

;; Project-specific key bindings
(global-set-key (kbd "C-c g r") 'geiser-mode-switch-to-repl)
(global-set-key (kbd "C-c g e") 'geiser-eval-definition)
(global-set-key (kbd "C-c g b") 'geiser-eval-buffer)
(global-set-key (kbd "C-c g l") 'geiser-load-file)
(global-set-key (kbd "C-c g d") 'geiser-doc-symbol-at-point)
(global-set-key (kbd "C-c g i") 'geiser-repl-import-module)

;; Org-mode key bindings for tangling
(global-set-key (kbd "C-c o t") 'org-babel-tangle)
(global-set-key (kbd "C-c o e") 'org-babel-execute-src-block)
(global-set-key (kbd "C-c o s") 'org-babel-execute-subtree)

;; Set up project file associations
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . scheme-mode))

;; Configure company-mode for better autocomplete
(require 'company)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(add-to-list 'company-backends 'geiser-company-backend)

;; Visual improvements
(setq show-paren-mode t)
(setq show-paren-style 'mixed)
(setq electric-pair-mode t)
(setq column-number-mode t)
(setq line-number-mode t)

;; Set default directory to project root
(setq default-directory strange-loop-project-root)

;; Custom functions for project navigation
(defun strange-loop-find-project-file ()
  "Find a file in the Strange Loop Cat project."
  (interactive)
  (let ((default-directory strange-loop-project-root))
    (call-interactively 'find-file)))

(defun strange-loop-run-tests ()
  "Run the project tests."
  (interactive)
  (let ((default-directory strange-loop-project-root))
    (compile "make test")))

(defun strange-loop-tangle-current-file ()
  "Tangle the current org file."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-babel-tangle)))

(defun strange-loop-start-repl ()
  "Start a Guile REPL with project settings."
  (interactive)
  (let ((geiser-guile-load-path
         (append geiser-guile-load-path
                 (list (expand-file-name "src/guile" strange-loop-project-root)
                       (expand-file-name "src/generated" strange-loop-project-root)))))
    (run-geiser 'guile)))

;; Project key bindings
(global-set-key (kbd "C-c p f") 'strange-loop-find-project-file)
(global-set-key (kbd "C-c p t") 'strange-loop-run-tests)
(global-set-key (kbd "C-c p T") 'strange-loop-tangle-current-file)
(global-set-key (kbd "C-c p r") 'strange-loop-start-repl)

;; Load dir-locals if they exist
(let ((dir-locals-file (expand-file-name ".dir-locals.el" strange-loop-project-root)))
  (when (file-exists-p dir-locals-file)
    (dir-locals-read-from-file dir-locals-file)))

;; Display startup message
(message "Strange Loop Cat Emacs configuration loaded!")
(message "Project root: %s" strange-loop-project-root)
(message "Use C-c p f to find project files, C-c p r to start REPL")

(provide 'strange-loop-cat)
;;; strange-loop-cat.el ends here