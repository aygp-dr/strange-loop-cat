;; Emacs Lisp script to tangle code blocks from an org file
(require 'org)

;; Auto-create directories without prompting
(setq org-babel-tangle-create-missing-dirs-and-files t)

;; Don't ask for confirmation
(setq org-confirm-babel-evaluate nil)

;; Set default tangle directory
(setq org-babel-default-header-args
      (cons '(:tangle-dir . "../src/generated")
            (assq-delete-all :tangle-dir org-babel-default-header-args)))

;; Get the directory containing this script
(defvar script-dir (file-name-directory (or load-file-name buffer-file-name)))

;; Tangle all org files
(let ((default-directory script-dir))
  (dolist (org-file '("examples/basics.org" 
                      "examples/functors.org" 
                      "examples/strange-loops.org"
                      "examples/monads.org"
                      "examples/yoneda.org"
                      "examples/geb-formal-systems.org"
                      "examples/modular-systems-and-symmetry.org"))
    (when (file-exists-p org-file)
      (message (concat "Tangling " org-file "..."))
      (org-babel-tangle-file org-file)
      (message (concat "Tangling " org-file "...done!")))))

;; Show success message
(message "Tangling of all files completed!")