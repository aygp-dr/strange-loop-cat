;; Emacs Lisp script to tangle code blocks from an org file
;; Common tangling settings are now in scripts/tangle-config.el

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