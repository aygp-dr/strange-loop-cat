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

;; Tangle the files from the project root
(let ((default-directory "/home/aygp-dr/projects/aygp-dr/strange-loop-cat/"))
  (message "Tangling examples/basics.org...")
  (org-babel-tangle-file "examples/basics.org")
  (message "Tangling examples/basics.org...done!")

  (message "Tangling examples/functors.org...")
  (org-babel-tangle-file "examples/functors.org")
  (message "Tangling examples/functors.org...done!")

  (message "Tangling examples/strange-loops.org...")
  (org-babel-tangle-file "examples/strange-loops.org")
  (message "Tangling examples/strange-loops.org...done!"))

;; Show success message
(message "Tangling of all files completed!")