;; setup.el - Simple setup for strange-loop-cat project
;; Load this file with M-x load-file

;; Basic functions for project
(defun slc-tangle-all ()
  "Tangle all Org files in the project."
  (interactive)
  (let ((files (directory-files-recursively 
                default-directory 
                "\\.org$")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (message "Tangling %s..." file)
        (org-babel-tangle)
        (kill-buffer)))))

(defun slc-build-mermaid ()
  "Build all Mermaid diagrams."
  (interactive)
  (compile "make mermaid"))

(defun slc-run-tests ()
  "Run all tests."
  (interactive)
  (compile "make test"))

(defun slc-preview-html ()
  "Preview current Org file as HTML."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-html-export-to-html)
    (browse-url (concat (file-name-sans-extension buffer-file-name) ".html"))))

;; Set up org-babel
(when (fboundp 'org-babel-do-load-languages)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp . t)
     (shell . t)
     (python . t))))

;; No need for confirmation
(setq org-confirm-babel-evaluate nil)

;; Set scheme program
(setq scheme-program-name "guile")

;; Print success message
(message "Project setup complete. Use M-x slc-* commands to work with the project.")
