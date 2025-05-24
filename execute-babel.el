;; Emacs Lisp script to execute code blocks in an org file
(require 'org)
(require 'ob-scheme)

;; Enable scheme for org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

;; Don't ask for confirmation
(setq org-confirm-babel-evaluate nil)

;; Open the file
(find-file "examples/test-babel.org")

;; Execute all code blocks
(org-babel-execute-buffer)

;; Show success message
(message "Execution of code blocks completed!")

;; Save any results
(save-buffer)