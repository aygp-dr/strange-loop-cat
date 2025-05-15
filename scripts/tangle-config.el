;; Common settings for working with org files
(require 'org)
(require 'org-lint)

;; Default settings for org-mode
(setq org-confirm-babel-evaluate nil)
(setq org-babel-tangle-create-missing-dirs-and-files t)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-lint-initial-message nil)

;; Default babel header args
(setq org-babel-default-header-args
      '((:noweb . "yes")
        (:results . "output")
        (:exports . "both")
        (:eval . "yes")
        (:mkdirp . "yes")))

;; Function to tangle a file
(defun tangle-file (file)
  "Tangle a single org file"
  (find-file file)
  (org-babel-tangle)
  (kill-buffer))

;; Function to tangle a list of files
(defun tangle-files (files)
  "Tangle a list of org files"
  (dolist (file files)
    (message "Tangling %s..." file)
    (tangle-file file)))

;; Function to run org-lint on a file
(defun lint-file (file)
  "Run org-lint on a single org file"
  (find-file file)
  (let ((issues (org-lint)))
    (if issues
        (progn
          (message "Issues found in %s:" file)
          (dolist (issue issues)
            (message "%s" issue)))
      (message "No issues found in %s." file)))
  (kill-buffer))

;; Function to run org-lint on a list of files
(defun lint-files (files)
  "Run org-lint on a list of org files"
  (dolist (file files)
    (message "Linting %s..." file)
    (lint-file file)))

;; Function to detangle a file
(defun detangle-file (file)
  "Detangle a single org file (update code blocks from tangled files)"
  (find-file file)
  (org-babel-detangle)
  (save-buffer)
  (kill-buffer))

;; Function to detangle a list of files
(defun detangle-files (files)
  "Detangle a list of org files"
  (dolist (file files)
    (message "Detangling %s..." file)
    (detangle-file file)))