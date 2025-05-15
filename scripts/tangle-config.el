;; Common settings for tangling org files
(require 'org)
(setq org-confirm-babel-evaluate nil)
(setq org-babel-tangle-create-missing-dirs-and-files t)

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