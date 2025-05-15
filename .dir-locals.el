;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((fill-column . 80)))
 (org-mode . ((org-confirm-babel-evaluate . nil)
              (org-startup-with-inline-images . t)
              (org-babel-tangle-create-missing-dirs-and-files . t)  ;; Create dirs automatically
              (org-default-notes-file . "notes.org")
              (org-lint-initial-message . nil)  ;; Suppress initial message
              (org-export-with-toc . 3)  ;; Include TOC in exports with depth 3
              (org-src-preserve-indentation . t)  ;; Preserve code indentation
              (org-src-fontify-natively . t)  ;; Syntax highlighting in code blocks
              (org-src-tab-acts-natively . t)  ;; Tab works properly in code blocks
              (org-edit-src-content-indentation . 0)  ;; Don't add indentation when editing
              (org-babel-default-header-args . ((:noweb . "yes")
                                              (:results . "output")
                                              (:exports . "both")
                                              (:eval . "yes")
                                              (:mkdirp . "yes")))
              (geiser-scheme-implementation . guile)
              (geiser-guile-binary . "guile3")
              (eval . (progn
                        (add-to-list 'org-babel-load-languages '(scheme . t))
                        (add-to-list 'org-babel-load-languages '(mermaid . t))
                        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))))
 (scheme-mode . ((scheme-program-name . "guile3"))))
