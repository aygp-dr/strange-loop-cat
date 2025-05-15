;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((fill-column . 80)))
 (org-mode . ((org-confirm-babel-evaluate . nil)
              (org-startup-with-inline-images . t)
              (org-babel-tangle-create-missing-dirs-and-files . t)  ;; Create dirs automatically
              (geiser-scheme-implementation . guile)
              (geiser-guile-binary . "guile3")
              (eval . (progn
                        (add-to-list 'org-babel-load-languages '(scheme . t))
                        (add-to-list 'org-babel-load-languages '(mermaid . t))
                        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))))
 (scheme-mode . ((scheme-program-name . "guile3"))))
