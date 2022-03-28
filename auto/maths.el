(TeX-add-style-hook
 "maths"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("amsmath" "leqno")))
   (TeX-run-style-hooks
    "latex2e"
    "paper"
    "paper10"
    "inputenc"
    "fontenc"
    "dsfont"
    "amsmath"))
 :latex)

