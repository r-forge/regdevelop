(TeX-add-style-hook
 "pl-description"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("geometry" "a4paper" "text={14.5cm,22cm}")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "graphicx"
    "inputenc"
    "geometry"
    "color"
    "booktabs"
    "amsmath"
    "pldescr")
   (TeX-add-symbols
    "wh"
    "Gets")
   (LaTeX-add-labels
    "intro.plscale"
    "options.smooth"
    "options.dateaxis"))
 :latex)

