(TeX-add-style-hook
 "ia_pas_de_soucis"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "french") ("inputenc" "utf8")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "babel"
    "listings"
    "hyperref"
    "fancyhdr"
    "fancybox"
    "amsmath"
    "amssymb"
    "xcolor"
    "changepage"
    "array"
    "placeins"
    "float"
    "verbatim"
    "qtree"
    "forest"
    "inputenc"
    "listingsutf8")
   (TeX-add-symbols
    '("pather" 1)
    "ffi"
    "imply")
   (LaTeX-add-labels
    "neg_table"
    "and_table"
    "or_table"
    "xor_table"
    "imply_table"
    "iff_table")
   (LaTeX-add-listings-lstdefinestyles
    "mystyle")
   (LaTeX-add-xcolor-definecolors
    "codecomments"
    "codenumbers"
    "codestrings"
    "keycolor"
    "backcolour")
   (LaTeX-add-array-newcolumntypes
    "M"))
 :latex)

