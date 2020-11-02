(require 'generic-x)

(define-generic-mode
  'lust-mode                          ;; name of the mode
  '(";")                           ;; comments delimiter
  '("fn" "macro" "if" "set" "let" "import")      ;; some keywords
  '(("car" . 'font-lock-function-name-face)
    ("cdr" . 'font-lock-function-name-face)
    ("cons" . 'font-lock-function-name-face)
    ("eval" . 'font-lock-function-name-face)
    ("macroexpand" . 'font-lock-function-name-face)
    ("println" . 'font-lock-function-name-face)
    ("negate" . 'font-lock-function-name-face)
    ("add" . 'font-lock-function-name-face)
    ("sub" . 'font-lock-function-name-face)
    ("mul" . 'font-lock-function-name-face)
    ("div" . 'font-lock-function-name-face)
    ("lt" . 'font-lock-function-name-face)
    ("gt" . 'font-lock-function-name-face)
    ("eq" . 'font-lock-function-name-face)
    ("#t" . 'font-lock-constant-face)
    ("#f" . 'font-lock-constant-face)
    ("'" . 'font-lock-preprocessor-face)
    ("quote" . 'font-lock-preprocessor-face)
    ("error" . 'font-lock-warning-face))
  '("\\.lust$")                    ;; files that trigger this mode
   nil                              ;; any other functions to call
  "Lust mode"
)

(provide 'lust-mode)
