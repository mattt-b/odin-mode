;; odin-mode.el - A minor mode for odin

(require 'cl)
(require 'rx)
(require 'js)

(defconst odin-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst odin-builtins
  '("cast" "proc" "new" "package" "make" "map" "typeid" "type_info_of"
    "auto_cast" "bit_set" "union" "len" "distict" "dynamic" "delete" "type_of" "alloc"))

(defconst odin-keywords
  '("if" "else" "do" "when" "while" "for" "switch" "case" "struct" "enum"
    "return" "remove" "continue" "fallthrough" "break" "defer" "inline" "proc"
    "import" "using" "#assert" "#no_bounds_check" "#align" "#packed" "#raw_union"
    "foreign" "cast" "proc" "new" "package" "make" "map" "typeid" "type_info_of"
    "auto_cast" "bit_set" "union" "len" "distict" "dynamic" "delete" "type_of" "alloc"))

(defconst odin-constants
  '("null" "true" "false"))

(defconst odin-typenames
  '("int" "uint" "u64" "u32" "u16" "u8"
    "s64" "s32" "s16" "s8" "float"
    "float32" "float64" "string"
    "complex64" "complex128" "uintptr"
    "bool" "b8" "b16" "b32" "b64"
    "rune" "rawptr" "any" "cstring"
    "^" "&" "$" "@"))

(defun odin-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun odin-keywords-rx (keywords)
  "build keyword regexp"
  (odin-wrap-word-rx (regexp-opt keywords t)))

(defconst odin-hat-type-rx (rx (group (and "^" (1+ word)))))
(defconst odin-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst odin-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defconst odin-font-lock-defaults
  `(
    ;; Keywords
    (,(odin-keywords-rx odin-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(odin-keywords-rx odin-builtins) 1 font-lock-variable-name-face)

    ;; Constants
    (,(odin-keywords-rx odin-constants) 1 font-lock-constant-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; At directives
    ("@\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(odin-wrap-word-rx odin-number-rx) . font-lock-constant-face)

    ;; Types
    (,(odin-keywords-rx odin-typenames) 1 font-lock-type-face)
    (,odin-hat-type-rx 1 font-lock-type-face)
    (,odin-dollar-type-rx 1 font-lock-type-face)

    ("---" . font-lock-constant-face)
    ))

;; add setq-local for older emacs versions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defconst odin--defun-rx "\(.*\).*\{")

(defmacro odin-paren-level ()
  `(car (syntax-ppss)))

(defun odin-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at odin--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun odin-beginning-of-defun (&optional count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (odin-paren-level)))
    (while (and
            (not (odin-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (odin-paren-level))
      (while (>= (odin-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (odin-line-is-defun)
      (beginning-of-line)))

(defun odin-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (odin-paren-level)))
    (when (> orig-level 0)
      (odin-beginning-of-defun)
      (end-of-line)
      (setq orig-level (odin-paren-level))
      (skip-chars-forward "^}")
      (while (>= (odin-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'odin-parent-mode
 (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode odin-mode odin-parent-mode "Odin"
 :syntax-table odin-mode-syntax-table
 :group 'odin
 (setq bidi-paragraph-direction 'left-to-right)
 (setq-local require-final-newline mode-require-final-newline)
 (setq-local parse-sexp-ignore-comments t)
 (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
 (setq-local comment-start "/*")
 (setq-local comment-end "*/")
 (setq-local indent-line-function 'js-indent-line)
 (setq-local font-lock-defaults '(odin-font-lock-defaults))
 (setq-local beginning-of-defun-function 'odin-beginning-of-defun)
 (setq-local end-of-defun-function 'odin-end-of-defun)

 (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(provide 'odin-mode)


;;; odin-mode.el ends here
