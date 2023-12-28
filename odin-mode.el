;;; odin-mode.el --- A minor mode for odin

;; Author: Ethan Morgan
;; Keywords: odin, language, languages, mode
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/glassofethanol/odin-mode

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'js)

(defgroup odin nil
  "Odin mode"
  :group 'languages)

;; `compilation-mode' configuration

(eval-after-load 'compile
 '(add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\)(\\([0-9]+\\):\\([0-9]+\\).*" 1 2 3)))

(defconst odin-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?? "." table)

    ;; Need this for #directive regexes to work correctly
    (modify-syntax-entry ?#   "_" table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst odin-builtins
  '("len" "cap"
    "typeid_of" "type_info_of"
    "swizzle" "complex" "real" "imag" "quaternion" "conj"
    "jmag" "kmag"
    "min" "max" "abs" "clamp"
    "expand_to_tuple"

    "init_global_temporary_allocator"
    "copy" "pop" "unordered_remove" "ordered_remove" "clear" "reserve"
    "resize" "new" "new_clone" "free" "free_all" "delete" "make"
    "clear_map" "reserve_map" "delete_key" "append_elem" "append_elems"
    "append" "append_string" "clear_dynamic_array" "reserve_dynamic_array"
    "resize_dynamic_array" "incl_elem" "incl_elems" "incl_bit_set"
    "excl_elem" "excl_elems" "excl_bit_set" "incl" "excl" "card"
    "assert" "panic" "unimplemented" "unreachable"))

(defconst odin-keywords
  '("import" "foreign" "package"
    "where" "when" "if" "else" "for" "switch" "in" "notin" "do" "case"
    "break" "continue" "fallthrough" "defer" "return" "proc"
    "struct" "union" "enum" "bit_field" "bit_set" "map" "dynamic"
    "auto_cast" "cast" "transmute" "distinct" "opaque"
    "using" "inline" "no_inline"
    "size_of" "align_of" "offset_of" "type_of"

    "context"
    ;; "_"

    ;; Reserved
    "macro" "const"))

(defconst odin-constants
  '("nil" "true" "false"
    "ODIN_OS" "ODIN_ARCH" "ODIN_ENDIAN" "ODIN_VENDOR"
    "ODIN_VERSION" "ODIN_ROOT" "ODIN_DEBUG"))

(defconst odin-typenames
  '("bool" "b8" "b16" "b32" "b64"

    "int"  "i8" "i16" "i32" "i64"
    "i16le" "i32le" "i64le"
    "i16be" "i32be" "i64be"
    "i128" "u128"
    "i128le" "u128le"
    "i128be" "u128be"

    "uint" "u8" "u16" "u32" "u64"
    "u16le" "u32le" "u64le"
    "u16be" "u32be" "u64be"

    "f32" "f64"
    "complex64" "complex128"

    "quaternion128" "quaternion256"

    "rune"
    "string" "cstring"

    "uintptr" "rawptr"
    "typeid" "any"
    "byte"))

(defconst odin-attributes
  '("builtin"
    "export"
    "static"
    "deferred_in" "deferred_none" "deferred_out"
    "require_results"
    "default_calling_convention" "link_name" "link_prefix"
    "deprecated" "private" "thread_local"))


(defconst odin-proc-directives
  '("#force_inline"
    "#force_no_inline"
    "#type")
  "Directives that can appear before a proc declaration")

(defconst odin-directives
  (append '("#align" "#packed"
            "#any_int"
            "#raw_union"
            "#no_nil"
            "#complete"
            "#no_alias"
            "#c_vararg"
            "#assert"
            "#file" "#line" "#location" "#procedure" "#caller_location"
            "#load"
            "#defined"
            "#bounds_check" "#no_bounds_check"
            "#partial") odin-proc-directives))

(defun odin-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun odin-wrap-keyword-rx (s)
  (concat "\\(?:\\S.\\_<\\|\\`\\)" s "\\_>"))

(defun odin-wrap-directive-rx (s)
  (concat "\\_<" s "\\>"))

(defun odin-wrap-attribute-rx (s)
  (concat "[[:space:]\n]*@[[:space:]\n]*(?[[:space:]\n]*" s "\\>"))

(defun odin-keywords-rx (keywords)
  "build keyword regexp"
  (odin-wrap-keyword-rx (regexp-opt keywords t)))

(defun odin-directives-rx (directives)
  (odin-wrap-directive-rx (regexp-opt directives t)))

(defun odin-attributes-rx (attributes)
  (odin-wrap-attribute-rx (regexp-opt attributes t)))

(defconst odin-identifier-rx "[[:word:][:multibyte:]_]+")
(defconst odin-hat-type-rx (rx (group (and "^" (1+ (any word "." "_"))))))
(defconst odin-dollar-type-rx (rx (group "$" (or (1+ (any word "_")) (opt "$")))))
(defconst odin-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))
(defconst odin-proc-rx (concat "\\(\\_<" odin-identifier-rx "\\_>\\)\\s *::\\s *\\(" (odin-directives-rx odin-proc-directives) "\\)?\\s *\\_<proc\\_>"))

(defconst odin-type-rx (concat "\\_<\\(" odin-identifier-rx "\\)\\s *::\\s *\\(?:struct\\|enum\\|union\\|distinct\\)\\s *\\_>"))


(defconst odin-font-lock-defaults
  `(
    ;; Types
    (,odin-hat-type-rx 1 font-lock-type-face)
    (,odin-dollar-type-rx 1 font-lock-type-face)
    (,(odin-keywords-rx odin-typenames) 1 font-lock-type-face)
    (,odin-type-rx 1 font-lock-type-face)

    ;; Hash directives
    (,(odin-directives-rx odin-directives) 1 font-lock-preprocessor-face)

    ;; At directives
    (,(odin-attributes-rx odin-attributes) 1 font-lock-preprocessor-face)

    ;; Keywords
    (,(odin-keywords-rx odin-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("'\\(\\\\.\\|[^']\\)'" . font-lock-constant-face)

    ;; Variables
    (,(odin-keywords-rx odin-builtins) 1 font-lock-builtin-face)

    ;; Constants
    (,(odin-keywords-rx odin-constants) 1 font-lock-constant-face)

    ;; Strings
    ;; ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(odin-wrap-word-rx odin-number-rx) . font-lock-constant-face)

    ;; Procedures
    (,odin-proc-rx 1 font-lock-function-name-face)

    ("---" . font-lock-constant-face)
    ("\\.\\.<" . font-lock-constant-face)
    ("\\.\\." . font-lock-constant-face)
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
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local indent-line-function 'js-indent-line)
  (setq-local font-lock-defaults '(odin-font-lock-defaults))
  (setq-local beginning-of-defun-function 'odin-beginning-of-defun)
  (setq-local end-of-defun-function 'odin-end-of-defun)
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))
  (setq imenu-generic-expression
        `(("type" ,(concat "^" odin-type-rx) 1)
          ("proc" ,(concat "^" odin-proc-rx) 1)))

  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(provide 'odin-mode)


;;; odin-mode.el ends here
