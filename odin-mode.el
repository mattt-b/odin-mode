;;; odin-mode.el --- A minor mode for odin. -*-lexical-binding: t-*-

;; Author: Ethan Morgan
;; Keywords: odin, language, languages, mode
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/glassofethanol/odin-mode

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'js)
(require 'treesit)

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
    "where" "when" "if" "else" "for" "switch" "in" "not_in" "do" "case"
    "break" "continue" "fallthrough" "defer" "return" "proc"
    "struct" "union" "enum" "bit_field" "bit_set" "map" "dynamic"
    "auto_cast" "cast" "transmute" "distinct" "opaque"
    "using" "inline" "no_inline"
    "size_of" "align_of" "offset_of" "type_of"
    "or_return" "or_else" "or_continue" "or_break"

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

    "f16" "f32" "f64"
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
    "init"
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
            "#optional_ok"
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

;; todo: is an optional count argument needed. If so for what?
(defun odin-beginning-of-defun ()
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

;;; Tree-sitter support

(defcustom odin-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `odin-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'odin)


(defvar odin-ts-mode--indent-rules
  `((odin
     ((parent-is "compilation_unit") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "namespace_declaration") parent-bol 0)
     ((parent-is "class_declaration") parent-bol 0)
     ((parent-is "constructor_declaration") parent-bol 0)
     ((parent-is "initializer_expression") parent-bol odin-ts-mode-indent-offset)
     ((match "{" "anonymous_object_creation_expression") parent-bol 0)
     ((parent-is "anonymous_object_creation_expression") parent-bol odin-ts-mode-indent-offset)
     ((match "{" "object_creation_expression") parent-bol 0)
     ((parent-is "object_creation_expression") parent-bol 0)
     ((parent-is "method_declaration") parent-bol 0)
     ((parent-is "enum_declaration") parent-bol 0)
     ((parent-is "operator_declaration") parent-bol 0)
     ((parent-is "field_declaration") parent-bol 0)
     ((parent-is "struct_declaration") parent-bol 0)
     ((parent-is "declaration_list") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "argument_list") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "interpolation") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent 0)
     ((parent-is "block") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "local_function_statement") parent-bol 0)
     ((parent-is "if_statement") parent-bol 0)
     ((parent-is "for_statement") parent-bol 0)
     ((parent-is "for_each_statement") parent-bol 0)
     ((parent-is "while_statement") parent-bol 0)
     ((match "{" "switch_expression") parent-bol 0)
     ((parent-is "switch_statement") parent-bol 0)
     ((parent-is "switch_body") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "switch_section") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "switch_expression") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "case_statement") parent-bol 0)
     ((parent-is "do_statement") parent-bol 0)
     ((parent-is "equals_value_clause") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "ternary_expression") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "conditional_expression") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "statement_block") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "type_arguments") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "array") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "formal_parameters") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "template_substitution") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "object_pattern") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "object") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "object_type") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "arrow_function") parent-bol odin-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol odin-ts-mode-indent-offset))))


(defun odin-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "method_declaration"
         "record_declaration"
         "struct_declaration"
         "enum_declaration"
         "interface_declaration"
         "class_declaration")
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))))



(defconst odin-ts-mode--builtins
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

(defconst odin-ts-mode--keywords
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

(defconst odin-ts-mode--constants
  '("nil" "true" "false"
    "ODIN-TS-MODE-_OS" "ODIN-TS-MODE-_ARCH" "ODIN-TS-MODE-_ENDIAN" "ODIN-TS-MODE-_VENDOR"
    "ODIN-TS-MODE-_VERSION" "ODIN-TS-MODE-_ROOT" "ODIN-TS-MODE-_DEBUG"))

(defconst odin-ts-mode--typenames
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

(defconst odin-ts-mode--attributes
  '("builtin"
    "export"
    "static"
    "deferred_in" "deferred_none" "deferred_out"
    "require_results"
    "default_calling_convention" "link_name" "link_prefix"
    "deprecated" "private" "thread_local"))


(defvar odin-ts-mode--operators
  '("!"  "!=" "%" "%=" "&" "&=" "&&" "*" "*=" "+" "+=" "," "-" "-="
    "->" "."  ".."  "..=" "..."  "/" "/=" ":" ";" "<<" "<<=" "<" "<="
    "=" "==" "=>" ">" ">=" ">>" ">>=" "@" "^" "^=" "|" "|=" "||" "?")

  "Rust operators for tree-sitter font-locking.")
(defun odin-ts-mode--iota-query-supported-p ()
  "Return t if the iota query is supported by the tree-sitter-go grammar."
  (ignore-errors
    (or (treesit-query-string "" '((iota) @font-lock-constant-face) 'go) t)))


(defvar odin-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'odin
   :feature 'attribute
   '((attribute_item) @font-lock-preprocessor-face
     (inner_attribute_item) @font-lock-preprocessor-face)

   :language 'odin
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'odin
   :feature 'comment
   '(([(block_comment) (line_comment)]) @font-lock-comment-face)

   :language 'odin
   :feature 'delimiter
   '((["," "." ";" ":" "::"]) @font-lock-delimiter-face)

   :language 'odin
   :feature 'definition
   '((function_item name: (identifier) @font-lock-function-name-face)
     (macro_definition "macro_rules!" @font-lock-constant-face)
     (macro_definition (identifier) @font-lock-preprocessor-face)
     (field_declaration name: (field_identifier) @font-lock-property-name-face)
     (parameter pattern: (_) @odin-ts-mode--fontify-pattern)
     (closure_parameters (_) @odin-ts-mode--fontify-pattern)
     (let_declaration pattern: (_) @odin-ts-mode--fontify-pattern)
     (for_expression pattern: (_) @odin-ts-mode--fontify-pattern)
     (let_condition pattern: (_) @odin-ts-mode--fontify-pattern)
     (match_arm pattern: (_) @odin-ts-mode--fontify-pattern))

   :language 'odin
   :feature 'assignment
   '((assignment_expression left: (_) @odin-ts-mode--fontify-pattern)
     (compound_assignment_expr left: (_) @odin-ts-mode--fontify-pattern))

   :language 'odin
   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-call-face
       (field_expression
        field: (field_identifier) @font-lock-function-call-face)
       (scoped_identifier
        name: (identifier) @font-lock-function-call-face)])
     (generic_function
      function: [(identifier) @font-lock-function-call-face
                 (field_expression
                  field: (field_identifier) @font-lock-function-call-face)
                 (scoped_identifier
                  name: (identifier) @font-lock-function-call-face)])
     (macro_invocation macro: (identifier) @font-lock-preprocessor-face))

   :language 'odin
   :feature 'keyword
   `([,@odin-ts-mode--keywords] @font-lock-keyword-face)

   :language 'odin
   :feature 'number
   '([(float_literal) (integer_literal)] @font-lock-number-face)

   :language 'odin
   :feature 'operator
   `([,@odin-ts-mode--operators] @font-lock-operator-face)

   :language 'odin
   :feature 'string
   '([(char_literal)
      (raw_string_literal)
      (string_literal)] @font-lock-string-face)

   :language 'odin
   :feature 'type
   `((scoped_use_list path: (identifier) @font-lock-constant-face)
     (scoped_use_list path: (scoped_identifier
                             name: (identifier) @font-lock-constant-face))
     ((use_as_clause alias: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))
     ((use_as_clause path: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))
     ((use_list (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))
     (use_wildcard [(identifier) @odin-ts-mode--fontify-scope
                    (scoped_identifier
                     name: (identifier) @odin-ts-mode--fontify-scope)])
     (enum_variant name: (identifier) @font-lock-type-face)
     (match_arm
      pattern: (match_pattern (_ type: (identifier) @font-lock-type-face)))
     (match_arm
      pattern: (match_pattern
                (_ type: (scoped_identifier
                          path: (identifier) @font-lock-type-face))))
     (mod_item name: (identifier) @font-lock-constant-face)
     (primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     ((scoped_identifier name: (identifier) @odin-ts-mode--fontify-tail))
     ((scoped_identifier path: (identifier) @font-lock-type-face)
      (:match
       "\\`\\(u8\\|u16\\|u32\\|u64\\|u128\\|usize\\|i8\\|i16\\|i32\\|i64\\|i128\\|isize\\|char\\|str\\)\\'"
       @font-lock-type-face))
     ((scoped_identifier path: (identifier) @odin-ts-mode--fontify-scope))
     ((scoped_type_identifier path: (identifier) @odin-ts-mode--fontify-scope))
     (type_identifier) @font-lock-type-face)

   :language 'odin
   :feature 'property
   '((field_identifier) @font-lock-property-use-face
     (shorthand_field_initializer (identifier) @font-lock-property-use-face))

   ;; Must be under type, otherwise some imports can be highlighted as constants.
   :language 'odin
   :feature 'constant
   `((boolean_literal) @font-lock-constant-face
     ((identifier) @font-lock-constant-face
      (:match "\\`[A-Z][A-Z\\d_]*\\'" @font-lock-constant-face)))

   :language 'odin
   :feature 'variable
   '((arguments (identifier) @font-lock-variable-use-face)
     (array_expression (identifier) @font-lock-variable-use-face)
     (assignment_expression right: (identifier) @font-lock-variable-use-face)
     (binary_expression left: (identifier) @font-lock-variable-use-face)
     (binary_expression right: (identifier) @font-lock-variable-use-face)
     (block (identifier) @font-lock-variable-use-face)
     (compound_assignment_expr right: (identifier) @font-lock-variable-use-face)
     (field_expression value: (identifier) @font-lock-variable-use-face)
     (field_initializer value: (identifier) @font-lock-variable-use-face)
     (if_expression condition: (identifier) @font-lock-variable-use-face)
     (let_condition value: (identifier) @font-lock-variable-use-face)
     (let_declaration value: (identifier) @font-lock-variable-use-face)
     (match_arm value: (identifier) @font-lock-variable-use-face)
     (match_expression value: (identifier) @font-lock-variable-use-face)
     (reference_expression value: (identifier) @font-lock-variable-use-face)
     (return_expression (identifier) @font-lock-variable-use-face)
     (tuple_expression (identifier) @font-lock-variable-use-face)
     (unary_expression (identifier) @font-lock-variable-use-face)
     (while_expression condition: (identifier) @font-lock-variable-use-face))

   :language 'odin
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'odin
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `odin-ts-mode'.")


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
(define-derived-mode odin-ts-mode odin-parent-mode "Odin"
  :syntax-table odin-mode-syntax-table
  (unless (treesit-ready-p 'odin)
    (error "Tree-sitter for Odin isn't avalible"))

  (treesit-parser-create 'odin)

  (c-ts-common-comment-setup)


  ;; Indent.
  (setq-local treesit-simple-indent-rules odin-ts-mode--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}()" electric-indent-chars))
  ;; Navigation.
  (setq-local treesit-defun-type-regexp "declaration")
  (setq-local treesit-defun-name-function #'odin-ts-mode--defun-name)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings odin-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment definition)
                ( keyword string type directives)
                ( constant escape-sequence expression literal property)
                ( function bracket delimiter error)))


  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              `(("Function" "\\`function_declaration\\'" nil nil)
                ("Method" "\\`method_declaration\\'" nil nil)
                ("Struct" "\\`type_declaration\\'" odin-ts-mode--struct-node-p nil)
                ("Interface" "\\`type_declaration\\'" odin-ts-mode--interface-node-p nil)
                ("Type" "\\`type_declaration\\'" odin-ts-mode--other-type-node-p nil)
                ("Alias" "\\`type_declaration\\'" odin-ts-mode--alias-node-p nil)))
  )


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

;; Lsp-mode configuration
(defcustom odin-language-server-executable "ols"
  "Executable to run odin-lsp. Default: ols"
  :type 'string
  :group 'odin)

(with-eval-after-load 'lsp-mode
  (when (and (boundp 'lsp-language-id-configuration)
             (fboundp 'lsp-register-client)
             (fboundp 'make-lsp-client)
             (fboundp 'lsp-stdio-connection)
             (fboundp 'lsp-activate-on))
    (add-to-list 'lsp-language-id-configuration
	             '(odin-mode . "odin"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection odin-language-server-executable)
		              :activation-fn  (lsp-activate-on "odin")
		              :server-id      'ols))))

(provide 'odin-mode)

;;; odin-mode.el ends here
