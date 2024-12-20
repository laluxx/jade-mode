;;; jade-mode.el --- Major mode for Jade programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Laluxx
;; Keywords: languages
;; Version: 0.1.0

;;; Commentary:

;; A major mode for editing Jade programming language files.
;; Supports basic syntax highlighting, indentation, and imenu.

;;; Code:

(require 'cl-lib)
(require 'imenu)

;; Customization group
(defgroup jade nil
  "Major mode for editing Jade source code."
  :group 'languages)

;; Define jade keywords
(defconst jade-keywords
  '("fn" "return")
  "Keywords used in Jade programming language.")

;; Define jade types
(defconst jade-types
  '("i32")
  "Built-in types in Jade programming language.")

;; Create regex for function definitions
(defconst jade-defun-regex
  (concat "^fn[[:space:]]+"
          "\\([[:word:]_][[:word:]_0-9]*\\)"  ; Function name
          "[[:space:]]*()") ; Arguments (currently only empty supported)
  "Regex to match Jade function definitions.")

;; Font lock keywords
(defconst jade-font-lock-keywords
  `(
    ;; Comments
    ("//.*$" . font-lock-comment-face)
    
    ;; Function definitions
    (,jade-defun-regex 1 font-lock-function-name-face)
    
    ;; Keywords
    (,(regexp-opt jade-keywords 'words) . font-lock-keyword-face)
    
    ;; Types
    (,(regexp-opt jade-types 'words) . font-lock-type-face))
  "Highlighting expressions for Jade mode.")

;; Custom newline and indent function
(defun jade-newline ()
  "Insert a newline and indent the new line."
  (interactive)
  (let ((cur-pos (point))
        (cur-col (current-column))
        (in-braces (looking-back "{[[:space:]]*" (line-beginning-position))))
    
    ;; Check if we're right after an opening brace
    (if in-braces
        (let ((brace-indent (current-indentation)))
          ;; Insert newline and indent for the current line
          (newline)
          (indent-line-to (+ brace-indent 4))
          ;; Insert closing brace on next line
          (save-excursion
            (newline)
            (indent-line-to brace-indent)
            (insert "}")))
      ;; Normal newline handling
      (let ((indent-level
             (save-excursion
               (forward-line 0)
               (let ((base-indent (current-indentation)))
                 (if (looking-at ".*{[[:space:]]*$")
                     (+ base-indent 4)
                   base-indent)))))
        (newline)
        (indent-line-to indent-level)
        (move-to-column indent-level))))

  ;; Indentation function
  (defun jade-indent-line ()
    "Indent current line as Jade code."
    (interactive)
    (let ((indent-level 0)
          (pos (- (point-max) (point)))
          cur-indent)
      (save-excursion
        (beginning-of-line)
        
        ;; Find previous non-empty line for base indentation
        (if (bobp)
            (setq cur-indent 0)
          (let ((continue t))
            (save-excursion
              (forward-line -1)
              (while (and continue (not (bobp)))
                (if (not (looking-at "^[[:space:]]*$"))
                    (progn
                      (setq indent-level (current-indentation))
                      (when (looking-at ".*{[[:space:]]*$")
                        (setq indent-level (+ indent-level 4)))
                      (setq continue nil))
                  (forward-line -1))))
            (setq cur-indent indent-level)))
        
        ;; Decrease indent for closing brace
        (when (looking-at "[[:space:]]*}")
          (setq cur-indent (max 0 (- cur-indent 4))))
        
        ;; Apply indentation
        (indent-line-to cur-indent)))
    
    ;; Move point after indentation if it was at the beginning
    (when (< (current-column) (current-indentation))
      (move-to-column (current-indentation)))))

;; Movement functions
(defun jade-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a function.
With ARG, do it that many times."
  (interactive "^p")
  (let ((arg (or arg 1)))
    (while (> arg 0)
      (re-search-backward jade-defun-regex nil t)
      (setq arg (1- arg)))))

(defun jade-end-of-defun (&optional arg)
  "Move forward to the end of a function.
With ARG, do it that many times."
  (interactive "^p")
  (let ((arg (or arg 1)))
    (while (> arg 0)
      (re-search-forward "{" nil t)
      (backward-char)
      (forward-sexp)
      (setq arg (1- arg)))))

;; Imenu integration
(defun jade-imenu-create-index ()
  "Create an index of all functions in buffer for Imenu."
  (let (index)
    (goto-char (point-min))
    (while (re-search-forward jade-defun-regex nil t)
      (push (cons (match-string-no-properties 1)
                  (match-beginning 0))
            index))
    (nreverse index)))

;;;###autoload
;; Mode map
(defvar jade-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Jade major mode.")

(define-derived-mode jade-mode prog-mode "Jade"
  "Major mode for editing Jade programming language code."
  :group 'jade
  
  ;; Comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  
  ;; Indentation
  (setq-local indent-line-function #'jade-indent-line)
  
  ;; Custom newline binding
  (define-key jade-mode-map (kbd "RET") 'jade-newline)
  
  ;; Font lock
  (setq-local font-lock-defaults '(jade-font-lock-keywords))
  
  ;; Imenu
  (setq-local imenu-create-index-function #'jade-imenu-create-index)
  
  ;; Movement
  (setq-local beginning-of-defun-function #'jade-beginning-of-defun)
  (setq-local end-of-defun-function #'jade-end-of-defun))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

(provide 'jade-mode)

;;; jade-mode.el ends here
