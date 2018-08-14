;;; esh-autosuggest.el --- History autosuggestions for eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/dieggsy/esh-autosuggest
;; Git-Repository: git://github.com/dieggsy/esh-autosuggest.git
;; Created: 2017-10-28
;; Version: 1.2.2
;; Keywords: completion company matching convenience abbrev
;; Package-Requires: ((emacs "24.4") (company "0.9.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Provides a company backend that implements functionality similar to fish
;; shell history autosuggestions.

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup esh-autosuggest nil
  "Fish-like autosuggestions for eshell."
  :group 'company)

(defcustom esh-autosuggest-delay 0
  "Delay for history autosuggestion."
  :group 'esh-autosuggest
  :type 'number)

(defcustom esh-autosuggest-use-company-map nil
  "Instead of overriding `company-active-map', use as-is.

This is disabled by default, as bindings in `company-active-map'
to RET and TAB may interfere with command input and completion
respectively."
  :group 'esh-autosuggest
  :type 'boolean)

(defvar esh-autosuggest-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<right>") 'company-complete-selection)
    (define-key keymap (kbd "C-f") 'company-complete-selection)
    (define-key keymap (kbd "M-<right>") 'esh-autosuggest-complete-word)
    (define-key keymap (kbd "M-f") 'esh-autosuggest-complete-word)
    keymap)
  "Keymap that is enabled during an active history
  autosuggestion.")

(defun esh-autosuggest-candidates (prefix)
  "Select the first eshell history candidate that starts with PREFIX."
  (let* ((history
          (delete-dups
           (mapcar (lambda (str)
                     (string-trim (substring-no-properties str)))
                   (ring-elements eshell-history-ring))))
         (most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        history)))
    (when most-similar
      `(,most-similar))))

(defun esh-autosuggest-complete-word ()
  (interactive)
  (save-excursion
    (let ((pos (point)))
      (company-complete-selection)
      (goto-char pos)
      (forward-word)
      (unless (or (eobp) (eolp))
        (kill-line))))
  (end-of-line)
  (ignore-errors
    (let ((inhibit-message t))
      (company-begin-backend 'esh-autosuggest))))

(defun esh-autosuggest--prefix ()
  "Get current eshell input."
  (let* ((input-start (progn
                        (save-excursion
                          (beginning-of-line)
                          (while (not (looking-at-p eshell-prompt-regexp))
                            (forward-line -1))
                          (eshell-bol))))
         (prefix
          (string-trim-left
           (buffer-substring-no-properties
            input-start
            (line-end-position)))))
    (if (not (string-empty-p prefix))
        prefix
      'stop)))

;;;###autoload
(defun esh-autosuggest (command &optional arg &rest ignored)
  "`company-mode' backend to provide eshell history suggestion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'esh-autosuggest))
    (prefix (and (eq major-mode 'eshell-mode)
                 (esh-autosuggest--prefix)))
    (candidates (esh-autosuggest-candidates arg))))

(define-minor-mode esh-autosuggest-mode
  "Enable fish-like autosuggestions in eshell.

You can use <right> to select the suggestion. This is
customizable through `esh-autosuggest-active-map'. If
you prefer to use the default value of `company-active-map', you
may set the variable
`esh-autosuggest-use-company-map', though this isn't
recommended as RET and TAB may not work as expected (send input,
trigger completions, respectively) when there is an active
suggestion.

The delay defaults to 0 seconds to emulate fish shell's
instantaneous suggestions, but is customizable with
`esh-autosuggest-delay'.

Note: This assumes you want to use something other than company
for shell completion, e.g. `eshell-pcomplete',
`completion-at-point', or helm-esh-pcomplete, since
`company-active-map', `company-backends', and `company-frontends'
will be locally overriden and company will be used solely for
history autosuggestions."
  :init-value nil
  :group 'esh-autosuggest
  (if esh-autosuggest-mode
      (progn
        (company-mode 1)
        (unless esh-autosuggest-use-company-map
          (setq-local company-active-map esh-autosuggest-active-map))
        (setq-local company-idle-delay esh-autosuggest-delay)
        (setq-local company-backends '(esh-autosuggest))
        (setq-local company-frontends '(company-preview-frontend)))
    (company-mode -1)
    (kill-local-variable 'company-active-map)
    (kill-local-variable 'company-idle-delay)
    (kill-local-variable 'company-backends)
    (kill-local-variable 'company-frontends)))

(provide 'esh-autosuggest)

;;; esh-autosuggest.el ends here
