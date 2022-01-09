;;; bibkey.el --- Library for and about BibTeX keys -*- lexical-binding: t -*-

;; Copyright © 2022 Mykhailo Shevchuk

;; Author: Mykhailo Shevchuk <mail+dev@mshevchuk.com>
;; Created: 05 January 2022
;; URL: https://github.com/myshevchuk/bibkey
;; Keywords: bib
;;
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE.  If not, visit
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; BibKey is a library for generation of BibTeX keys
;;

;;; Code:
;;
;; This library used to be a part of Org Roam BibTeX (ORB) as orb-core.el.
;; Older commits can be tracked here: https://github.com/org-roam/org-roam-bibtex

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'dash)
(require 's)

(require 'cl-lib)
(require 'subr-x)
(require 'rx)

(defgroup bibkey nil
  "Automatic generation of BibTeX citation keys."
  :group 'bibkey
  :prefix "bibkey-")

(defcustom bibkey-format "%a%y%T[4][1]"
  "Format string for automatically generated citation keys.

Supported wildcards:

Basic
==========

 %a         |author| - first author's (or editor's) last name
 %t         |title | - first word of title
 %f{field}  |field | - first word of arbitrary field
 %y         |year  | - year YYYY
 %p         |page  | - first page
 %e{(expr)} |elisp | - execute elisp expression

Extended
==========

1. Capitalized versions:

 %A        |author| >
 %T        |title | >  Same as %a,%t,%f{field} but
 %F{field} |field | >  preserve original capitalization

2. Starred versions

 %a*, %A* |author| - include author's (editor's) initials
 %t*, %T* |title | - do not ignore words in `bibkey-titlewords-ignore'
 %y*      |year  | - year's last two digits __YY
 %p*      |page  | - use \"pagetotal\" field instead of default \"pages\"

3. Optional parameters

 %a[N][M][D]        |author| >
 %t[N][M][D]        |title | > include first N words/names
 %f{field}[N][M][D] |field | > include at most M first characters of word/name
 %p[D]              |page  | > put delimiter D between words

N and M should be a single digit 1-9. Putting more digits or any
other symbols will lead to ignoring the optional parameter and
those following it altogether.  D should be a single alphanumeric
symbol or one of `-_.:|'.

Optional parameters work both with capitalized and starred
versions where applicable.

4. Elisp expression

 - can be anything
 - should return a string or nil
 - will be evaluated before expanding other wildcards and therefore
can insert other wildcards
 - will have `entry' variable bound to the value of BibTeX entry the key
is being generated for, as returned by `bibtex-completion-get-entry'.
The variable may be safely manipulated in a destructive manner.

%e{(or (bibtex-completion-get-value \"volume\" entry) \"N/A\")}
%e{(my-function entry)}

Key generation is performed by  `bibkey-generate-key'."
  :risky t
  :type 'string
  :group 'bibkey)

(defcustom bibkey-titlewords-ignore
  '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
    "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*")
  "Patterns from title that will be ignored during key generation.
Every element is a regular expression to match parts of the title
that should be ignored during automatic key generation.  Case
sensitive."
  ;; Default value was taken from `bibtex-autokey-titleword-ignore'.
  :type '(repeat :tag "Regular expression" regexp)
  :group 'bibkey)

(defcustom bibkey-empty-field-token "N/A"
  "String to use when BibTeX field is nil or empty."
  :type 'string
  :group 'bibkey)

(defcustom bibkey-invalid-symbols
  " \"'()={},~#%\\"
  "Characters not allowed in a BibTeX key.
The key will be stripped of these characters."
  :type 'string
  :group 'bibkey)

(defun bibkey-get-value (field entry &optional default)
  "Return the value for FIELD in ENTRY.
Surrounding curly braces, double quotes and whitespace characters are trimmed."
  (let ((value (cdr (assoc-string field entry 'case-fold)))
        (left (rx (seq (opt whitespace) (+ (in "\"{" whitespace)))))
        (right (rx (seq (+ (in "\"}" whitespace)) (opt whitespace)))))
    (if value
        (save-match-data (string-trim value left right))
      default)))

(defun bibkey--format-field (field &rest specs)
  "Return BibTeX FIELD formatted according to plist SPECS.

Recognized keys:
==========
:entry       - BibTeX entry to use
:value       - Value of BibTeX field to use
               instead retrieving it from :entry
:capital     - capitalized version
:starred     - starred version
:words       - first optional parameter (number of words)
:characters  - second optional parameter (number of characters)
:delimiter   - third optional parameter (delimiter)

All values should be strings, including those representing numbers.

This function is used internally by `bibkey-generate-key'."
  (declare (indent 1))
  (-let* (((&plist :entry entry
                   :value value
                   :capital capital
                   :starred starred
                   :words words
                   :characters chars
                   :delimiter delim) specs)
          ;; field values will be split into a list of words. `separator' is a
          ;; regexp for word separators: either a whitespace, one or more
          ;; dashes, or en dash, or em dash
          (separator "\\([ \n\t]\\|[-]+\\|[—–]\\)")
          (invalid-chars-rx
           (rx-to-string `(any ,bibkey-invalid-symbols) t))
          (delim (or delim ""))
          result)
    ;; 0. virtual field "=name=" is used internally here and in
    ;; `bibkey-generate-key'; it stands for author or editor
    (if (string= field "=name=")
        ;; in name fields, logical words are full names consisting of several
        ;; words and containing spaces and punctuation, separated by a logical
        ;; separator, the word "and"
        (setq separator " and "
              value (or value
                        (bibkey-get-value "author" entry)
                        (bibkey-get-value "editor" entry)))
      ;; otherwise proceed with value or get it from entry
      (setq value (or value
                      (bibkey-get-value field entry))))
    (if (or (not value)
            (string-empty-p value))
        (setq result bibkey-empty-field-token)
      (when (> (length value) 0)
        (save-match-data
          ;; 1. split field into words
          (setq result (split-string value separator t "[ ,.;:-]+"))
          ;; 1a) only for title;
          ;; STARRED = include words from `bibkey-titlewords-ignore
          ;; unstarred version filters the keywords, starred ignores this block
          (when (and (string= field "title")
                     (not starred))
            (let ((ignore-rx (concat "\\`\\(:?"
                                     (mapconcat #'identity
                                                bibkey-titlewords-ignore
                                                "\\|") "\\)\\'"))
                  (words ()))
              (setq result (dolist (word result (nreverse words))
                             (unless (string-match-p ignore-rx word)
                               (push word words))))))
          ;; 2. take number of words equal to WORDS if that is set
          ;; or just the first word; also 0 = 1.
          (if words
              (setq words (string-to-number words)
                    result (-take (if (> words (length result))
                                      (length result)
                                    words)
                                  result))
            (setq result (list (car result))))
          ;; 2a) only for "=name=" field, i.e. author or editor
          ;; STARRED = include initials
          (when (string= field "=name=")
            ;; NOTE: here we expect name field 'Doe, J. B.'
            ;; should ideally be able to handle 'Doe, John M. Longname, Jr'
            (let ((r-x (if starred
                           "[ ,.\t\n]"
                         "\\`\\(.*?\\),.*\\'"))
                  (rep (if starred "" "\\1"))
                  (words ()))
              (setq result
                    (dolist (name result (nreverse words))
                      (push (s-replace-regexp r-x rep name) words)))))
          ;; 3. take at most CHARS number of characters from every word
          (when chars
            (let ((words ()))
              (setq chars (string-to-number chars)
                    result (dolist (word result (nreverse words))
                             (push
                              (substring word 0
                                         (if (< chars (length word))
                                             chars
                                           (length word)))
                              words)))))
          ;; 4. almost there: concatenate words, include DELIMiter
          (setq result (mapconcat #'identity result delim))
          ;; 5. CAPITAL = preserve case
          (unless capital
            (setq result (downcase result))))))
    ;; return result stripped of the invalid characters
    (s-replace-regexp invalid-chars-rx "" result t)))

(defun bibkey--eval (expr &optional entry)
  "Evaluate an arbitrary elisp expression EXPR passed as readable string.
The result of evaluting EXPR should be a string or nil.  The
expression will have ENTRY bound to `entry' at its disposal.
ENTRY should be a well-structured BibTeX entry alist such as
returned by `parsebib-read-entry' or `bibtex-completion-get-entry'."
  (let ((result (eval `(let ((entry (quote ,(copy-tree entry))))
                         ,(read expr)))))
    (unless (or (stringp result)
                (not result))
      (user-error "Invalid return type: %s.  \
Expression must return a string or nil" (type-of result)))
    (or result "")))

;;;###autoload
(defun bibkey-generate-key (entry &optional control-string)
  "Generate citation key from ENTRY according to `bibkey-format'.
Return a string.  If optional CONTROL-STRING is non-nil, use it
instead of `bibkey-format'."
  (let* ((case-fold-search nil)
         (str (or control-string bibkey-format))
         ;; star regexp: group 3!
         (star '(opt (group-n 3 "*")))
         ;; optional parameters: regexp groups 4-6!
         (opt1 '(opt (and "[" (opt (group-n 4 digit)) "]")))
         (opt2 '(opt (and "[" (opt (group-n 5 digit)) "]")))
         (opt3 '(opt (and "[" (opt (group-n 6 (any alnum "_.:|-"))) "]")))
         ;; capital letters: regexp group 2!
         ;; author wildcard regexp
         (a-rx (macroexpand
                `(rx (group-n 1 (or "%a" (group-n 2 "%A"))
                              ,star ,opt1 ,opt2 ,opt3))))
         ;; title wildcard regexp
         (t-rx (macroexpand
                `(rx (group-n 1 (or "%t" (group-n 2 "%T"))
                              ,star ,opt1 ,opt2 ,opt3))))
         ;; any field wildcard regexp
         ;; required parameter: group 7!
         (f-rx (macroexpand
                `(rx (group-n 1 (or "%f" (group-n 2 "%F"))
                              (and "{" (group-n 7 (1+ letter)) "}")
                              ,opt1 ,opt2 ,opt3))))
         ;; year wildcard regexp
         (y-rx (rx (group-n 1 "%y" (opt (group-n 3 "*")))))
         ;; page wildcard regexp
         (p-rx (macroexpand `(rx (group-n 1 "%p" ,star ,opt3))))
         ;; elisp expression wildcard regexp
         ;; elisp sexp: group 8!
         (e-rx (rx (group-n 1 "%e"
                            "{" (group-n 8 "(" (1+ ascii) ")") "}"))))
    ;; Evaluating elisp expression should go the first because it can produce
    ;; additional wildcards
    (while (string-match e-rx str)
      (setq str (replace-match
                 (save-match-data
                   (bibkey--eval
                    (match-string 8 str) entry)) t nil str 1)))
    ;; Expanding all other wildcards are actually
    ;; variations of calls to `bibkey--format-field' with many
    ;; commonalities, so we wrap it into a macro
    (cl-macrolet
        ((expand
          (wildcard &key field value entry capital
                    starred words characters delimiter)
          (let ((cap (or capital '(match-string 2 str)))
                (star (or starred '(match-string 3 str)))
                (opt1 (or words '(match-string 4 str)))
                (opt2 (or characters '(match-string 5 str)))
                (opt3 (or delimiter '(match-string 6 str))))
            `(while (string-match ,wildcard str)
               (setq str (replace-match
                          ;; we can safely pass nil values
                          ;; `bibkey--format-field' should
                          ;; handle them correctly
                          (bibkey--format-field ,field
                            :entry ,entry :value ,value
                            :capital ,cap :starred ,star
                            :words ,opt1 :characters ,opt2 :delimiter ,opt3)
                          t nil str 1))))))
      ;; Handle author wildcards
      (expand a-rx
              :field "=name="
              :value (or (bibkey-get-value "author" entry)
                         (bibkey-get-value "editor" entry)))
      ;; Handle title wildcards
      (expand t-rx
              :field "title"
              :value (or (bibkey-get-value "title" entry) ""))
      ;; Handle custom field wildcards
      (expand f-rx
              :field (match-string 7 str)
              :entry entry)
      ;; Handle pages wildcards %p*[-]
      (expand p-rx
              :field (if (match-string 3 str)
                         "pagetotal" "pages")
              :entry entry
              :words "1"))
    ;; Handle year wildcards
    ;; it's simple, so we do not use `bibkey--format-field' here
    ;; year should be well-formed: YYYY
    ;; TODO: put year into cl-macrolet
    (let ((year (or (bibkey-get-value "year" entry)
                    (bibkey-get-value "date" entry))))
      (if (or (not year)
              (string-empty-p year)
              (string= year bibkey-empty-field-token))
          (while (string-match y-rx str)
            (setq str (replace-match bibkey-empty-field-token
                                     t nil str 1)))
        (while (string-match y-rx str)
          (setq year (format "%04d" (string-to-number year))
                str (replace-match
                     (format "%s" (if (match-string 3 str)
                                      (substring year 2 4)
                                    (substring year 0 4)))
                     t nil str 1)))))
    str))

(provide 'bibkey)

;;; bibkey.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
