;;; deadfd.el --- fast, friendly searching with fd  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Tommy Zhang

;; Author: Tommy Zhang <ab9986@qq.com>
;; URL: https://github.com/ab9986/deadfd
;; Package-Version: 20190516.2159
;; Keywords: tools
;; Version: 0.8
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (s "1.11.0") (spinner "1.7.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Perform text searches with the speed of ripfd and the comfort of
;; Emacs.  This is a bespoke mode that does not rely on
;; compilation-mode, but tries to be a perfect fit for ripfd.

;; Install from MELPA, then `M-x deadfd' will do a search!

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'spinner)

(defgroup deadfd nil
  "A powerful text search UI using ripfd."
  :group 'tools
  :group 'matching)

(defcustom deadfd-executable
  "fd"
  "The fd executable used by deadfd.
This will be looked up on `exec-path' if it isn't an absolute
path to the binary."
  :type 'string
  :group 'deadfd)

(defvar deadfd-max-buffers
  4
  "Deadfd will kill the least recently used results buffer
if there are more than this many.

To disable cleanup entirely, set this variable to nil.")

(defvar deadfd-project-root-function
  #'deadfd--project-root
  "Function called by `deadfd' to work out the root directory
to search from.

See also `deadfd-project-root-overrides'.")

(defvar deadfd-project-root-overrides nil
  "An alist associating project directories with the desired
search directory.

This is useful for large repos where you only want to search a
subdirectory. It's also handy for nested repos where you want to
search from the parent.

This affects the behaviour of `deadfd--project-root', so this
variable has no effect if you change
`deadfd-project-root-function'.")

(defvar deadfd-history
  nil
  "A list of the previous search terms.")

(defvar deadfd-max-line-length
  500
  "Truncate lines if they are longer than this.

Emacs performance can be really poor long lines, so this ensures
that searching minified files does not slow down movement in
results buffers.

In extreme cases (100KiB+ single-line files), we can get a stack
overflow on our regexp matchers if we don't apply this.")

(defface deadfd-meta-face
  '((t :inherit font-lock-comment-face))
  "Face used for deadfd UI text."
  :group 'deadfd)

(defface deadfd-filename-face
  '((t :inherit bold))
  "Face used for filename headings in results buffers."
  :group 'deadfd)

(defface deadfd-search-term-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for the search term in results buffers."
  :group 'deadfd)

(defface deadfd-regexp-metachar-face
  '((t :inherit
       ;; TODO: I've seen a more appropriate face in some themes,
       ;; find out what to use instead here.
       font-lock-constant-face))
  "Face used for regexp metacharacters in search terms."
  :group 'deadfd)

(defface deadfd-match-face
  '((t :inherit match))
  "Face used for the portion of a line that matches the search term."
  :group 'deadfd)

(defvar-local deadfd--search-term nil)
(defvar-local deadfd--search-type 'string)
(defvar-local deadfd--search-case 'smart)
(defvar-local deadfd--file-type 'all)

(defvar-local deadfd--context nil
  "When set, also show context of results.
This is stored as a cons cell of integers (lines-before . lines-after).")
(defvar-local deadfd--initial-filename nil
  "The filename of the buffer that deadfd was started from.
Used to offer better default values for file options.")

(defvar-local deadfd--current-file nil
  "The file we're currently inserting results for.")
(defvar-local deadfd--spinner nil)
(defvar-local deadfd--remaining-output nil
  "We can't guarantee that our process filter will always receive whole lines.
We save the last line here, in case we need to append more text to it.")
(defvar-local deadfd--postpone-start nil
  "If non-nil, don't (re)start searches.")
(defvar-local deadfd--running nil
  "If non-nil, a search is still running.")

(defvar-local deadfd--debug-command nil)
(defvar-local deadfd--debug-first-output nil)

(defvar-local deadfd--imenu-alist nil
  "Alist that stores filename and position for each matched files.
It is used to create `imenu' index.")

(defconst deadfd--position-column-width 5)

(defconst deadfd--color-code
  (rx "\x1b[" (+ digit) "m")
  "Regular expression for an ANSI color code.")

(defun deadfd--insert-output (output &optional finished)
  "Propertize OUTPUT from rifd and write to the current buffer."
  ;; If we had an unfinished line from our last call, include that.
  (when deadfd--remaining-output
    (setq output (concat deadfd--remaining-output output))
    (setq deadfd--remaining-output nil))

  (let ((inhibit-read-only t)
        (lines (s-lines output))
        prev-line-num)
    ;; Process filters run asynchronously, and don't guarantee that
    ;; OUTPUT ends with a complete line. Save the last line for
    ;; later processing.
    (unless finished
      (setq deadfd--remaining-output (-last-item lines))
      (setq lines (butlast lines)))

    (save-excursion
      (goto-char (point-max))
      (dolist (line lines)
        (cond
         ;; Ignore blank lines.
         ((s-blank? line))
         ;; Lines of just -- are used as a context separator when
         ;; calling ripfd with context flags.
         ((string= line "--")
          (let ((separator "--"))
            ;; Try to make the separator length match the previous
            ;; line numbers.
            (when prev-line-num
              (setq separator
                    (s-repeat (log prev-line-num 10) "-")))
            (insert
             (propertize (concat separator "\n")
                         'face 'deadfd-meta-face))))
         ;; If we don't have a color code, ripfd must be complaining
         ;; about something (e.g. zero matches for a
         ;; glob, or permission denied on some directories).
         ((not (s-matches-p deadfd--color-code line))
          (when deadfd--current-file
            (setq deadfd--current-file nil)
            (insert "\n"))
          (insert line "\n\n"))
         (t
          (-let* ((truncate-p (> (length line) deadfd-max-line-length))
                  (line
                   (if truncate-p
                       (substring line 0 deadfd-max-line-length)
                     line))
                  ((filename line-num content) (deadfd--split-line line))
                  (formatted-line-num
                   (s-pad-right deadfd--position-column-width " "
                                (number-to-string line-num)))
                  (pretty-line-num
                   (propertize formatted-line-num
                               'face 'deadfd-meta-face
                               'deadfd-filename filename
                               'deadfd-line-number line-num
                               'read-only t
                               'front-sticky t
                               'rear-nonsticky t))
                  (pretty-filename
                   (propertize filename
                               'face 'deadfd-filename-face
                               'deadfd-filename filename
                               'read-only t
                               'front-sticky t)))
            (cond
             ;; This is the first file we've seen, print the heading.
             ((null deadfd--current-file)
              (push (cons filename (point)) deadfd--imenu-alist)
              (insert pretty-filename "\n"))
             ;; This is a new file, print the heading with a spacer.
             ((not (equal deadfd--current-file filename))
              (push (cons filename (1+ (point))) deadfd--imenu-alist)
              (insert "\n" pretty-filename "\n")))
            (setq deadfd--current-file filename)

            ;; TODO: apply the invisible property if the user decided
            ;; to hide this filename before we finished finding
            ;; results in it.
            (insert pretty-line-num content)
            (when truncate-p
              (insert
               (propertize " ... (truncated)"
                           'face 'deadfd-meta-face)))
            (insert "\n")

            (setq prev-line-num line-num))))))))

(defvar deadfd-finished-hook nil
  "Hook run when `deadfd' search is finished.")

(defun deadfd--process-sentinel (process output)
  "Update the deadfd buffer associated with PROCESS as complete."
  (let ((buffer (process-buffer process))
        (finished-p (string= output "finished\n")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq deadfd--running nil)
        ;; rg has terminated, so stop the spinner.
        (spinner-stop deadfd--spinner)

        (deadfd--insert-output "" finished-p)

        ;; Report any errors that occurred.
        (unless (member output
                        (list
                         "exited abnormally with code 1\n"
                         "interrupt\n"
                         "finished\n"))
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert output))))

        (run-hooks 'deadfd-finished-hook)
        (message "Deadfd finished")))))

(defun deadfd--process-filter (process output)
  ;; Searches may see a lot of output, but it's really useful to have
  ;; a snippet of output when debugging. Store the first output received.
  (unless deadfd--debug-first-output
    (setq deadfd--debug-first-output output))

  ;; If we had an unfinished line from our last call, include that.
  (when deadfd--remaining-output
    (setq output (concat deadfd--remaining-output output))
    (setq deadfd--remaining-output nil))

  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (deadfd--insert-output output))))

(defun deadfd--extract-regexp (pattern s)
  "Search for PATTERN in S, and return the content of the first group."
  (string-match pattern s)
  (match-string 1 s))

(defconst deadfd--filename-regexp
  (rx bos "\x1b[0m\x1b[3" (or "5" "6") "m"
      (? "./")
      (group (+? anything))
      "\x1b[")
  "Extracts the filename from a ripfd line with ANSI color sequences.
We use the color sequences to extract the filename exactly, even
if the path contains colons.")

(defconst deadfd--line-num-regexp
  (rx "\x1b[32m" (group (+ digit)))
  "Extracts the line number from a ripfd line with ANSI color sequences.
Ripfd uses a unique color for line numbers, so we use that to
extract the linue number exactly.")

(defconst deadfd--line-contents-regexp
  (rx "\x1b[32m" (+ digit) "\x1b[0m" (or ":" "-") (group (* anything)))
  "Extract the line contents from a ripfd line with ANSI color sequences.
Use the unique color for line numbers to ensure we start at the
correct colon.

Note that the text in the group will still contain color codes
highlighting which parts matched the user's search term.")

(defconst deadfd--hit-regexp
  (rx-to-string
   `(seq
     ;; A reset color code.
     "\x1b[0m"
     ;; Two color codes, bold and color (any order).
     (regexp ,deadfd--color-code)
     (regexp ,deadfd--color-code)
     ;; The actual text.
     (group (+? anything))
     ;; A reset color code again.
     "\x1b[0m"))
  "Extract the portion of a line found by ripfd that matches the user's input.
This may occur multiple times in one line.")

(defun deadfd--split-line (line)
  "Split out the components of a raw LINE of output from rg.
Return the filename, line number, and the line content with ANSI
color codes replaced with string properties."
  (list
   (deadfd--extract-regexp deadfd--filename-regexp line)
   (string-to-number
    (deadfd--extract-regexp deadfd--line-num-regexp line))
   (deadfd--propertize-hits
    (deadfd--extract-regexp deadfd--line-contents-regexp line))))

(defun deadfd--propertize-hits (line-contents)
  "Given LINE-CONTENTS from ripfd, replace ANSI color codes
with Emacs text properties."
  (replace-regexp-in-string
   deadfd--hit-regexp
   (lambda (s)
     (propertize
      (match-string 1 s)
      'face 'deadfd-match-face))
   line-contents))

(define-button-type 'deadfd-search-term
  'action #'deadfd--search-term
  'help-echo "Change search term")

(defun deadfd--search-term (_button)
  (setq deadfd--search-term
        ;; TODO: say string or regexp
        (read-from-minibuffer
         "Search term: "
         deadfd--search-term))
  (rename-buffer
   (deadfd--buffer-name deadfd--search-term default-directory) t)
  (deadfd-restart))

(define-button-type 'deadfd-type
  'action #'deadfd--search-type
  'search-type nil
  'help-echo "Change search type")

(defun deadfd--search-type (button)
  (setq deadfd--search-type (button-get button 'search-type))
  (deadfd-restart))

(define-button-type 'deadfd-case
  'action #'deadfd--case
  'case nil
  'help-echo "Change case sensitivity")

(defun deadfd--case (button)
  (setq deadfd--search-case (button-get button 'case))
  (deadfd-restart))

(define-button-type 'deadfd-context
  'action #'deadfd--context
  'context nil
  'help-echo "Show/hide context around match")

(defun deadfd--context (button)
  ;; deadfd--context takes the value of (before . after) when set.
  (setq deadfd--context
        (cl-case (button-get button 'context)
          ((nil)
           nil)
          (before
           (cons
            (read-number "Show N lines before: ")
            (or (cdr-safe deadfd--context) 0)))
          (after
           (cons
            (or (car-safe deadfd--context) 0)
            (read-number "Show N lines after: ")))
          (t
           (error "Unknown context type"))))

  (deadfd-restart))

(defun deadfd--type-list ()
  "Query the rg executable for available file types."
  (let* ((output (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file-shell-command
                      (format "%s --type-list" deadfd-executable)
                      nil '(t nil)))))
         (lines (s-lines (s-trim output)))
         (types-and-globs
          (--map
           (s-split (rx ": ") it)
           lines)))
    (-map
     (-lambda ((type globs))
       (list type (s-split (rx ", ") globs)))
     types-and-globs)))

(define-button-type 'deadfd-file-type
  'action #'deadfd--file-type
  'case nil
  'help-echo "Change file type")

(defun deadfd--format-file-type (file-type extensions)
  (let* ((max-exts 4)
         (truncated (> (length extensions) max-exts)))
    (when truncated
      (setq extensions
            (append (-take max-exts extensions)
                    (list "..."))))
    (format "%s (%s)"
            file-type
            (s-join ", " extensions))))

(defun deadfd--glob-regexp (glob)
  "Convert GLOB pattern to the equivalent elisp regexp."
  (let* ((i 0)
         (result "^"))
    (while (< i (length glob))
      (let* ((char (elt glob i)))
        (cond
         ;; ? matches a single char in globs.
         ((eq char ??)
          (setq result (concat result "."))
          (cl-incf i))
         ;; * matches zero or more of anything.
         ((eq char ?*)
          (setq result (concat result ".*"))
          (cl-incf i))
         ;; [ab] matches a literal a or b.
         ;; [a-z] matches characters between a and z inclusive.
         ;; [?] matches a literal ?.
         ((eq char ?\[)
          ;; Find the matching ].
          (let ((j (1+ i)))
            (while (and (< j (length glob))
                        (not (eq (elt glob j) ?\])))
              (cl-incf j))
            (cl-incf j)
            (setq result (concat result
                                 (substring glob i j)))
            (setq i (1+ j))))
         (t
          (setq result (concat result (char-to-string char)))
          (cl-incf i)))))
    (concat result "$")))

(defun deadfd--matches-globs-p (filename globs)
  "Return non-nil if FILENAME matches any glob pattern in GLOBS."
  (when filename
    (--any (string-match-p (deadfd--glob-regexp it) filename)
           globs)))

(defun deadfd--relevant-file-type (filename types-and-globs)
  "Try to find the most relevant item in TYPES-AND-GLOBS for FILENAME."
  (let (;; Find all the items in TYPES-AND-GLOBS whose glob match
        ;; FILENAME.
        (matching (-filter (-lambda ((_type globs))
                             (deadfd--matches-globs-p filename globs))
                           types-and-globs)))
    (->> matching
         ;; Prefer longer names, so "markdown" over "md" for the type
         ;; name.
         (-sort (-lambda ((type1 _) (type2 _))
                  (< (length type1) (length type2))))
         ;; Prefer types with more extensions, as they tend to be more
         ;; common languages (e.g. 'ocaml' over 'ml').
         (-sort (-lambda ((_ globs1) (_ globs2))
                  (< (length globs1) (length globs2))))
         ;; But prefer elisp over lisp for .el files.
         (-sort (-lambda ((type1 _) (type2 _))
                  ;; Return t if we're comparing elisp with lisp, nil
                  ;; otherwise.
                  (and (equal type1 "lisp")
                       (equal type2 "elisp"))))
         ;; Take the highest scoring matching.
         (-last-item))))

(defun deadfd--read-file-type (filename)
  "Read a ripfd file type, defaulting to the type that matches FILENAME."
  (let* (;; Get the list of types we can offer.
         (types-and-globs (deadfd--type-list))
         ;; Build a list mapping the formatted types to the type name.
         (type-choices
          (-map
           (-lambda ((type globs))
             (list
              (deadfd--format-file-type type globs)
              type))
           types-and-globs))
         ;; Work out the default type name based on the filename.
         (default-type-and-globs
           (deadfd--relevant-file-type filename types-and-globs))
         (default
           (-when-let ((default-type default-globs) default-type-and-globs)
             (deadfd--format-file-type default-type default-globs)))
         ;; Prompt the user for a file type.
         (chosen
          (completing-read
           "File type: " type-choices nil t nil nil default)))
    (nth 1 (assoc chosen type-choices))))

(defun deadfd--file-type (button)
  (let ((button-type (button-get button 'file-type)))
    (cond
     ((eq button-type 'all)
      (setq deadfd--file-type 'all))
     ((eq button-type 'type)
      (let ((new-file-type
             (deadfd--read-file-type deadfd--initial-filename)))
        (setq deadfd--file-type (cons 'type new-file-type))))
     ((eq button-type 'glob)
      (let ((glob
             (read-from-minibuffer
              "Glob: "
              (cond
               ;; If we already have a glob pattern, edit it.
               ((eq (car-safe deadfd--file-type) 'glob)
                (cdr deadfd--file-type))
               ;; If the initial file had a file name of the form
               ;; foo.bar, offer *.bar as the initial glob.
               ((and deadfd--initial-filename
                     (file-name-extension deadfd--initial-filename))
                (format "*.%s"
                        (file-name-extension deadfd--initial-filename)))
               (t
                "*")))))
        (setq deadfd--file-type (cons 'glob glob))))
     (t
      (error "Unknown button type: %S" button-type))))
  (deadfd-restart))

(define-button-type 'deadfd-directory
  'action #'deadfd--directory
  'help-echo "Change base directory")

(defun deadfd--directory (_button)
  "Prompt the user for a new search directory, then restart the search."
  (setq default-directory
        (expand-file-name
         (read-directory-name "Search files in: ")))
  (rename-buffer
   (deadfd--buffer-name deadfd--search-term default-directory))
  (deadfd-restart))

(defun deadfd--button (text type &rest properties)
  ;; `make-text-button' mutates the string to add properties, so copy
  ;; TEXT first.
  (setq text (substring-no-properties text))
  (apply #'make-text-button text nil :type type properties))

(defun deadfd--format-command (search-term search-type case context)
  "Return a command string that we can execute in a shell
to obtain ripfd results."
  (format
   "%s --color auto  %s %s %s %s -- %s ."
   deadfd-executable
   (cond
    ((eq search-type 'string)
     "--fixed-strings")
    ((eq search-type 'regexp)
     "")
    (t
     (error "Unknown search type: %s" search-type)))
   (cond
    ((eq case 'smart)
     "")
    ((eq case 'sensitive)
     "--case-sensitive")
    ((eq case 'ignore)
     "--ignore-case")
    (t
     (error "Unknown case: %s" case)))
   ;; TODO: pass this as an argument.
   (cond
    ((eq deadfd--file-type 'all)
     "")
    ((eq (car-safe deadfd--file-type) 'type)
     (format "--extension %s" (cdr deadfd--file-type)))
    (t
     (error "Unknown file-type: %S" deadfd--file-type)))
   (if context
       (format "--before-context %s --after-context %s"
               (car context) (cdr context))
     "")
   (shell-quote-argument search-term)))

(defun deadfd--write-heading ()
  "Write the deadfd heading with buttons reflecting the current
search settings."
  (let ((start-pos (point))
        (inhibit-read-only t))
    (insert (propertize "Search term: "
                        'face 'deadfd-meta-face)
            (if (eq deadfd--search-type 'regexp)
                (deadfd--propertize-regexp deadfd--search-term)
              (propertize
               deadfd--search-term
               'face 'deadfd-search-term-face))
            " "
            (deadfd--button "change" 'deadfd-search-term)
            "\n"
            (propertize "Search type: "
                        'face 'deadfd-meta-face)

            (if (eq deadfd--search-type 'string)
                "string"
              (deadfd--button "string" 'deadfd-type
                                'search-type 'string))
            " "
            (if (eq deadfd--search-type 'words)
                "words"
              (deadfd--button "words" 'deadfd-type
                                'search-type 'words))
            " "
            (if (eq deadfd--search-type 'regexp)
                "regexp"
              (deadfd--button "regexp" 'deadfd-type
                                'search-type 'regexp))
            "\n"
            (propertize "Case: "
                        'face 'deadfd-meta-face)
            (if (eq deadfd--search-case 'smart)
                "smart"
              (deadfd--button "smart" 'deadfd-case
                                'case 'smart))
            " "
            (if (eq deadfd--search-case 'sensitive)
                "sensitive"
              (deadfd--button "sensitive" 'deadfd-case
                                'case 'sensitive))
            " "
            (if (eq deadfd--search-case 'ignore)
                "ignore"
              (deadfd--button "ignore" 'deadfd-case
                                'case 'ignore))
            "\n"
            (propertize "Context: "
                        'face 'deadfd-meta-face)
            (if deadfd--context
                (deadfd--button "none" 'deadfd-context
                                  'context nil)
              "none")
            " "
            (deadfd--button "before" 'deadfd-context
                              'context 'before)
            (if deadfd--context
                (format ":%d" (car deadfd--context))
              "")
            " "
            (deadfd--button "after" 'deadfd-context
                              'context 'after)
            (if deadfd--context
                (format ":%d" (cdr deadfd--context))
              "")

            "\n\n"
            (propertize "Directory: "
                        'face 'deadfd-meta-face)
            (deadfd--button
             (abbreviate-file-name default-directory)
             'deadfd-directory)
            (if (get-text-property 0 'deadfd-overridden default-directory)
                (propertize " (from override)" 'face 'deadfd-meta-face)
              "")
            "\n"
            (propertize "Files: "
                        'face 'deadfd-meta-face)
            (if (eq deadfd--file-type 'all)
                "all"
              (deadfd--button "all" 'deadfd-file-type
                                'file-type 'all))
            " "
            (deadfd--button "type" 'deadfd-file-type
                              'file-type 'type)
            (if (eq (car-safe deadfd--file-type) 'type)
                (format ":%s" (cdr deadfd--file-type))
              "")
            " "
            (deadfd--button "glob" 'deadfd-file-type
                              'file-type 'glob)
            (if (eq (car-safe deadfd--file-type) 'glob)
                (format ":%s" (cdr deadfd--file-type))
              "")
            "\n\n")
    (put-text-property
     start-pos (point)
     'read-only t)
    (put-text-property
     start-pos (point)
     'front-sticky t)))

;; TODO: could we do this in the minibuffer too?
(defun deadfd--propertize-regexp (regexp)
  "Given a string REGEXP representing a search term with regular
expression syntax, highlight the metacharacters.
Returns a copy of REGEXP with properties set."
  (setq regexp (copy-sequence regexp))

  ;; See https://docs.rs/regex/1.0.0/regex/#syntax
  (let ((metachars
         ;; Characters that don't match themselves.
         '(?\( ?\) ?\[ ?\] ?\{ ?\} ?| ?. ?+ ?* ?? ?^ ?$))
        ;; Characters that have special regexp meaning when preceded
        ;; with a backslash. This includes things like \b but not
        ;; things like \n.
        (escape-metachars
         '(?A ?b ?B ?d ?D ?p ?s ?S ?w ?W ?z))
        (prev-char nil))
    ;; Put the standard search term face on every character
    ;; individually.
    (dotimes (i (length regexp))
      (put-text-property
       i (1+ i)
       'face 'deadfd-search-term-face
       regexp))
    ;; Put the metacharacter face on any character that isn't treated
    ;; literally.
    (--each-indexed (string-to-list regexp)
      (cond
       ;; Highlight everything between { and }.
       ((and (eq it ?\{) (not (equal prev-char ?\\)))
        (let ((closing-pos it-index))
          ;; TODO: we have loops like this in several places, factor
          ;; out.
          (while (and (< closing-pos (length regexp))
                      (not (eq (elt regexp closing-pos)
                               ?\})))
            (cl-incf closing-pos))
          (cl-incf closing-pos)
          (put-text-property
           it-index closing-pos
           'face
           'deadfd-regexp-metachar-face
           regexp)))
       ;; Highlight individual metachars.
       ((and (memq it metachars) (not (equal prev-char ?\\)))
        (put-text-property
         it-index (1+ it-index)
         'face
         'deadfd-regexp-metachar-face
         regexp))
       ((and (memq it escape-metachars) (equal prev-char ?\\))
        (put-text-property
         (1- it-index) (1+ it-index)
         'face 'deadfd-regexp-metachar-face
         regexp)))

      (setq prev-char it)))
  regexp)

(defun deadfd--buffer-name (search-term directory)
  ;; TODO: Handle buffers already existing with this name.
  (format "*deadfd %s %s*"
          search-term
          (abbreviate-file-name directory)))

(defun deadfd--buffers ()
  "All the current deadfd results buffers.
Returns a list ordered by the most recently accessed."
  (--filter (with-current-buffer it
              (eq major-mode 'deadfd-mode))
            ;; `buffer-list' seems to be ordered by most recently
            ;; visited first.
            (buffer-list)))

(defun deadfd--buffer (search-term directory initial-filename)
  "Create and initialise a search results buffer."
  (let* ((buf-name (deadfd--buffer-name search-term directory))
         (buf (get-buffer buf-name)))
    (if buf
        ;; There was already a buffer with this name. Reset its search
        ;; state.
        (with-current-buffer buf
          (deadfd--stop-and-reset))
      ;; We need to create the buffer, ensure we don't exceed
      ;; `deadfd-max-buffers' by killing the least recently used.
      (progn
        (when (numberp deadfd-max-buffers)
          (let* ((excess-buffers (-drop (1- deadfd-max-buffers)
                                        (deadfd--buffers))))
            ;; Kill buffers so we have one buffer less than the maximum
            ;; before we create a new one.
            (-each excess-buffers #'kill-buffer)))

        (setq buf (get-buffer-create buf-name))))

    (with-current-buffer buf
      (setq default-directory directory)
      (let ((inhibit-read-only t))
        ;; This needs to happen first, as it clobbers all buffer-local
        ;; variables.
        (deadfd-mode)
        (erase-buffer)

        (setq deadfd--search-term search-term)
        (setq deadfd--current-file nil)
        (setq deadfd--initial-filename initial-filename))
      (setq buffer-read-only t))
    buf))

(defvar deadfd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'deadfd-visit-result)
    (define-key map (kbd "o") #'deadfd-visit-result-other-window)
    ;; TODO: we should still be able to click on buttons.

    (define-key map (kbd "g") #'deadfd-restart)

    ;; TODO: this should work when point in anywhere in file, not just
    ;; on its heading.
    (define-key map (kbd "TAB") #'deadfd-toggle-file-results)

    ;; Keybinding chosen to match `kill-compilation'.
    (define-key map (kbd "C-c C-k") #'deadfd-kill-process)

    (define-key map (kbd "n") #'deadfd-forward)
    (define-key map (kbd "p") #'deadfd-backward)
    (define-key map (kbd "N") #'deadfd-forward-match)
    (define-key map (kbd "P") #'deadfd-backward-match)

    map)
  "Keymap for `deadfd-mode'.")

(defvar deadfd-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'deadfd-visit-result)
    map)
  "Keymap for `deadfd-edit-mode'.")

(define-derived-mode deadfd-mode special-mode
  '("Deadfd" (:eval (spinner-print deadfd--spinner)))
  "Major mode for deadfd results buffers."
  (remove-hook 'after-change-functions #'deadfd--propagate-change t))

(defun deadfd--find-file (path)
  "Open PATH in a buffer, and return a cons cell
\(BUF . OPENED). OPENED is nil if there was aleady a buffer for
this path."
  (let* ((initial-buffers (buffer-list))
         (opened nil)
         ;; Skip running find-file-hook since it may prompt the user.
         (find-file-hook nil)
         ;; If we end up opening a buffer, don't bother with file
         ;; variables. It prompts the user, and we discard the buffer
         ;; afterwards anyway.
         (enable-local-variables nil)
         ;; Bind `auto-mode-alist' to nil, so we open the buffer in
         ;; `fundamental-mode' if it isn't already open.
         (auto-mode-alist nil)
         ;; Use `find-file-noselect' so we still decode bytes from the
         ;; underlying file.
         (buf (find-file-noselect path)))
    (unless (-contains-p initial-buffers buf)
      (setq opened t))
    (cons buf opened)))

(defun deadfd--propagate-change (beg end length)
  "Repeat the last modification to the results buffer in the
underlying file."
  ;; We should never be called outside a edit buffer, but be
  ;; defensive. Buggy functions in change hooks are painful.
  (when (eq major-mode 'deadfd-edit-mode)
    (save-excursion
      (goto-char beg)
      (-let* ((column (+ (deadfd--current-column) length))
              (filename (deadfd--filename))
              (line-number (deadfd--line-number))
              ((buf . opened) (deadfd--find-file filename))
              (inserted (buffer-substring beg end)))
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (goto-char
               (deadfd--buffer-position line-number column))
              (if (> length 0)
                  ;; We removed chars in the results buffer, so remove.
                  (delete-char (- length))
                ;; We inserted something, so insert the same chars.
                (insert inserted))))
          ;; If we weren't visiting this file before, just save it and
          ;; close it.
          (when opened
            (basic-save-buffer)
            (kill-buffer buf)))))))

(defvar deadfd-edit-mode-hook nil)

(defun deadfd-edit-mode ()
  "Major mode for editing the results files directly from a
deadfd results buffer.

\\{deadfd-edit-mode-map}"
  (interactive)
  (when deadfd--running
    (user-error "Can't edit a results buffer until the search is finished"))
  ;; We deliberately don't use `define-derived-mode' here because we
  ;; don't want to call `kill-all-local-variables'. Initialise the
  ;; major mode manually.
  (run-hooks 'change-major-mode-hook)
  (setq major-mode 'deadfd-edit-mode)
  (setq mode-name
        '(:propertize "Deadfd:Edit" face mode-line-emphasis))
  (use-local-map deadfd-edit-mode-map)

  (setq buffer-read-only nil)
  (add-hook 'after-change-functions #'deadfd--propagate-change nil t)

  (run-mode-hooks 'deadfd-edit-mode-hook))

(defun deadfd--current-column ()
  "Get the current column position in char terms.
This treats tabs as 1 and ignores the line numbers in the results
buffer."
  (let* ((line-start (line-beginning-position))
         (line-number
          (get-text-property line-start 'deadfd-line-number))
         (line-number-width
          (max deadfd--position-column-width
               (length (number-to-string line-number))))
         (char-count 0))
    (save-excursion
      (while (not (equal (point) line-start))
        (cl-incf char-count)
        (backward-char 1)))
    (max
     (- char-count line-number-width)
     0)))

(defun deadfd--flash-column-offsets (start end)
  "Temporarily highlight column offset from START to END."
  (let* ((line-start (line-beginning-position))
         (overlay (make-overlay
                   (+ line-start start)
                   (+ line-start end))))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 1.0 nil 'delete-overlay overlay)))

(defun deadfd--match-face-p (pos)
  "Is there a match face at POS?"
  (eq (get-text-property pos 'face) 'deadfd-match-face))

(defun deadfd--match-positions ()
  "Return a list of indexes of the current line's matches."
  (let (positions)
    (save-excursion
      (beginning-of-line)

      (let* ((line-number
              (get-text-property (point) 'deadfd-line-number))
             (line-number-width
              (max deadfd--position-column-width
                   (length (number-to-string line-number))))
             (i 0)
             (start-pos 0)
             (line-end-pos (line-end-position)))

        (forward-char line-number-width)

        (while (<= (point) line-end-pos)
          ;; If we've just entered a match, record the start position.
          (when (and (deadfd--match-face-p (point))
                     (not (deadfd--match-face-p (1- (point)))))
            (setq start-pos i))
          ;; If we've just left a match, record the match range.
          (when (and (not (deadfd--match-face-p (point)))
                     (deadfd--match-face-p (1- (point))))
            (push (list start-pos i) positions))

          (setq i (1+ i))
          (forward-char 1))))

    (nreverse positions)))

(defun deadfd--buffer-position (line-number column-offset)
  "Return the position equivalent to LINE-NUMBER at COLUMN-OFFSET
in the current buffer."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line-number))
    (forward-char column-offset)

    (point)))

(defun deadfd--filename (&optional pos)
  "Get the filename of the result at point POS.
If POS is nil, use the beginning position of the current line."
  (get-text-property (or pos (line-beginning-position)) 'deadfd-filename))

(defun deadfd--line-number ()
  "Get the filename of the result at point."
  (get-text-property (line-beginning-position) 'deadfd-line-number))

(defun deadfd--visit-result (open-fn)
  "Goto the search result at point."
  (interactive)
  (let* (
         (file-name (deadfd--filename)))
    (message file-name)
    (when file-name
      (funcall open-fn file-name)
      )))

(defun deadfd-visit-result-other-window ()
  "Goto the search result at point, opening in another window."
  (interactive)
  (deadfd--visit-result #'find-file-other-window))

(defun deadfd-visit-result ()
  "Goto the search result at point."
  (interactive)
  (deadfd--visit-result #'org-open-file-with-system))

(defvar-local deadfd--hidden-files nil
  "An alist recording which files currently have their lines
hidden in this deadfd results buffer.

Keys are interned filenames, so they compare with `eq'.")

(defun deadfd-toggle-file-results ()
  "Show/hide the results of the file at point."
  (interactive)
  (let* ((file-name (deadfd--filename))
         (line-number (deadfd--line-number)))
    (when (and file-name (not line-number))
      ;; We're on a file heading.
      (if (alist-get (intern file-name) deadfd--hidden-files)
          (deadfd--show)
        (deadfd--hide)))))

(defun deadfd--show ()
  (-let* ((file-name (deadfd--filename))
          ((start-pos end-pos) (alist-get (intern file-name) deadfd--hidden-files)))
    (remove-overlays start-pos end-pos 'invisible t)
    (setf (alist-get (intern file-name) deadfd--hidden-files)
          nil)))

(defun deadfd--hide ()
  "Hide the file results immediately after point."
  (save-excursion
    (let* ((file-name (deadfd--filename))
           (start-pos
            (progn
              (forward-line)
              (point)))
           (end-pos
            (progn
              (while (and
                      (get-text-property (point) 'deadfd-line-number)
                      (not (bobp)))
                (forward-line))
              ;; Step over the newline.
              (1+ (point))))
           (o (make-overlay start-pos end-pos)))
      (overlay-put o 'invisible t)
      (setf (alist-get (intern file-name) deadfd--hidden-files)
            (list start-pos end-pos)))))

(defun deadfd--interrupt-process ()
  "Gracefully stop the rg process, synchronously."
  (-when-let (proc (get-buffer-process (current-buffer)))
    ;; Ensure that our process filter is not called again.
    (set-process-filter proc #'ignore)

    (interrupt-process proc)
    ;; Wait for the process to terminate, so we know that
    ;; `deadfd--process-sentinel' has been called.
    (while (process-live-p proc)
      ;; `redisplay' can trigger process filters or sentinels.
      (redisplay)
      (sleep-for 0.1))))

(defun deadfd-kill-process ()
  "Kill the deadfd process associated with the current buffer."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (deadfd--interrupt-process)
    (message "No process running.")))

(defun deadfd--item-p (pos)
  "Is there something at POS that we can interact with?"
  (or (button-at pos)
      (deadfd--filename pos)))

(defun deadfd--move (forward-p)
  "Move to the next item.
This will either be a button, a filename, or a search result."
  (interactive)
  (let ((pos (point)))
    ;; If point is initially on an item, move past it.
    (while (and (deadfd--item-p pos)
                (if forward-p
                    (< pos (point-max))
                  (> pos (point-min))))
      (if forward-p
          (cl-incf pos)
        (cl-decf pos)))
    ;; Find the next item.
    (while (and (not (deadfd--item-p pos))
                (if forward-p
                    (< pos (point-max))
                  (> pos (point-min))))
      (if forward-p
          (cl-incf pos)
        (cl-decf pos)))
    ;; Regardless of direction, ensure point is at the beginning of
    ;; the item.
    (while (and (if forward-p
                    (< pos (point-max))
                  (> pos (point-min)))
                (deadfd--item-p (1- pos)))
      (cl-decf pos))
    ;; If we reached an item (we aren't at the first/last item), then
    ;; go to it.
    (when (deadfd--item-p pos)
      (goto-char pos))))

(defun deadfd-forward ()
  "Move forward to the next item.
This will either be a button, a filename, or a search result. See
also `deadfd-forward-match'."
  (interactive)
  (deadfd--move t))

(defun deadfd-backward ()
  "Move backward to the previous item.
This will either be a button, a filename, or a search result. See
also `deadfd-backward-match'."
  (interactive)
  (deadfd--move nil))

(defun deadfd--move-match (forward-p)
  "Move point to the beginning of the next/previous match."
  (interactive)
  (let ((start-pos (point)))
    ;; Move over the current match, if we were already on one.
    (while (eq (get-text-property (point) 'face)
               'deadfd-match-face)
      (if forward-p (forward-char) (backward-char)))
    (condition-case err
        (progn
          ;; Move point to the next match, which may be on the same line.
          (while (not (eq (get-text-property (point) 'face)
                          'deadfd-match-face))
            (if forward-p (forward-char) (backward-char)))
          ;; Ensure point is at the beginning of the match.
          (unless forward-p
            (while (eq (get-text-property (point) 'face)
                       'deadfd-match-face)
              (backward-char))
            (forward-char)))
      ;; Don't move point beyond the last match. However, it's still
      ;; useful to signal that we're at the end, so users can use this
      ;; command with macros and terminate when it's done.
      (beginning-of-buffer
       (goto-char start-pos)
       (signal 'beginning-of-buffer nil))
      (end-of-buffer
       (goto-char start-pos)
       (signal 'end-of-buffer nil)))))

(defun deadfd-forward-match ()
  "Move point forward to the beginning of next match.
Note that a result line may contain more than one match, or zero
matches (if the result line has been truncated)."
  (interactive)
  (deadfd--move-match t))

(defun deadfd-backward-match ()
  "Move point backward to the beginning of previous match."
  (interactive)
  (deadfd--move-match nil))

(defun deadfd--start (search-term search-type case)
  "Start a ripfd search."
  (setq deadfd--spinner (spinner-create 'progress-bar t))
  (setq deadfd--running t)
  (spinner-start deadfd--spinner)
  (let* ((command (deadfd--format-command
                   search-term search-type case
                   deadfd--context))
         (process
          (start-file-process-shell-command
           (format "rg %s" search-term)
           (current-buffer)
           command)))
    (setq deadfd--debug-command command)
    (set-process-filter process #'deadfd--process-filter)
    (set-process-sentinel process #'deadfd--process-sentinel)))

(defun deadfd--stop-and-reset ()
  "Terminate the current search and reset any search state."
  ;; Stop the old search, so we don't carry on inserting results from
  ;; the last thing we searched for.
  (deadfd--interrupt-process)

  (let ((inhibit-read-only t))
    ;; Reset UI: remove results, reset items hidden by TAB, and arrow
    ;; position.
    (erase-buffer)
    (setq deadfd--hidden-files nil)
    (when overlay-arrow-position
      (set-marker overlay-arrow-position nil))

    ;; Reset intermediate search state.
    (setq deadfd--current-file nil)
    (setq deadfd--spinner nil)
    (setq deadfd--remaining-output nil)
    (setq deadfd--current-file nil)
    (setq deadfd--debug-first-output nil)
    (setq deadfd--imenu-alist nil)))

(defun deadfd-restart ()
  "Re-run ripfd with the current search settings."
  (interactive)
  ;; If we haven't started yet, start the search if we've been called
  ;; by the user.
  (when (and deadfd--postpone-start
             (called-interactively-p 'interactive))
    (setq deadfd--postpone-start nil))

  (deadfd--stop-and-reset)

  (let ((start-point (point))
        (inhibit-read-only t))
    (deadfd--write-heading)
    ;; If the point was in the heading, ensure that we restore its
    ;; position.
    (goto-char (min (point-max) start-point))

    (if deadfd--postpone-start
        (deadfd--write-postponed)
      (deadfd--start
       deadfd--search-term
       deadfd--search-type
       deadfd--search-case))))

(defun deadfd--read-search-term ()
  "Read a search term from the minibuffer.
If region is active, return that immediately.  Otherwise, prompt
for a string, offering the current word as a default."
  (let (search-term)
    (if (use-region-p)
        (progn
          (setq search-term
                (buffer-substring-no-properties (region-beginning) (region-end)))
          (deactivate-mark))
      (let* ((sym (symbol-at-point))
             (sym-name (when sym
                         (substring-no-properties (symbol-name sym))))
             ;; TODO: prompt should say search string or search regexp
             ;; as appropriate.
             (prompt
              (if sym
                  (format "Search term (default %s): " sym-name)
                "Search term: ")))
        (setq search-term
              (read-from-minibuffer
               prompt nil nil nil 'deadfd-history sym-name))
        (when (equal search-term "")
          (setq search-term sym-name))))
    (unless (equal (car deadfd-history) search-term)
      (push search-term deadfd-history))
    search-term))

(defun deadfd--normalise-dirname (path)
  "Expand PATH and ensure that it doesn't end with a slash.
If PATH is remote path, it is not expanded."
  (directory-file-name (if (file-remote-p path)
                           path
                         (let (file-name-handler-alist)
                           (expand-file-name path)))))

(defun deadfd--lookup-override (path)
  "If PATH is present in `deadfd-project-root-overrides',
return the overridden value.
Otherwise, return PATH as is."
  (let* ((normalised-path (deadfd--normalise-dirname path))
         (override
          (-first
           (-lambda ((original . _))
             (equal (deadfd--normalise-dirname original) normalised-path))
           deadfd-project-root-overrides)))
    (when override
      (setq path (cdr override))
      (unless (stringp path)
        (user-error "Bad override: expected a path string, but got: %S" path))
      (setq path (propertize path 'deadfd-overridden t)))
    path))

(defun deadfd--project-root ()
  "Guess the project root of the given FILE-PATH."
  (let ((root default-directory)
        (project (project-current)))
    (when project
      (setq root (cdr project)))
    (when root
      (deadfd--lookup-override root))))

(defun deadfd--write-postponed ()
  (let* ((inhibit-read-only t)
         (restart-key
          (where-is-internal #'deadfd-restart deadfd-mode-map t)))
    (save-excursion
      (goto-char (point-max))
      (insert
       (format "Press %s to start the search."
               (key-description restart-key))))))

(defun deadfd--create-imenu-index ()
  "Create `imenu' index for matched files."
  (when deadfd--imenu-alist
    (list (cons "Files" (reverse deadfd--imenu-alist)))))

;;;###autoload
(defun deadfd (search-term)
  "Start a ripfd search for SEARCH-TERM.
If called with a prefix argument, create the results buffer but
don't actually start the search."
  (interactive (list (deadfd--read-search-term)))
  (let* ((dir (funcall deadfd-project-root-function))
         (buf (deadfd--buffer
               search-term
               dir
               (or deadfd--initial-filename
                   (buffer-file-name))))
         (last-results-buf (car-safe (deadfd--buffers)))
         prev-search-type
         prev-search-case)
    ;; Find out what search settings were used last time.
    (when last-results-buf
      (with-current-buffer last-results-buf
        (setq prev-search-type deadfd--search-type)
        (setq prev-search-case deadfd--search-case)))

    (switch-to-buffer buf)

    (setq imenu-create-index-function #'deadfd--create-imenu-index)
    (setq next-error-function #'deadfd-next-error)

    ;; If we have previous search settings, apply them to our new
    ;; search results buffer.
    (when last-results-buf
      (setq deadfd--search-type prev-search-type)
      (setq deadfd--search-case prev-search-case))

    (deadfd--write-heading)

    (if current-prefix-arg
        ;; Don't start the search, just create the buffer and inform
        ;; the user how to start when they're ready.
        (progn
          (setq deadfd--postpone-start t)
          (deadfd--write-postponed))
      ;; Start the search immediately.
      (deadfd--start
       search-term
       deadfd--search-type
       deadfd--search-case))))

(defun deadfd-next-error (arg reset)
  "Move to the next error.
If ARG is given, move by that many errors.

This is intended for use with `next-error-function', which see."
  (when reset
    (goto-char (point-min)))
  (beginning-of-line)
  (let ((direction (> arg 0)))
    (setq arg (abs arg))

    (while (and
            (not (zerop arg))
            (not (eobp)))
      (if direction
          (forward-line 1)
        (forward-line -1))
      ;; If we are on a specific result (not a heading), we have a line
      ;; number.
      (when (get-text-property (point) 'deadfd-line-number)
        (cl-decf arg))))
  (deadfd-visit-result-other-window))

(defun deadfd-debug ()
  "Show a buffer with some debug information about the current search."
  (interactive)
  (let ((command deadfd--debug-command)
        (output deadfd--debug-first-output)
        (buf (get-buffer-create "*deadfd debug*"))
        (inhibit-read-only t))
    (pop-to-buffer buf)
    (erase-buffer)
    (special-mode)
    (setq buffer-read-only t)

    (insert
     "About your environment:\n"
     (format "Platform: %s\n" system-type)
     (format "Emacs version: %s\n" emacs-version)
     (format "Command: %s\n" command)
     (format "default-directory: %S\n" default-directory)
     (format "\nInitial output from ripfd:\n%S" output)
     (format "\n\nPlease file bugs at https://github.com/ab9986/deadfd/issues/new"))))

(provide 'deadfd)
;;; deadfd.el ends here
