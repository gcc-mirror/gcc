;;; Here is a handy keybinding: 

(global-set-key "\C-x\\" 'detexinfo)

;;;;;;;;;;;;;;;; detexinfo.el ;;;;;;;;;;;;;;;;
;;;
;;; Remove Texinfo commands from a Texinfo source file.
;;;
;;; Copyright (C) 1991, 1992 Free Software Foundation
;;; Robert J. Chassell
;;; bugs to bug-texinfo@prep.ai.mit.edu
;;;
;;; ==> test version <==
;;; Fails if Texinfo source file contains formatting errors.
;;; 
;;; Version 0.05 -  3 Jun 1992
;;; Add to list of removed commands.  Improve messages.
;;;
;;; Version 0.04 - 27 Jan 1992
;;; Rewrite to insert detexinfo'd text into a temporary buffer.
;;; 
;;; Version 0.03 - 27 Dec 1991
;;; Improved messages.
;;;
;;; Version 0.02 - 13 Nov 1991
;;; detexinfo-remove-inline-cmd, detexinfo-syntax-table: Handle 
;;;          nested commands.  
;;; detexinfo: Handle nested @'s, eg @samp{@}} and @samp{@@};
;;;          replace @TeX{} with TeX.
;;; 
;;; Version 0.01 - 13 Nov 1991
;;;
;;; Based on detex.el, by Bengt Martensson, 4 Oct 1987
;;;
;;;;;;;;;;;;;;;;

(defvar detexinfo-buffer-name "*detexinfo*"
  "*Name of the temporary buffer used by \\[detexinfo].")

(defvar detexinfo-syntax-table nil)

(if detexinfo-syntax-table
    nil
  (setq detexinfo-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\[ "." detexinfo-syntax-table)
  (modify-syntax-entry ?\] "." detexinfo-syntax-table)
  (modify-syntax-entry ?\" "." detexinfo-syntax-table)
  (modify-syntax-entry ?\\ "." detexinfo-syntax-table)
  (modify-syntax-entry ?\( "." detexinfo-syntax-table)
  (modify-syntax-entry ?\) "." detexinfo-syntax-table)
  (modify-syntax-entry ?{ "(}" detexinfo-syntax-table)
  (modify-syntax-entry ?} "){" detexinfo-syntax-table))

(defun detexinfo ()
  "Remove Texinfo commands from current buffer, copying result to new buffer.
BUG: Fails if Texinfo source file contains formatting errors."
  (interactive)
  (let ((input-buffer (current-buffer)))
    ;; Find a buffer to use.
    (switch-to-buffer (get-buffer-create detexinfo-buffer-name))
    (setq major-mode 'detexinfo-mode)
    (set-syntax-table detexinfo-syntax-table)
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    
    ;; Replace @{ and @} with %#* and *#% temporarily, so @samp{@{} works.
    ;; What is a better way of doing this??
    (goto-char (point-min))
    (while (search-forward "@{" nil t)  ; e.g., @samp{@{}
      (replace-match "%#*"))
    (goto-char (point-min))
    (while (search-forward "@}" nil t)
      (forward-char -3)                 ; e.g.,  @samp{@@}
      (if (looking-at "@")              ; Two @@ in a row
          (progn
            (delete-char 2)
            (insert "%&%#"))
        (forward-char 1)
        (delete-char 2)
        (insert "*#%")))
    
    (goto-char (point-min))
    ;; Remove @refill, the only inline command without braces.
    (while (search-forward "@refill" nil t)
      (replace-match ""))
    ;; Replace @TeX{} with TeX
    (goto-char (point-min))
    (while (search-forward "@TeX{}" nil t) (replace-match "TeX" t t))
    
    (detexinfo-remove-line-cmds-without-arg)
    (detexinfo-remove-inline-cmds-without-arg)
    (detexinfo-remove-inline-cmds-keep-arg)
    (detexinfo-remove-line-cmds-deletable-arg)
    (detexinfo-remove-line-cmds-maybe-delete-arg)
    (detexinfo-remove-line-cmds-keep-arg)
    
    ;; Now replace %#*, *#%, and %&%#  with {,  }, and @@.
    (goto-char (point-min))
    (while (search-forward "%#*" nil t)
      (replace-match "{"))
    (goto-char (point-min))
    (while (search-forward "*#%" nil t)
      (replace-match "}"))
    (goto-char (point-min))
    (while (search-forward "%&%#" nil t)
      (replace-match "@@"))
    
    ;; Scan for remaining two character @-commands
    (goto-char (point-min))
    (while (search-forward "@" nil t)
      (cond ((looking-at "[*:]")
             (delete-region (1- (point)) (1+ (point))))
            ((looking-at "[{}^@.'`]\"?!")
             (delete-region (1- (point)) (point)))))
    
    (goto-char (point-min))
    (message "Done...removed Texinfo commands from buffer. You may save it.")))

(defun detexinfo-remove-whole-line (cmd)
  "Delete Texinfo line command CMD at beginning of line and rest of line."
  (goto-char (point-min))
  (while
      (re-search-forward
       (concat "^@" cmd "[ \n]+") (point-max) t)
    (goto-char (match-beginning 0))
    (delete-region
     (point) (save-excursion (end-of-line) (1+ (point))))))

(defun detexinfo-remove-inline-cmd (cmd)
  "Delete Texinfo inline command CMD, eg. @point, @code."
  (goto-char (point-min))
  (while
      (re-search-forward (concat "@" cmd "{") (point-max) t)
    (save-excursion
      (forward-char -1)
      (forward-sexp 1)
      (delete-char -1))                 ; delete right brace
    (delete-region (point) (match-beginning 0)))) 

;;;;;;;;;;;;;;;; 

;;; 1. @setfilename and other line commands with args to delete

(defvar detexinfo-line-cmds-deletable-arg
  '("enumerate" "ftable" "vtable" "itemize" "table"
    "setfilename" "settitle" "setchapternewpage" 
    "footnotestyle" "paragraphindent"
    "include" "need" "sp" 
    "clear" "ifclear" "ifset"  "set"
    "defcodeindex" "defindex" "syncodeindex" "synindex")
  "List of Texinfo commands whose arguments should be deleted.")

(defun detexinfo-remove-line-cmds-deletable-arg ()
  "Delete Texinfo line commands together with their args, eg @setfilename."
  (message "Removing commands such as @enumerate...with their arguments...")
  (mapcar 'detexinfo-remove-whole-line
          detexinfo-line-cmds-deletable-arg))

;;; 2. @cindex and other cmds with args that may be deleted
;;;    This list is here just to make it easier to revise the
;;;    categories.  In particular, you might want to keep the index entries.

(defvar detexinfo-line-cmds-maybe-delete-arg
   '("cindex" "findex" "kindex" "pindex" "tindex" "vindex" "node"
     "c" "comment" "end" "headings"  "printindex" "vskip" 
     "evenfooting"  "evenheading" "everyfooting" "everyheading" 
     "oddfooting" "oddheading")
  "List of Texinfo commands whose arguments may possibly be deleted.")

(defun detexinfo-remove-line-cmds-maybe-delete-arg ()
  "Delete Texinfo line commands together with their arguments, eg, @cindex."
  (message "Removing commands such as @cindex...with their arguments...")
  (mapcar 'detexinfo-remove-whole-line
          detexinfo-line-cmds-maybe-delete-arg))

;;; 3. @chapter and other line cmds with args to keep.

(defvar detexinfo-line-cmds-keep-arg
   '("top" "chapter" "section" "subsection" "subsubsection" 
     "unnumbered" "unnumberedsec" "unnumberedsubsec" "unnumberedsubsubsec" 
     "majorheading" "chapheading" "heading" "subheading" "subsubheading" 
      "appendix" "appendixsec" "appendixsubsec" "appendixsubsubsec" 
     "item" "itemx"
     "title" "subtitle" "center" "author" "exdent"  
     "defcv" "deffn" "defivar" "defmac" "defmethod" "defop" "defopt" 
     "defspec" "deftp" "deftypefn" "deftypefun" "deftypvr"
     "deftypevar" "defun" "defvar" "defvr")
  "List of Texinfo line commands whose arguments should be kept.")

(defun detexinfo-remove-line-cmds-keep-arg ()
  "Delete Texinfo line commands but keep their arguments, eg @chapter."
  (message "Removing commands such as @chapter...but not their arguments...")
  (mapcar 'detexinfo-remove-line-cmd-keep-arg
          detexinfo-line-cmds-keep-arg))

(defun detexinfo-remove-line-cmd-keep-arg (cmd)
  "Delete Texinfo line command CMD but keep its argument, eg @chapter."
  (goto-char (point-min))
  (while
      (re-search-forward
       (concat "^@" cmd "[ \n]+") (point-max) t)
    (delete-region (match-beginning 0) (match-end 0))))

;;; 4. @bye and other line commands without args.

(defvar detexinfo-line-cmds-without-arg
  '("bye" "contents" "display" "example" "finalout" 
    "flushleft" "flushright" "format" "group" "ifhtml" "ifinfo" "iftex" 
    "ignore" "lisp" "menu" "noindent" "page" "quotation"  
    "shortcontents" "smallbook" "smallexample" "smalllisp" 
    "summarycontents" "tex" "thischapter" "thischaptername" 
    "thisfile" "thispage" "thissection" "thistitle" "titlepage")
  "List of Texinfo commands without arguments that should be deleted.")

(defun detexinfo-remove-line-cmds-without-arg ()
  "Delete line Texinfo commands that lack args, eg. @example."
  (message "Removing commands such as @example...that lack arguments...")
  (mapcar 'detexinfo-remove-whole-line
          detexinfo-line-cmds-without-arg))

;;; 5. @equiv and other inline cmds without args.

(defvar detexinfo-inline-cmds-without-arg
  '("equiv" "error" "expansion" "point" "print" "result"
    "asis"  "br" "bullet" "dots" "minus" "today")
  "List of Texinfo inline commands without arguments that should be deleted.")

(defun detexinfo-remove-inline-cmds-without-arg ()
  "Delete Texinfo inline commands in that lack arguments."
  (message "Removing within line commands such as @result...")
  (mapcar 'detexinfo-remove-inline-cmd
          detexinfo-inline-cmds-without-arg))

;;; 6. @code and other inline cmds with args to keep

(defvar detexinfo-inline-cmds-keep-arg
  '("b" "cartouche" "cite" "code" "copyright" "ctrl" "dfn" "dmn"
    "emph" "file" "footnote" "i" "inforef"
    "kbd" "key" "pxref" "r" "ref" "samp" "sc" "titlefont" 
    "strong" "t" "var" "w" "xref")
  "List of Texinfo inline commands with arguments that should be kept.")

(defun detexinfo-remove-inline-cmds-keep-arg ()
  "Delete Texinfo inline commands but keep its arg, eg. @code."
  (message
   "Removing within line commands such as @code...but not their arguments...")
  (mapcar 'detexinfo-remove-inline-cmd
          detexinfo-inline-cmds-keep-arg))

;;;;;;;;;;;;;;;; end detexinfo.el ;;;;;;;;;;;;;;;;
