;;; info.el --- info package for Emacs.

;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note that nowadays we expect info files to be made using makeinfo.

;;; Code:

(defvar Info-history nil
  "List of info nodes user has visited.
Each element of list is a list (FILENAME NODENAME BUFFERPOS).")

(defvar Info-enable-edit nil
  "*Non-nil means the \\<Info-mode-map>\\[Info-edit] command in Info can edit the current node.
This is convenient if you want to write info files by hand.
However, we recommend that you not do this.
It is better to write a Texinfo file and generate the Info file from that,
because that gives you a printed manual as well.")

(defvar Info-enable-active-nodes nil
  "Non-nil allows Info to execute Lisp code associated with nodes.
The Lisp code is executed when the node is selected.")
(put 'Info-enable-active-nodes 'risky-local-variable t)

(defvar Info-fontify t
  "*Non-nil enables highlighting and fonts in Info nodes.")

(defvar Info-fontify-maximum-menu-size 30000
  "*Maximum size of menu to fontify if `Info-fontify' is non-nil.")

(defvar Info-directory-list
  (let ((path (getenv "INFOPATH"))
	;; This is for older Emacs versions
	;; which might get this info.el from the Texinfo distribution.
	(path-separator (if (boundp 'path-separator) path-separator
			  (if (eq system-type 'ms-dos) ";" ":")))
	(source (expand-file-name "info/" source-directory))
	(sibling (if installation-directory
		     (expand-file-name "info/" installation-directory)))
	alternative)
    (if path
	(let ((list nil)
	      idx)
	  (while (> (length path) 0)
	    (setq idx (or (string-match path-separator path) (length path))
		  list (cons (substring path 0 idx) list)
		  path (substring path (min (1+ idx)
					    (length path)))))
	  (nreverse list))
      (if (and sibling (file-exists-p sibling))
	  (setq alternative sibling)
	(setq alternative source))
      (if (or (member alternative Info-default-directory-list)
	      (not (file-exists-p alternative))
	      ;; On DOS/NT, we use movable executables always,
	      ;; and we must always find the Info dir at run time.
	      (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
		  nil
		;; Use invocation-directory for Info only if we used it for
		;; exec-directory also.
		(not (string= exec-directory
			      (expand-file-name "lib-src/"
						installation-directory)))))
	  Info-default-directory-list
	(reverse (cons alternative
		       (cdr (reverse Info-default-directory-list)))))))
  "List of directories to search for Info documentation files.
nil means not yet initialized.  In this case, Info uses the environment
variable INFOPATH to initialize it, or `Info-default-directory-list'
if there is no INFOPATH variable in the environment.
The last element of `Info-default-directory-list' is the directory
where Emacs installs the Info files that come with it.

If you run the Emacs executable from the `src' directory in the Emacs
source tree, the `info' directory in the source tree is used as the last
element, in place of the installation Info directory.  This is useful
when you run a version of Emacs without installing it.")

(defvar Info-additional-directory-list nil
  "List of additional directories to search for Info documentation files.
These directories are not searched for merging the `dir' file.")

(defvar Info-current-file nil
  "Info file that Info is now looking at, or nil.
This is the name that was specified in Info, not the actual file name.
It doesn't contain directory names or file name extensions added by Info.")

(defvar Info-current-subfile nil
  "Info subfile that is actually in the *info* buffer now,
or nil if current info file is not split into subfiles.")

(defvar Info-current-node nil
  "Name of node that Info is now looking at, or nil.")

(defvar Info-tag-table-marker (make-marker)
  "Marker pointing at beginning of current Info file's tag table.
Marker points nowhere if file has no tag table.")

(defvar Info-current-file-completions nil
  "Cached completion list for current Info file.")

(defvar Info-index-alternatives nil
  "List of possible matches for last Info-index command.")

(defvar Info-standalone nil
  "Non-nil if Emacs was started solely as an Info browser.")

(defvar Info-suffix-list
  (if (eq system-type 'ms-dos)
      '( (".gz"      . "gunzip")
	 (".z"       . "gunzip")
	 (".inf"     . nil)
	 (""         . nil))
    '( (".info.Z"  . "uncompress")
       (".info.Y"  . "unyabba")
       (".info.gz" . "gunzip")
       (".info.z"  . "gunzip")
       (".info"    . nil)
       (".Z"       . "uncompress")
       (".Y"       . "unyabba")
       (".gz"      . "gunzip")
       (".z"       . "gunzip")
       (""         . nil)))
  "List of file name suffixes and associated decoding commands.
Each entry should be (SUFFIX . STRING); the file is given to
the command as standard input.  If STRING is nil, no decoding is done.
Because the SUFFIXes are tried in order, the empty string should
be last in the list.")

;; Concatenate SUFFIX onto FILENAME.  SUFFIX should start with a dot.
;; First, on ms-dos, delete some of the extension in FILENAME
;; to make room.
(defun info-insert-file-contents-1 (filename suffix)
  (if (not (eq system-type 'ms-dos))
      (concat filename suffix)
    (let* ((sans-exts (file-name-sans-extension filename))
	   ;; How long is the extension in FILENAME (not counting the dot).
	   (ext-len (max 0 (- (length filename) (length sans-exts) 1)))
	   ext-left)
      ;; SUFFIX starts with a dot.  If FILENAME already has one,
      ;; get rid of the one in SUFFIX (unless suffix is empty).
      (or (and (<= ext-len 0)
	       (not (eq (aref filename (1- (length filename))) ?.)))
	  (= (length suffix) 0)
	  (setq suffix (substring suffix 1)))
      ;; How many chars of that extension should we keep?
      (setq ext-left (min ext-len (max 0 (- 3 (length suffix)))))
      ;; Get rid of the rest of the extension, and add SUFFIX.
      (concat (substring filename 0 (- (length filename)
				       (- ext-len ext-left)))
	      suffix))))

(defun info-insert-file-contents (filename &optional visit)
  "Insert the contents of an info file in the current buffer.
Do the right thing if the file has been compressed or zipped."
  (let ((tail Info-suffix-list)
	fullname decoder)
    (if (file-exists-p filename)
	;; FILENAME exists--see if that name contains a suffix.
	;; If so, set DECODE accordingly.
	(progn
	  (while (and tail
		      (not (string-match
			    (concat (regexp-quote (car (car tail))) "$")
			    filename)))
	    (setq tail (cdr tail)))
	  (setq fullname filename
		decoder (cdr (car tail))))
      ;; Try adding suffixes to FILENAME and see if we can find something.
      (while (and tail
		  (not (file-exists-p (info-insert-file-contents-1
				       filename (car (car tail))))))
	(setq tail (cdr tail)))
      ;; If we found a file with a suffix, set DECODER according to the suffix
      ;; and set FULLNAME to the file's actual name.
      (setq fullname (info-insert-file-contents-1 filename (car (car tail)))
	    decoder (cdr (car tail)))
      (or tail
	  (error "Can't find %s or any compressed version of it" filename)))
    ;; check for conflict with jka-compr
    (if (and (featurep 'jka-compr)
	     (jka-compr-installed-p)
	     (jka-compr-get-compression-info fullname))
	(setq decoder nil))
    (insert-file-contents fullname visit)
    (if decoder
	(let ((buffer-read-only nil)
	      (default-directory (or (file-name-directory fullname)
				     default-directory)))
	  (call-process-region (point-min) (point-max) decoder t t)))))

;;;###autoload (add-hook 'same-window-buffer-names "*info*")

;;;###autoload
(defun info (&optional file)
  "Enter Info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir' 
in all the directories in that path."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Info file name: " nil nil t))))
  (if file
      (Info-goto-node (concat "(" file ")"))
    (if (get-buffer "*info*")
	(pop-to-buffer "*info*")
      (Info-directory))))

;;;###autoload
(defun info-standalone ()
  "Run Emacs as a standalone Info reader.
Usage:  emacs -f info-standalone [filename]
In standalone mode, \\<Info-mode-map>\\[Info-exit] exits Emacs itself."
  (setq Info-standalone t)
  (if (and command-line-args-left
	   (not (string-match "^-" (car command-line-args-left))))
      (condition-case err
	  (progn
	    (info (car command-line-args-left))
	    (setq command-line-args-left (cdr command-line-args-left)))
	(error (send-string-to-terminal
		(format "%s\n" (if (eq (car-safe err) 'error)
				   (nth 1 err) err)))
	       (save-buffers-kill-emacs)))
    (info)))

;; Go to an info node specified as separate filename and nodename.
;; no-going-back is non-nil if recovering from an error in this function;
;; it says do not attempt further (recursive) error recovery.
(defun Info-find-node (filename nodename &optional no-going-back)
  ;; Convert filename to lower case if not found as specified.
  ;; Expand it.
  (if filename
      (let (temp temp-downcase found)
	(setq filename (substitute-in-file-name filename))
	(if (string= (downcase filename) "dir")
	    (setq found t)
	  (let ((dirs (if (string-match "^\\./" filename)
			  ;; If specified name starts with `./'
			  ;; then just try current directory.
			  '("./")
			(if (file-name-absolute-p filename)
			    ;; No point in searching for an
			    ;; absolute file name
			    '(nil)
			  (if Info-additional-directory-list
			      (append Info-directory-list
				      Info-additional-directory-list)
			    Info-directory-list)))))
	    ;; Search the directory list for file FILENAME.
	    (while (and dirs (not found))
	      (setq temp (expand-file-name filename (car dirs)))
	      (setq temp-downcase
		    (expand-file-name (downcase filename) (car dirs)))
	      ;; Try several variants of specified name.
	      (let ((suffix-list Info-suffix-list))
		(while (and suffix-list (not found))
		  (cond ((file-exists-p
			  (info-insert-file-contents-1
			   temp (car (car suffix-list))))
			 (setq found temp))
			((file-exists-p
			  (info-insert-file-contents-1
			   temp-downcase (car (car suffix-list))))
			 (setq found temp-downcase)))
		  (setq suffix-list (cdr suffix-list))))
	      (setq dirs (cdr dirs)))))
	(if found
	    (setq filename found)
	  (error "Info file %s does not exist" filename))))
  ;; Record the node we are leaving.
  (if (and Info-current-file (not no-going-back))
      (setq Info-history
	    (cons (list Info-current-file Info-current-node (point))
		  Info-history)))
  ;; Go into info buffer.
  (switch-to-buffer "*info*")
  (buffer-disable-undo (current-buffer))
  (or (eq major-mode 'Info-mode)
      (Info-mode))
  (widen)
  (setq Info-current-node nil)
  (unwind-protect
      (progn
	;; Switch files if necessary
	(or (null filename)
	    (equal Info-current-file filename)
	    (let ((buffer-read-only nil))
	      (setq Info-current-file nil
		    Info-current-subfile nil
		    Info-current-file-completions nil
		    Info-index-alternatives nil
		    buffer-file-name nil)
	      (erase-buffer)
	      (if (eq filename t)
		  (Info-insert-dir)
		(info-insert-file-contents filename t)
		(setq default-directory (file-name-directory filename)))
	      (set-buffer-modified-p nil)
	      ;; See whether file has a tag table.  Record the location if yes.
	      (set-marker Info-tag-table-marker nil)
	      (goto-char (point-max))
	      (forward-line -8)
	      ;; Use string-equal, not equal, to ignore text props.
	      (or (string-equal nodename "*")
		  (not (search-forward "\^_\nEnd tag table\n" nil t))
		  (let (pos)
		    ;; We have a tag table.  Find its beginning.
		    ;; Is this an indirect file?
		    (search-backward "\nTag table:\n")
		    (setq pos (point))
		    (if (save-excursion
			  (forward-line 2)
			  (looking-at "(Indirect)\n"))
			;; It is indirect.  Copy it to another buffer
			;; and record that the tag table is in that buffer.
			(save-excursion
			  (let ((buf (current-buffer)))
			    (set-buffer (get-buffer-create " *info tag table*"))
                            (buffer-disable-undo (current-buffer))
			    (setq case-fold-search t)
			    (erase-buffer)
			    (insert-buffer-substring buf)
			    (set-marker Info-tag-table-marker
					(match-end 0))))
		      (set-marker Info-tag-table-marker pos))))
	      (setq Info-current-file
		    (if (eq filename t) "dir" filename))))
	;; Use string-equal, not equal, to ignore text props.
	(if (string-equal nodename "*")
	    (progn (setq Info-current-node nodename)
		   (Info-set-mode-line))
	  ;; Search file for a suitable node.
	  (let ((guesspos (point-min))
		(regexp (concat "Node: *" (regexp-quote nodename) " *[,\t\n\177]")))
	    ;; First get advice from tag table if file has one.
	    ;; Also, if this is an indirect info file,
	    ;; read the proper subfile into this buffer.
	    (if (marker-position Info-tag-table-marker)
		(save-excursion
		  (set-buffer (marker-buffer Info-tag-table-marker))
		  (goto-char Info-tag-table-marker)
		  (if (re-search-forward regexp nil t)
		      (progn
			(setq guesspos (read (current-buffer)))
			;; If this is an indirect file,
			;; determine which file really holds this node
			;; and read it in.
			(if (not (eq (current-buffer) (get-buffer "*info*")))
			    (setq guesspos
				  (Info-read-subfile guesspos))))
		    (error "No such node: %s" nodename))))
	    (goto-char (max (point-min) (- guesspos 1000)))
	    ;; Now search from our advised position (or from beg of buffer)
	    ;; to find the actual node.
	    (catch 'foo
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward regexp beg t)
		      (throw 'foo t))))
	      (error "No such node: %s" nodename)))
	  (Info-select-node)))
    ;; If we did not finish finding the specified node,
    ;; go back to the previous one.
    (or Info-current-node no-going-back (null Info-history)
	(let ((hist (car Info-history)))
	  (setq Info-history (cdr Info-history))
	  (Info-find-node (nth 0 hist) (nth 1 hist) t)
	  (goto-char (nth 2 hist)))))
  (goto-char (point-min)))

;; Cache the contents of the (virtual) dir file, once we have merged
;; it for the first time, so we can save time subsequently.
(defvar Info-dir-contents nil)

;; Cache for the directory we decided to use for the default-directory
;; of the merged dir text.
(defvar Info-dir-contents-directory nil)

;; Record the file attributes of all the files from which we
;; constructed Info-dir-contents.
(defvar Info-dir-file-attributes nil)

;; Construct the Info directory node by merging the files named `dir'
;; from various directories.  Set the *info* buffer's
;; default-directory to the first directory we actually get any text
;; from.
(defun Info-insert-dir ()
  (if (and Info-dir-contents Info-dir-file-attributes
	   ;; Verify that none of the files we used has changed
	   ;; since we used it.
	   (eval (cons 'and
		       (mapcar '(lambda (elt)
				  (let ((curr (file-attributes (car elt))))
				    ;; Don't compare the access time.
				    (if curr (setcar (nthcdr 4 curr) 0))
				    (setcar (nthcdr 4 (cdr elt)) 0)
				    (equal (cdr elt) curr)))
			       Info-dir-file-attributes))))
      (insert Info-dir-contents)
    (let ((dirs Info-directory-list)
	  buffers buffer others nodes dirs-done)

      (setq Info-dir-file-attributes nil)

      ;; Search the directory list for the directory file.
      (while dirs
	(let ((truename (file-truename (expand-file-name (car dirs)))))
	  (or (member truename dirs-done)
	      (member (directory-file-name truename) dirs-done)
	      ;; Try several variants of specified name.
	      ;; Try upcasing, appending `.info', or both.
	      (let* (file
		     (attrs
		      (or
		       (progn (setq file (expand-file-name "dir" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "DIR" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "dir.info" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "DIR.INFO" truename))
			      (file-attributes file)))))
		(setq dirs-done
		      (cons truename
			    (cons (directory-file-name truename)
				  dirs-done)))
		(if attrs
		    (save-excursion
		      (or buffers
			  (message "Composing main Info directory..."))
		      (set-buffer (generate-new-buffer "info dir"))
		      (insert-file-contents file)
		      (setq buffers (cons (current-buffer) buffers)
			    Info-dir-file-attributes
			    (cons (cons file attrs)
				  Info-dir-file-attributes))))))
	  (or (cdr dirs) (setq Info-dir-contents-directory (car dirs)))
	  (setq dirs (cdr dirs))))
      
      (or buffers
	  (error "Can't find the Info directory node"))
      ;; Distinguish the dir file that comes with Emacs from all the
      ;; others.  Yes, that is really what this is supposed to do.
      ;; If it doesn't work, fix it.
      (setq buffer (car buffers)
	    others (cdr buffers))

      ;; Insert the entire original dir file as a start; note that we've
      ;; already saved its default directory to use as the default
      ;; directory for the whole concatenation.
      (insert-buffer buffer)

      ;; Look at each of the other buffers one by one.
      (while others
	(let ((other (car others)))
	  ;; In each, find all the menus.
	  (save-excursion
	    (set-buffer other)
	    (goto-char (point-min))
	    ;; Find each menu, and add an elt to NODES for it.
	    (while (re-search-forward "^\\* Menu:" nil t)
	      (let (beg nodename end)
		(forward-line 1)
		(setq beg (point))
		(search-backward "\n\^_")
		(search-forward "Node: ")
		(setq nodename (Info-following-node-name))
		(search-forward "\n\^_" nil 'move)
		(beginning-of-line)
		(setq end (point))
		(setq nodes (cons (list nodename other beg end) nodes))))))
	(setq others (cdr others)))
      ;; Add to the main menu a menu item for each other node.
      (re-search-forward "^\\* Menu:")
      (forward-line 1)
      (let ((menu-items '("top"))
	    (nodes nodes)
	    (case-fold-search t)
	    (end (save-excursion (search-forward "\^_" nil t) (point))))
	(while nodes
	  (let ((nodename (car (car nodes))))
	    (save-excursion
	      (or (member (downcase nodename) menu-items)
		  (re-search-forward (concat "^\\* "
					     (regexp-quote nodename)
					     "::")
				     end t)
		  (progn
		    (insert "* " nodename "::" "\n")
		    (setq menu-items (cons nodename menu-items))))))
	  (setq nodes (cdr nodes))))
      ;; Now take each node of each of the other buffers
      ;; and merge it into the main buffer.
      (while nodes
	(let ((nodename (car (car nodes))))
	  (goto-char (point-min))
	  ;; Find the like-named node in the main buffer.
	  (if (re-search-forward (concat "\n\^_.*\n.*Node: "
					 (regexp-quote nodename)
					 "[,\n\t]")
				 nil t)
	      (progn
		(search-forward "\n\^_" nil 'move)
		(beginning-of-line)
		(insert "\n"))
	    ;; If none exists, add one.
	    (goto-char (point-max))
	    (insert "\^_\nFile: dir\tNode: " nodename "\n\n* Menu:\n\n"))
	  ;; Merge the text from the other buffer's menu
	  ;; into the menu in the like-named node in the main buffer.
	  (apply 'insert-buffer-substring (cdr (car nodes))))
	(setq nodes (cdr nodes)))
      ;; Kill all the buffers we just made.
      (while buffers
	(kill-buffer (car buffers))
	(setq buffers (cdr buffers)))
      (message "Composing main Info directory...done"))
    (setq Info-dir-contents (buffer-string)))
  (setq default-directory Info-dir-contents-directory))

(defun Info-read-subfile (nodepos)
  (set-buffer (marker-buffer Info-tag-table-marker))
  (goto-char (point-min))
  (search-forward "\n\^_")
  (let (lastfilepos
	lastfilename)
    (forward-line 2)
    (catch 'foo
      (while (not (looking-at "\^_"))
	(if (not (eolp))
	    (let ((beg (point))
		  thisfilepos thisfilename)
	      (search-forward ": ")
	      (setq thisfilename  (buffer-substring beg (- (point) 2)))
	      (setq thisfilepos (read (current-buffer)))
	      ;; read in version 19 stops at the end of number.
	      ;; Advance to the next line.
	      (forward-line 1)
	      (if (> thisfilepos nodepos)
		  (throw 'foo t))
	      (setq lastfilename thisfilename)
	      (setq lastfilepos thisfilepos))
	  (forward-line 1))))
    (set-buffer (get-buffer "*info*"))
    (or (equal Info-current-subfile lastfilename)
	(let ((buffer-read-only nil))
	  (setq buffer-file-name nil)
	  (widen)
	  (erase-buffer)
	  (info-insert-file-contents lastfilename)
	  (set-buffer-modified-p nil)
	  (setq Info-current-subfile lastfilename)))
    (goto-char (point-min))
    (search-forward "\n\^_")
    (+ (- nodepos lastfilepos) (point))))

;; Select the info node that point is in.
(defun Info-select-node ()
  (save-excursion
   ;; Find beginning of node.
   (search-backward "\n\^_")
   (forward-line 2)
   ;; Get nodename spelled as it is in the node.
   (re-search-forward "Node:[ \t]*")
   (setq Info-current-node
	 (buffer-substring-no-properties (point)
					 (progn
					  (skip-chars-forward "^,\t\n")
					  (point))))
   (Info-set-mode-line)
   ;; Find the end of it, and narrow.
   (beginning-of-line)
   (let (active-expression)
     (narrow-to-region (point)
		       (if (re-search-forward "\n[\^_\f]" nil t)
			   (prog1
			    (1- (point))
			    (if (looking-at "[\n\^_\f]*execute: ")
				(progn
				  (goto-char (match-end 0))
				  (setq active-expression
					(read (current-buffer))))))
			 (point-max)))
     (if Info-enable-active-nodes (eval active-expression))
     (if Info-fontify (Info-fontify-node))
     (run-hooks 'Info-selection-hook))))

(defun Info-set-mode-line ()
  (setq mode-line-buffer-identification
	(concat
	 "Info:  ("
	 (if Info-current-file
	     (file-name-nondirectory Info-current-file)
	   "")
	 ")"
	 (or Info-current-node ""))))

;; Go to an info node specified with a filename-and-nodename string
;; of the sort that is found in pointers in nodes.

(defun Info-goto-node (nodename)
  "Go to info node named NAME.  Give just NODENAME or (FILENAME)NODENAME."
  (interactive (list (Info-read-node-name "Goto node: ")))
  (let (filename)
    (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)"
		  nodename)
    (setq filename (if (= (match-beginning 1) (match-end 1))
		       ""
		     (substring nodename (match-beginning 2) (match-end 2)))
	  nodename (substring nodename (match-beginning 3) (match-end 3)))
    (let ((trim (string-match "\\s *\\'" filename)))
      (if trim (setq filename (substring filename 0 trim))))
    (let ((trim (string-match "\\s *\\'" nodename)))
      (if trim (setq nodename (substring nodename 0 trim))))
    (if transient-mark-mode (deactivate-mark))
    (Info-find-node (if (equal filename "") nil filename)
		    (if (equal nodename "") "Top" nodename))))

;; This function is used as the "completion table" while reading a node name.
;; It does completion using the alist in completion-table
;; unless STRING starts with an open-paren.
(defun Info-read-node-name-1 (string predicate code)
  (let ((no-completion (and (> (length string) 0) (eq (aref string 0) ?\())))
    (cond ((eq code nil)
	   (if no-completion
	       string
	     (try-completion string completion-table predicate)))
	  ((eq code t)
	   (if no-completion
	       nil
	     (all-completions string completion-table predicate)))
	  ((eq code 'lambda)
	   (if no-completion
	       t
	     (assoc string completion-table))))))

(defun Info-read-node-name (prompt &optional default)
  (let* ((completion-ignore-case t)
	 (completion-table (Info-build-node-completions))
	 (nodename (completing-read prompt 'Info-read-node-name-1)))
    (if (equal nodename "")
	(or default
	    (Info-read-node-name prompt))
      nodename)))

(defun Info-build-node-completions ()
  (or Info-current-file-completions
      (let ((compl nil))
	(save-excursion
	  (save-restriction
	    (if (marker-buffer Info-tag-table-marker)
		(progn
		  (set-buffer (marker-buffer Info-tag-table-marker))
		  (widen)
		  (goto-char Info-tag-table-marker)
		  (while (re-search-forward "\nNode: \\(.*\\)\177" nil t)
		    (setq compl
			  (cons (list (buffer-substring (match-beginning 1)
							(match-end 1)))
				compl))))
	      (widen)
	      (goto-char (point-min))
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward "Node: *\\([^,\n]*\\) *[,\n\t]"
					  beg t)
		      (setq compl 
			    (cons (list (buffer-substring (match-beginning 1)
							  (match-end 1)))
				  compl))))))))
	(setq Info-current-file-completions compl))))

(defun Info-restore-point (hl)
  "If this node has been visited, restore the point value when we left."
  (while hl
    (if (and (equal (nth 0 (car hl)) Info-current-file)
	     ;; Use string-equal, not equal, to ignore text props.
	     (string-equal (nth 1 (car hl)) Info-current-node))
	(progn
	  (goto-char (nth 2 (car hl)))
	  (setq hl nil))		;terminate the while at next iter
      (setq hl (cdr hl)))))

(defvar Info-last-search nil
  "Default regexp for \\<Info-mode-map>\\[Info-search] command to search for.")

(defun Info-search (regexp)
  "Search for REGEXP, starting from point, and select node it's found in."
  (interactive "sSearch (regexp): ")
  (if transient-mark-mode (deactivate-mark))
  (if (equal regexp "")
      (setq regexp Info-last-search)
    (setq Info-last-search regexp))
  (let ((found ()) current
	(onode Info-current-node)
	(ofile Info-current-file)
	(opoint (point))
	(osubfile Info-current-subfile))
    (save-excursion
      (save-restriction
	(widen)
	(if (null Info-current-subfile)
	    (progn (re-search-forward regexp) (setq found (point)))
	  (condition-case err
	      (progn (re-search-forward regexp) (setq found (point)))
	    (search-failed nil)))))
    (if (not found) ;can only happen in subfile case -- else would have erred
	(unwind-protect
	    (let ((list ()))
	      (set-buffer (marker-buffer Info-tag-table-marker))
	      (goto-char (point-min))
	      (search-forward "\n\^_\nIndirect:")
	      (save-restriction
		(narrow-to-region (point)
				  (progn (search-forward "\n\^_")
					 (1- (point))))
		(goto-char (point-min))
		(search-forward (concat "\n" osubfile ": "))
		(beginning-of-line)
		(while (not (eobp))
		  (re-search-forward "\\(^.*\\): [0-9]+$")
		  (goto-char (+ (match-end 1) 2))
		  (setq list (cons (cons (read (current-buffer))
					 (buffer-substring (match-beginning 1)
							   (match-end 1)))
				   list))
		  (goto-char (1+ (match-end 0))))
		(setq list (nreverse list)
		      current (car (car list))
		      list (cdr list)))
	      (while list
		(message "Searching subfile %s..." (cdr (car list)))
		(Info-read-subfile (car (car list)))
		(setq list (cdr list))
;;		(goto-char (point-min))
		(if (re-search-forward regexp nil t)
		    (setq found (point) list ())))
	      (if found
		  (message "")
		(signal 'search-failed (list regexp))))
	  (if (not found)
	      (progn (Info-read-subfile opoint)
		     (goto-char opoint)
		     (Info-select-node)))))
    (widen)
    (goto-char found)
    (Info-select-node)
    ;; Use string-equal, not equal, to ignore text props.
    (or (and (string-equal onode Info-current-node)
	     (equal ofile Info-current-file))
	(setq Info-history (cons (list ofile onode opoint)
				 Info-history)))))

;; Extract the value of the node-pointer named NAME.
;; If there is none, use ERRORNAME in the error message; 
;; if ERRORNAME is nil, just return nil.
(defun Info-extract-pointer (name &optional errorname)
  (save-excursion
   (goto-char (point-min))
   (forward-line 1)
   (if (re-search-backward (concat name ":") nil t)
       (progn
	 (goto-char (match-end 0))
	 (Info-following-node-name))
     (if (eq errorname t)
	 nil
       (error "Node has no %s" (capitalize (or errorname name)))))))

;; Return the node name in the buffer following point.
;; ALLOWEDCHARS, if non-nil, goes within [...] to make a regexp
;; saying which chas may appear in the node name.
(defun Info-following-node-name (&optional allowedchars)
  (skip-chars-forward " \t")
  (buffer-substring-no-properties
   (point)
   (progn
     (while (looking-at (concat "[" (or allowedchars "^,\t\n") "]"))
       (skip-chars-forward (concat (or allowedchars "^,\t\n") "("))
       (if (looking-at "(")
	   (skip-chars-forward "^)")))
     (skip-chars-backward " ")
     (point))))

(defun Info-next ()
  "Go to the next node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "next")))

(defun Info-prev ()
  "Go to the previous node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "prev[ious]*" "previous")))

(defun Info-up ()
  "Go to the superior node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "up"))
  (Info-restore-point Info-history))

(defun Info-last ()
  "Go back to the last node visited."
  (interactive)
  (or Info-history
      (error "This is the first Info node you looked at"))
  (let (filename nodename opoint)
    (setq filename (car (car Info-history)))
    (setq nodename (car (cdr (car Info-history))))
    (setq opoint (car (cdr (cdr (car Info-history)))))
    (setq Info-history (cdr Info-history))
    (Info-find-node filename nodename)
    (setq Info-history (cdr Info-history))
    (goto-char opoint)))

(defun Info-directory ()
  "Go to the Info directory node."
  (interactive)
  (Info-find-node "dir" "top"))

(defun Info-follow-reference (footnotename)
  "Follow cross reference named NAME to the node it refers to.
NAME may be an abbreviation of the reference name."
  (interactive
   (let ((completion-ignore-case t)
	 completions default alt-default (start-point (point)) str i bol eol)
     (save-excursion
       ;; Store end and beginning of line.
       (end-of-line)
       (setq eol (point))
       (beginning-of-line)
       (setq bol (point))

       (goto-char (point-min))
       (while (re-search-forward "\\*note[ \n\t]*\\([^:]*\\):" nil t)
	 (setq str (buffer-substring
		    (match-beginning 1)
		    (1- (point))))
	 ;; See if this one should be the default.
	 (and (null default)
	      (<= (match-beginning 0) start-point)
	      (<= start-point (point))
	      (setq default t))
	 ;; See if this one should be the alternate default.
	 (and (null alt-default)
	      (and (<= bol (match-beginning 0))
		   (<= (point) eol))
	      (setq alt-default t))
	 (setq i 0)
	 (while (setq i (string-match "[ \n\t]+" str i))
	   (setq str (concat (substring str 0 i) " "
			     (substring str (match-end 0))))
	   (setq i (1+ i)))
	 ;; Record as a completion and perhaps as default.
	 (if (eq default t) (setq default str))
	 (if (eq alt-default t) (setq alt-default str))
	 (setq completions
	       (cons (cons str nil)
		     completions))))
     ;; If no good default was found, try an alternate.
     (or default
	 (setq default alt-default))
     ;; If only one cross-reference found, then make it default.
     (if (eq (length completions) 1)
         (setq default (car (car completions))))
     (if completions
	 (let ((input (completing-read (if default
					   (concat "Follow reference named: ("
						   default ") ")
					 "Follow reference named: ")
				       completions nil t)))
	   (list (if (equal input "")
		     default input)))
       (error "No cross-references in this node"))))
  (let (target beg i (str (concat "\\*note " (regexp-quote footnotename))))
    (while (setq i (string-match " " str i))
      (setq str (concat (substring str 0 i) "[ \t\n]+" (substring str (1+ i))))
      (setq i (+ i 6)))
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward str nil t)
	  (error "No cross-reference named %s" footnotename))
      (goto-char (+ (match-beginning 0) 5))
      (setq target
	    (Info-extract-menu-node-name "Bad format cross reference" t)))
    (while (setq i (string-match "[ \t\n]+" target i))
      (setq target (concat (substring target 0 i) " "
			   (substring target (match-end 0))))
      (setq i (+ i 1)))
    (Info-goto-node target)))

(defun Info-extract-menu-node-name (&optional errmessage multi-line)
  (skip-chars-forward " \t\n")
  (let ((beg (point))
	str i)
    (skip-chars-forward "^:")
    (forward-char 1)
    (setq str
	  (if (looking-at ":")
	      (buffer-substring-no-properties beg (1- (point)))
	    (skip-chars-forward " \t\n")
	    (Info-following-node-name (if multi-line "^.,\t" "^.,\t\n"))))
    (while (setq i (string-match "\n" str i))
      (aset str i ?\ ))
    ;; Collapse multiple spaces.
    (while (string-match "  +" str)
      (setq str (replace-match " " t t str)))
    str))

;; No one calls this.
;;(defun Info-menu-item-sequence (list)
;;  (while list
;;    (Info-menu (car list))
;;    (setq list (cdr list))))

(defun Info-complete-menu-item (string predicate action)
  (let ((case-fold-search t))
    (cond ((eq action nil)
	   (let (completions
		 (pattern (concat "\n\\* \\("
				  (regexp-quote string)
				  "[^:\t\n]*\\):")))
	     (save-excursion
	       (set-buffer Info-complete-menu-buffer)
	       (goto-char (point-min))
	       (search-forward "\n* Menu:")
	       (while (re-search-forward pattern nil t)
		 (setq completions (cons (cons (format "%s"
						       (buffer-substring
							(match-beginning 1)
							(match-end 1)))
					       (match-beginning 1))
					 completions))))
	     (try-completion string completions predicate)))
	  ((eq action t)
	   (let (completions
		 (pattern (concat "\n\\* \\("
				  (regexp-quote string)
				  "[^:\t\n]*\\):")))
	     (save-excursion
	       (set-buffer Info-complete-menu-buffer)
	       (goto-char (point-min))
	       (search-forward "\n* Menu:")
	       (while (re-search-forward pattern nil t)
		 (setq completions (cons (cons (format "%s"
						       (buffer-substring
							(match-beginning 1)
							(match-end 1)))
					       (match-beginning 1))
					 completions))))
	     (all-completions string completions predicate)))
	  (t
	   (save-excursion
	     (set-buffer Info-complete-menu-buffer)
	     (goto-char (point-min))
	     (search-forward "\n* Menu:")
	     (re-search-forward (concat "\n\\* "
					(regexp-quote string)
					":")
				nil t))))))


(defun Info-menu (menu-item)
  "Go to node for menu item named (or abbreviated) NAME.
Completion is allowed, and the menu item point is on is the default."
  (interactive
   (let ((completions '())
	 ;; If point is within a menu item, use that item as the default
	 (default nil)
	 (p (point))
	 beg
	 (last nil))
     (save-excursion
       (goto-char (point-min))
       (if (not (search-forward "\n* menu:" nil t))
	   (error "No menu in this node"))
       (setq beg (point))
       (and (< (point) p)
	    (save-excursion
	      (goto-char p)
	      (end-of-line)
	      (re-search-backward "\n\\* \\([^:\t\n]*\\):" beg t)
	      (setq default (format "%s" (buffer-substring
					  (match-beginning 1)
					  (match-end 1)))))))
     (let ((item nil))
       (while (null item)
	 (setq item (let ((completion-ignore-case t)
			  (Info-complete-menu-buffer (current-buffer)))
		      (completing-read (if default
					   (format "Menu item (default %s): "
						   default)
					   "Menu item: ")
				       'Info-complete-menu-item nil t)))
	 ;; we rely on the fact that completing-read accepts an input
	 ;; of "" even when the require-match argument is true and ""
	 ;; is not a valid possibility
	 (if (string= item "")
	     (if default
		 (setq item default)
	         ;; ask again
	         (setq item nil))))
       (list item))))
  ;; there is a problem here in that if several menu items have the same
  ;; name you can only go to the node of the first with this command.
  (Info-goto-node (Info-extract-menu-item menu-item)))
  
(defun Info-extract-menu-item (menu-item)
  (setq menu-item (regexp-quote menu-item))
  (save-excursion
    (goto-char (point-min))
    (or (search-forward "\n* menu:" nil t)
	(error "No menu in this node"))
    (or (re-search-forward (concat "\n\\* " menu-item ":") nil t)
	(re-search-forward (concat "\n\\* " menu-item) nil t)
	(error "No such item in menu"))
    (beginning-of-line)
    (forward-char 2)
    (Info-extract-menu-node-name)))

;; If COUNT is nil, use the last item in the menu.
(defun Info-extract-menu-counting (count)
  (save-excursion
    (goto-char (point-min))
    (or (search-forward "\n* menu:" nil t)
	(error "No menu in this node"))
    (if count
	(or (search-forward "\n* " nil t count)
	    (error "Too few items in menu"))
      (while (search-forward "\n* " nil t)
	nil))
    (Info-extract-menu-node-name)))

(defun Info-nth-menu-item ()
  "Go to the node of the Nth menu item.
N is the digit argument used to invoke this command."
  (interactive)
  (Info-goto-node
   (Info-extract-menu-counting
    (- (aref (this-command-keys) (1- (length (this-command-keys)))) ?0))))

(defun Info-top-node ()
  "Go to the Top node of this file."
  (interactive)
  (Info-goto-node "Top"))

(defun Info-final-node ()
  "Go to the final node in this file."
  (interactive)
  (Info-goto-node "Top")
  (let (Info-history)
    ;; Go to the last node in the menu of Top.
    (Info-goto-node (Info-extract-menu-counting nil))
    ;; If the last node in the menu is not last in pointer structure,
    ;; move forward until we can't go any farther. 
    (while (Info-forward-node t t) nil)
    ;; Then keep moving down to last subnode, unless we reach an index.
    (while (and (not (string-match "\\<index\\>" Info-current-node))
		(save-excursion (search-forward "\n* Menu:" nil t)))
      (Info-goto-node (Info-extract-menu-counting nil)))))

(defun Info-forward-node (&optional not-down no-error)
  "Go forward one node, considering all nodes as forming one sequence."
  (interactive)
  (goto-char (point-min))
  (forward-line 1)
  ;; three possibilities, in order of priority:
  ;;     1. next node is in a menu in this node (but not in an index)
  ;;     2. next node is next at same level
  ;;     3. next node is up and next
  (cond ((and (not not-down)
              (save-excursion (search-forward "\n* menu:" nil t))
	      (not (string-match "\\<index\\>" Info-current-node)))
	 (Info-goto-node (Info-extract-menu-counting 1))
         t)
        ((save-excursion (search-backward "next:" nil t))
         (Info-next)
         t)
        ((and (save-excursion (search-backward "up:" nil t))
	      ;; Use string-equal, not equal, to ignore text props.
	      (not (string-equal (downcase (Info-extract-pointer "up"))
				 "top")))
         (let ((old-node Info-current-node))
           (Info-up)
           (let (Info-history success)
             (unwind-protect
                 (setq success (Info-forward-node t no-error))
               (or success (Info-goto-node old-node))))))
        (no-error nil)
        (t (error "No pointer forward from this node"))))

(defun Info-backward-node ()
  "Go backward one node, considering all nodes as forming one sequence."
  (interactive)
  (let ((prevnode (Info-extract-pointer "prev[ious]*" t))
	(upnode (Info-extract-pointer "up" t)))
    (cond ((and upnode (string-match "(" upnode))
	   (error "First node in file"))
	  ((and upnode (or (null prevnode)
			   ;; Use string-equal, not equal,
			   ;; to ignore text properties.
			   (string-equal (downcase prevnode)
					 (downcase upnode))))
	   (Info-up))
	  (prevnode
	   ;; If we move back at the same level,
	   ;; go down to find the last subnode*.
	   (Info-prev)
	   (let (Info-history)
	     (while (and (not (string-match "\\<index\\>" Info-current-node))
			 (save-excursion (search-forward "\n* Menu:" nil t)))
	       (Info-goto-node (Info-extract-menu-counting nil)))))
	  (t
	   (error "No pointer backward from this node")))))

(defun Info-exit ()
  "Exit Info by selecting some other buffer."
  (interactive)
  (if Info-standalone
      (save-buffers-kill-emacs)
    (switch-to-buffer (prog1 (other-buffer (current-buffer))
			(bury-buffer (current-buffer))))))

(defun Info-next-menu-item ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (search-forward "\n* menu:" nil t)
    (or (search-forward "\n* " nil t)
	(error "No more items in menu"))
    (Info-goto-node (Info-extract-menu-node-name))))

(defun Info-last-menu-item ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (let ((beg (save-excursion
		 (and (search-backward "\n* menu:" nil t)
		      (point)))))
      (or (and beg (search-backward "\n* " beg t))
	  (error "No previous items in menu")))
    (Info-goto-node (save-excursion
		      (goto-char (match-end 0))
		      (Info-extract-menu-node-name)))))

(defmacro Info-no-error (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(t))) '(error nil)))

(defun Info-next-preorder ()
  "Go to the next subnode or the next node, or go up a level."
  (interactive)
  (cond ((Info-no-error (Info-next-menu-item)))
	((Info-no-error (Info-next)))
	((Info-no-error (Info-up))
	 ;; Since we have already gone thru all the items in this menu,
	 ;; go up to the end of this node.
	 (goto-char (point-max))
	 ;; Since logically we are done with the node with that menu,
	 ;; move on from it.
	 (Info-next-preorder))
	(t
	 (error "No more nodes"))))

(defun Info-last-preorder ()
  "Go to the last node, popping up a level if there is none."
  (interactive)
  (cond ((Info-no-error
	  (Info-last-menu-item)
	  ;; If we go down a menu item, go to the end of the node
	  ;; so we can scroll back through it.
	  (goto-char (point-max)))
	 ;; Keep going down, as long as there are nested menu nodes.
	 (while (Info-no-error
		 (Info-last-menu-item)
		 ;; If we go down a menu item, go to the end of the node
		 ;; so we can scroll back through it.
		 (goto-char (point-max))))
	 (recenter -1))
	((Info-no-error (Info-prev))
	 (goto-char (point-max))
	 (while (Info-no-error
		 (Info-last-menu-item)
		 ;; If we go down a menu item, go to the end of the node
		 ;; so we can scroll back through it.
		 (goto-char (point-max))))
	 (recenter -1))
	((Info-no-error (Info-up))
	 (goto-char (point-min))
	 (or (search-forward "\n* Menu:" nil t)
	     (goto-char (point-max))))
	(t (error "No previous nodes"))))

(defun Info-scroll-up ()
  "Scroll one screenful forward in Info, considering all nodes as one sequence.
Once you scroll far enough in a node that its menu appears on the screen,
the next scroll moves into its first subnode.  When you scroll past
the end of a node, that goes to the next node or back up to the parent node."
  (interactive)
  (if (or (< (window-start) (point-min))
	  (> (window-start) (point-max)))
      (set-window-start (selected-window) (point)))
  (let ((virtual-end (save-excursion
		       (goto-char (point-min))
		       (if (search-forward "\n* Menu:" nil t)
			   (point)
			 (point-max)))))
    (if (or (< virtual-end (window-start))
	    (pos-visible-in-window-p virtual-end))
	(Info-next-preorder)
      (scroll-up))))

(defun Info-scroll-down ()
  "Scroll one screenful back in Info, considering all nodes as one sequence.
Within the menu of a node, this goes to its last subnode.
When you scroll past the beginning of a node, that goes to the
previous node or back up to the parent node."
  (interactive)
  (if (or (< (window-start) (point-min))
	  (> (window-start) (point-max)))
      (set-window-start (selected-window) (point)))
  (let* ((current-point (point))
	 (virtual-end (save-excursion
			(beginning-of-line)
			(setq current-point (point))
			(goto-char (point-min))
			(search-forward "\n* Menu:"
					current-point
					t))))
    (if (or virtual-end (pos-visible-in-window-p (point-min)))
	(Info-last-preorder)
      (scroll-down))))

(defun Info-next-reference (&optional recur)
  "Move cursor to the next cross-reference or menu item in the node."
  (interactive)
  (let ((pat "\\*note[ \n\t]*\\([^:]*\\):\\|^\\* .*:")
	(old-pt (point)))
    (or (eobp) (forward-char 1))
    (or (re-search-forward pat nil t)
	(progn
	  (goto-char (point-min))
	  (or (re-search-forward pat nil t)
	      (progn
		(goto-char old-pt)
		(error "No cross references in this node")))))
    (goto-char (match-beginning 0))
    (if (looking-at "\\* Menu:")
	(if recur
	    (error "No cross references in this node")
	  (Info-next-reference t)))))

(defun Info-prev-reference (&optional recur)
  "Move cursor to the previous cross-reference or menu item in the node."
  (interactive)
  (let ((pat "\\*note[ \n\t]*\\([^:]*\\):\\|^\\* .*:")
	(old-pt (point)))
    (or (re-search-backward pat nil t)
	(progn
	  (goto-char (point-max))
	  (or (re-search-backward pat nil t)
	      (progn
		(goto-char old-pt)
		(error "No cross references in this node")))))
    (goto-char (match-beginning 0))
    (if (looking-at "\\* Menu:")
	(if recur
	    (error "No cross references in this node")
	  (Info-prev-reference t)))))

(defun Info-index (topic)
  "Look up a string in the index for this file.
The index is defined as the first node in the top-level menu whose
name contains the word \"Index\", plus any immediately following
nodes whose names also contain the word \"Index\".
If there are no exact matches to the specified topic, this chooses
the first match which is a case-insensitive substring of a topic.
Use the `,' command to see the other matches.
Give a blank topic name to go to the Index node itself."
  (interactive "sIndex topic: ")
  (let ((orignode Info-current-node)
	(rnode nil)
	(pattern (format "\n\\* \\([^\n:]*%s[^\n:]*\\):[ \t]*\\([^.\n]*\\)\\.[ \t]*\\([0-9]*\\)"
			 (regexp-quote topic)))
	node)
    (Info-goto-node "Top")
    (or (search-forward "\n* menu:" nil t)
	(error "No index"))
    (or (re-search-forward "\n\\* \\(.*\\<Index\\>\\)" nil t)
	(error "No index"))
    (goto-char (match-beginning 1))
    ;; Here, and subsequently in this function,
    ;; we bind Info-history to nil for internal node-switches
    ;; so that we don't put junk in the history.
    ;; In the first Info-goto-node call, above, we do update the history
    ;; because that is what the user's previous node choice into it.
    (let ((Info-history nil))
      (Info-goto-node (Info-extract-menu-node-name)))
    (or (equal topic "")
	(let ((matches nil)
	      (exact nil)
	      (Info-history nil)
	      found)
	  (while
	      (progn
		(goto-char (point-min))
		(while (re-search-forward pattern nil t)
		  (setq matches
			(cons (list (buffer-substring (match-beginning 1)
						      (match-end 1))
				    (buffer-substring (match-beginning 2)
						      (match-end 2))
				    Info-current-node
				    (string-to-int (concat "0"
							   (buffer-substring
							    (match-beginning 3)
							    (match-end 3)))))
			      matches)))
		(and (setq node (Info-extract-pointer "next" t))
		     (string-match "\\<Index\\>" node)))
	    (Info-goto-node node))
	  (or matches
	      (progn
		(Info-goto-node orignode)
		(error "No `%s' in index" topic)))
	  ;; Here it is a feature that assoc is case-sensitive.
	  (while (setq found (assoc topic matches))
	    (setq exact (cons found exact)
		  matches (delq found matches)))
	  (setq Info-index-alternatives (nconc exact (nreverse matches)))
	  (Info-index-next 0)))))

(defun Info-index-next (num)
  "Go to the next matching index item from the last `i' command."
  (interactive "p")
  (or Info-index-alternatives
      (error "No previous `i' command in this file"))
  (while (< num 0)
    (setq num (+ num (length Info-index-alternatives))))
  (while (> num 0)
    (setq Info-index-alternatives
	  (nconc (cdr Info-index-alternatives)
		 (list (car Info-index-alternatives)))
	  num (1- num)))
  (Info-goto-node (nth 1 (car Info-index-alternatives)))
  (if (> (nth 3 (car Info-index-alternatives)) 0)
      (forward-line (nth 3 (car Info-index-alternatives)))
    (forward-line 3)  ; don't search in headers
    (let ((name (car (car Info-index-alternatives))))
      (Info-find-index-name name)))
  (message "Found `%s' in %s.  %s"
	   (car (car Info-index-alternatives))
	   (nth 2 (car Info-index-alternatives))
	   (if (cdr Info-index-alternatives)
	       "(Press `,' for more)"
	     "(Only match)")))

(defun Info-find-index-name (name)
  "Move point to the place within the current node where NAME is defined."
  (if (or (re-search-forward (format
			      "[a-zA-Z]+: %s\\( \\|$\\)"
			      (regexp-quote name)) nil t)
	  (search-forward (format "`%s'" name) nil t)
	  (and (string-match "\\`.*\\( (.*)\\)\\'" name)
	       (search-forward
		(format "`%s'" (substring name 0 (match-beginning 1)))
		nil t))
	  (search-forward name nil t))
      (beginning-of-line)
    (goto-char (point-min))))

(defun Info-undefined ()
  "Make command be undefined in Info."
  (interactive)
  (ding))

(defun Info-help ()
  "Enter the Info tutorial."
  (interactive)
  (delete-other-windows)
  (Info-find-node "info"
		  (if (< (window-height) 23)
		      "Help-Small-Screen"
		    "Help")))

(defun Info-summary ()
  "Display a brief summary of all Info commands."
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Help*")
    (erase-buffer)
    (insert (documentation 'Info-mode))
    (help-mode)
    (goto-char (point-min))
    (let (ch flag)
      (while (progn (setq flag (not (pos-visible-in-window-p (point-max))))
		    (message (if flag "Type Space to see more"
			       "Type Space to return to Info"))
		    (if (not (eq ?\  (setq ch (read-event))))
			(progn (setq unread-command-events (list ch)) nil)
		      flag))
	(scroll-up)))
    (bury-buffer "*Help*")))

(defun Info-get-token (pos start all &optional errorstring)
  "Return the token around POS,
POS must be somewhere inside the token
START is a regular expression which will match the
    beginning of the tokens delimited string
ALL is a regular expression with a single
    parenthesized subpattern which is the token to be
    returned. E.g. '{\(.*\)}' would return any string
    enclosed in braces around POS.
SIG optional fourth argument, controls action on no match
    nil: return nil
    t: beep
    a string: signal an error, using that string."
  (save-excursion
    (goto-char pos)
    (re-search-backward start (max (point-min) (- pos 200)) 'yes)
    (let (found)
      (while (and (re-search-forward all (min (point-max) (+ pos 200)) 'yes)
		  (not (setq found (and (<= (match-beginning 0) pos)
					(> (match-end 0) pos))))))
      (if (and found (<= (match-beginning 0) pos)
	       (> (match-end 0) pos))
	  (buffer-substring (match-beginning 1) (match-end 1))
	(cond ((null errorstring)
	       nil)
	      ((eq errorstring t)
	       (beep)
	       nil)
	      (t
	       (error "No %s around position %d" errorstring pos)))))))

(defun Info-mouse-follow-nearest-node (click)
  "\\<Info-mode-map>Follow a node reference near point.
Like \\[Info-menu], \\[Info-follow-reference], \\[Info-next], \\[Info-prev] or \\[Info-up] command, depending on where you click.
At end of the node's text, moves to the next node, or up if none."
  (interactive "e")
  (let* ((start (event-start click))
	 (window (car start))
	 (pos (car (cdr start))))
    (select-window window)
    (goto-char pos))
  (and (not (Info-try-follow-nearest-node))
       (save-excursion (forward-line 1) (eobp))
       (Info-next-preorder)))

(defun Info-follow-nearest-node ()
  "\\<Info-mode-map>Follow a node reference near point.
Like \\[Info-menu], \\[Info-follow-reference], \\[Info-next], \\[Info-prev] or \\[Info-up] command, depending on where point is.
If no reference to follow, moves to the next node, or up if none."
  (interactive)
  (or (Info-try-follow-nearest-node)
      (Info-next-preorder)))

;; Common subroutine.
(defun Info-try-follow-nearest-node ()
  "Follow a node reference near point.  Return non-nil if successful."
  (let (node)
    (cond
     ((setq node (Info-get-token (point) "\\*note[ \n]"
				 "\\*note[ \n]\\([^:]*\\):"))
      (Info-follow-reference node))
     ((setq node (Info-get-token (point) "\\* " "\\* \\([^:]*\\)::"))
      (Info-goto-node node))
     ((setq node (Info-get-token (point) "\\* " "\\* \\([^:]*\\):"))
      (Info-menu node))
     ((setq node (Info-get-token (point) "Up: " "Up: \\([^,\n\t]*\\)"))
      (Info-goto-node node))
     ((setq node (Info-get-token (point) "Next: " "Next: \\([^,\n\t]*\\)"))
      (Info-goto-node node))
     ((setq node (Info-get-token (point) "File: " "File: \\([^,\n\t]*\\)"))
      (Info-goto-node "Top"))
     ((setq node (Info-get-token (point) "Prev: " "Prev: \\([^,\n\t]*\\)"))
      (Info-goto-node node)))
    node))

(defvar Info-mode-map nil
  "Keymap containing Info commands.")
(if Info-mode-map
    nil
  (setq Info-mode-map (make-keymap))
  (suppress-keymap Info-mode-map)
  (define-key Info-mode-map "." 'beginning-of-buffer)
  (define-key Info-mode-map " " 'Info-scroll-up)
  (define-key Info-mode-map "\C-m" 'Info-follow-nearest-node)
  (define-key Info-mode-map "\t" 'Info-next-reference)
  (define-key Info-mode-map "\e\t" 'Info-prev-reference)
  (define-key Info-mode-map "1" 'Info-nth-menu-item)
  (define-key Info-mode-map "2" 'Info-nth-menu-item)
  (define-key Info-mode-map "3" 'Info-nth-menu-item)
  (define-key Info-mode-map "4" 'Info-nth-menu-item)
  (define-key Info-mode-map "5" 'Info-nth-menu-item)
  (define-key Info-mode-map "6" 'Info-nth-menu-item)
  (define-key Info-mode-map "7" 'Info-nth-menu-item)
  (define-key Info-mode-map "8" 'Info-nth-menu-item)
  (define-key Info-mode-map "9" 'Info-nth-menu-item)
  (define-key Info-mode-map "0" 'undefined)
  (define-key Info-mode-map "?" 'Info-summary)
  (define-key Info-mode-map "]" 'Info-forward-node)
  (define-key Info-mode-map "[" 'Info-backward-node)
  (define-key Info-mode-map "<" 'Info-top-node)
  (define-key Info-mode-map ">" 'Info-final-node)
  (define-key Info-mode-map "b" 'beginning-of-buffer)
  (define-key Info-mode-map "d" 'Info-directory)
  (define-key Info-mode-map "e" 'Info-edit)
  (define-key Info-mode-map "f" 'Info-follow-reference)
  (define-key Info-mode-map "g" 'Info-goto-node)
  (define-key Info-mode-map "h" 'Info-help)
  (define-key Info-mode-map "i" 'Info-index)
  (define-key Info-mode-map "l" 'Info-last)
  (define-key Info-mode-map "m" 'Info-menu)
  (define-key Info-mode-map "n" 'Info-next)
  (define-key Info-mode-map "p" 'Info-prev)
  (define-key Info-mode-map "q" 'Info-exit)
  (define-key Info-mode-map "s" 'Info-search)
  ;; For consistency with Rmail.
  (define-key Info-mode-map "\M-s" 'Info-search)
  (define-key Info-mode-map "t" 'Info-top-node)
  (define-key Info-mode-map "u" 'Info-up)
  (define-key Info-mode-map "," 'Info-index-next)
  (define-key Info-mode-map "\177" 'Info-scroll-down)
  (define-key Info-mode-map [mouse-2] 'Info-mouse-follow-nearest-node)
  )

;; Info mode is suitable only for specially formatted data.
(put 'info-mode 'mode-class 'special)

(defun Info-mode ()
  "\\<Info-mode-map>
Info mode provides commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which discusses
one topic and contains references to other nodes which discuss related
topics.  Info has commands to follow the references and show you other nodes.

\\[Info-help]	Invoke the Info tutorial.

Selecting other nodes:
\\[Info-mouse-follow-nearest-node]
	Follow a node reference you click on.
	  This works with menu items, cross references, and
	  the \"next\", \"previous\" and \"up\", depending on where you click.
\\[Info-next]	Move to the \"next\" node of this node.
\\[Info-prev]	Move to the \"previous\" node of this node.
\\[Info-up]	Move \"up\" from this node.
\\[Info-menu]	Pick menu item specified by name (or abbreviation).
	Picking a menu item causes another node to be selected.
\\[Info-directory]	Go to the Info directory node.
\\[Info-follow-reference]	Follow a cross reference.  Reads name of reference.
\\[Info-last]	Move to the last node you were at.
\\[Info-index]	Look up a topic in this file's Index and move to that node.
\\[Info-index-next]	(comma) Move to the next match from a previous `i' command.

Moving within a node:
\\[Info-scroll-up]	Normally, scroll forward a full screen.  If the end of the buffer is
already visible, try to go to the next menu entry, or up if there is none.
\\[Info-scroll-down]  Normally, scroll backward.  If the beginning of the buffer is
already visible, try to go to the previous menu entry, or up if there is none.
\\[beginning-of-buffer]	Go to beginning of node.  

Advanced commands:
\\[Info-exit]	Quit Info: reselect previously selected buffer.
\\[Info-edit]	Edit contents of selected node.
1	Pick first item in node's menu.
2, 3, 4, 5   Pick second ... fifth item in node's menu.
\\[Info-goto-node]	Move to node specified by name.
	You may include a filename as well, as (FILENAME)NODENAME.
\\[universal-argument] \\[info]	Move to new Info file with completion.
\\[Info-search]	Search through this Info file for specified regexp,
	and select the node in which the next occurrence is found.
\\[Info-next-reference]	Move cursor to next cross-reference or menu item.
\\[Info-prev-reference]	Move cursor to previous cross-reference or menu item."
  (kill-all-local-variables)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (use-local-map Info-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (make-local-variable 'Info-current-file)
  (make-local-variable 'Info-current-subfile)
  (make-local-variable 'Info-current-node)
  (make-local-variable 'Info-tag-table-marker)
  (make-local-variable 'Info-history)
  (make-local-variable 'Info-index-alternatives)
  (if (memq (framep (selected-frame)) '(x pc))
      (progn
	(make-face 'info-node)
	(make-face 'info-menu-5)
	(make-face 'info-xref)
	(or (face-differs-from-default-p 'info-node)
	    (if (face-differs-from-default-p 'bold-italic)
		(copy-face 'bold-italic 'info-node)
	      (copy-face 'bold 'info-node)))
	(or (face-differs-from-default-p 'info-menu-5)
	    (set-face-underline-p 'info-menu-5 t))
	(or (face-differs-from-default-p 'info-xref)
	    (copy-face 'bold 'info-xref)))
    (setq Info-fontify nil))
  (Info-set-mode-line)
  (run-hooks 'Info-mode-hook))

(defvar Info-edit-map nil
  "Local keymap used within `e' command of Info.")
(if Info-edit-map
    nil
  (setq Info-edit-map (nconc (make-sparse-keymap) text-mode-map))
  (define-key Info-edit-map "\C-c\C-c" 'Info-cease-edit))

;; Info-edit mode is suitable only for specially formatted data.
(put 'info-edit-mode 'mode-class 'special)

(defun Info-edit-mode ()
  "Major mode for editing the contents of an Info node.
Like text mode with the addition of `Info-cease-edit'
which returns to Info mode for browsing.
\\{Info-edit-map}"
  (use-local-map Info-edit-map)
  (setq major-mode 'Info-edit-mode)
  (setq mode-name "Info Edit")
  (kill-local-variable 'mode-line-buffer-identification)
  (setq buffer-read-only nil)
  (force-mode-line-update)
  (buffer-enable-undo (current-buffer))
  (run-hooks 'Info-edit-mode-hook))

(defun Info-edit ()
  "Edit the contents of this Info node.
Allowed only if variable `Info-enable-edit' is non-nil."
  (interactive)
  (or Info-enable-edit
      (error "Editing info nodes is not enabled"))
  (Info-edit-mode)
  (message "%s" (substitute-command-keys
	    "Editing: Type \\<Info-edit-map>\\[Info-cease-edit] to return to info")))

(defun Info-cease-edit ()
  "Finish editing Info node; switch back to Info proper."
  (interactive)
  ;; Do this first, so nothing has changed if user C-g's at query.
  (and (buffer-modified-p)
       (y-or-n-p "Save the file? ")
       (save-buffer))
  (use-local-map Info-mode-map)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (Info-set-mode-line)
  (setq buffer-read-only t)
  (force-mode-line-update)
  (and (marker-position Info-tag-table-marker)
       (buffer-modified-p)
       (message "Tags may have changed.  Use Info-tagify if necessary")))

(defvar Info-file-list-for-emacs
  '("ediff" "forms" "gnus" "info" ("mh" . "mh-e") "sc")
  "List of Info files that describe Emacs commands.
An element can be a file name, or a list of the form (PREFIX . FILE)
where PREFIX is a name prefix and FILE is the file to look in.
If the element is just a file name, the file name also serves as the prefix.")

(defun Info-find-emacs-command-nodes (command)
  "Return a list of locations documenting COMMAND.
The `info-file' property of COMMAND says which Info manual to search.
If COMMAND has no property, the variable `Info-file-list-for-emacs'
defines heuristics for which Info manual to try.
The locations are of the format used in Info-history, i.e.
\(FILENAME NODENAME BUFFERPOS\)."
  (let ((where '())
	(cmd-desc (concat "^\\* " (regexp-quote (symbol-name command))
			  ":\\s *\\(.*\\)\\.$"))
	(info-file "emacs"))		;default
    ;; Determine which info file this command is documented in.
    (if (get command 'info-file)
	(setq info-file (get command 'info-file))
      ;; If it doesn't say explicitly, test its name against
      ;; various prefixes that we know.
      (let ((file-list Info-file-list-for-emacs))
	(while file-list
	  (let* ((elt (car file-list))
		 (name (if (consp elt)
			   (car elt)
			 elt))
		 (file (if (consp elt) (cdr elt) elt))
		 (regexp (concat "\\`" (regexp-quote name)
				 "\\(\\'\\|-\\)")))
	    (if (string-match regexp (symbol-name command))
		(setq info-file file file-list nil))
	    (setq file-list (cdr file-list))))))
    (save-excursion
      (condition-case nil
	  (Info-find-node info-file "Command Index")
	;; Some manuals may not have a separate Command Index node,
	;; so try just Index instead.
	(error
	 (Info-find-node info-file "Index")))
      ;; Take the index node off the Info history.
      (setq Info-history (cdr Info-history))
      (goto-char (point-max))
      (while (re-search-backward cmd-desc nil t)
	  (setq where (cons (list Info-current-file
				  (buffer-substring
				   (match-beginning 1)
				   (match-end 1))
				  0)
			    where)))
      where)))

;;;###autoload
(defun Info-goto-emacs-command-node (command)
  "Go to the Info node in the Emacs manual for command COMMAND.
The command is found by looking up in Emacs manual's Command Index
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'."
  (interactive "CFind documentation for command: ")
  (or (commandp command)
      (signal 'wrong-type-argument (list 'commandp command)))
  (let ((where (Info-find-emacs-command-nodes command)))
    (if where
	(let ((num-matches (length where)))
	  ;; Get Info running, and pop to it in another window.
	  (save-window-excursion
	    (info))
	  (pop-to-buffer "*info*")
	  (Info-find-node (car (car where))
			  (car (cdr (car where))))
	  (if (> num-matches 1)
	      (progn
		;; Info-find-node already pushed (car where) onto
		;; Info-history.  Put the other nodes that were found on
		;; the history.
		(setq Info-history (nconc (cdr where) Info-history))
		(message "Found %d other entr%s.  Use %s to see %s."
			 (1- num-matches)
			 (if (> num-matches 2) "ies" "y")
			 (substitute-command-keys "\\[Info-last]")
			 (if (> num-matches 2) "them" "it")))))
      (error "Couldn't find documentation for %s" command))))

;;;###autoload
(defun Info-goto-emacs-key-command-node (key)
  "Go to the Info node in the Emacs manual the command bound to KEY, a string.
Interactively, if the binding is execute-extended-command, a command is read.
The command is found by looking up in Emacs manual's Command Index
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'."
  (interactive "kFind documentation for key:")
  (let ((command (key-binding key)))
    (cond ((null command)
	   (message "%s is undefined" (key-description key)))
	  ((and (interactive-p)
		(eq command 'execute-extended-command))
	   (Info-goto-emacs-command-node
	    (read-command "Find documentation for command: ")))
	  (t
	   (Info-goto-emacs-command-node command)))))

(defvar Info-title-face-alist
  '((?* bold underline)
    (?= bold-italic underline)
    (?- italic underline))
  "*Alist of face or list of faces to use for pseudo-underlined titles.
The alist key is the character the title is underlined with (?*, ?= or ?-).")

(defun Info-fontify-node ()
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (if (looking-at "^File: [^,: \t]+,?[ \t]+")
	  (progn
	    (goto-char (match-end 0))
	    (while
		(looking-at "[ \t]*[^:, \t\n]+:[ \t]+\\([^:,\t\n]+\\),?")
	      (goto-char (match-end 0))
	      (put-text-property (match-beginning 1) (match-end 1)
				 'face 'info-xref)
	      (put-text-property (match-beginning 1) (match-end 1)
				 'mouse-face 'highlight))))
      (goto-char (point-min))
      (while (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*+\\|=+\\|-+\\)$"
				 nil t)
	(put-text-property (match-beginning 1) (match-end 1)
			   'face
			   (cdr (assq (preceding-char) Info-title-face-alist)))
	(put-text-property (match-end 1) (match-end 2)
			   'invisible t))
      (goto-char (point-min))
      (while (re-search-forward "\\*Note[ \n\t]+\\([^:]*\\):" nil t)
	(if (= (char-after (1- (match-beginning 0))) ?\") ; hack
	    nil
	  (put-text-property (match-beginning 1) (match-end 1)
			     'face 'info-xref)
	  (put-text-property (match-beginning 1) (match-end 1)
			     'mouse-face 'highlight)))
      (goto-char (point-min))
      (if (and (search-forward "\n* Menu:" nil t)
	       (not (string-match "\\<Index\\>" Info-current-node))
	       ;; Don't take time to annotate huge menus
	       (< (- (point-max) (point)) Info-fontify-maximum-menu-size))
	  (let ((n 0))
	    (while (re-search-forward "^\\* \\([^:\t\n]*\\):" nil t)
	      (setq n (1+ n))
	      (if (memq n '(5 9))   ; visual aids to help with 1-9 keys
		  (put-text-property (match-beginning 0)
				     (1+ (match-beginning 0))
				     'face 'info-menu-5))
	      (put-text-property (match-beginning 1) (match-end 1)
				 'face 'info-node)
	      (put-text-property (match-beginning 1) (match-end 1)
				 'mouse-face 'highlight))))
      (set-buffer-modified-p nil))))

(provide 'info)

;;; info.el ends here
