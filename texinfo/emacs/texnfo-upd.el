;;; texnfo-upd.el --- utilities for updating nodes and menus in Texinfo files

;; Copyright 1989, 1990, 1991, 1992, 1996 Free Software Foundation, Inc.

;; Author: Robert J. Chassell      
;; Date:   12 Sep 1996
;; Maintainer: Robert J. Chassell <bug-texinfo@prep.ai.mit.edu>
;; Keywords: maint, tex, docs

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

;; Known bug: update commands fail to ignore @ignore.

;; Summary: how to use the updating commands

;; The node and menu updating functions automatically

;;   * insert missing `@node' lines,
;;   * insert the `Next', `Previous' and `Up' pointers of a node,
;;   * insert or update the menu for a section, 
;;   * create a master menu for a Texinfo source file.
;;
;; Passed an argument, the `texinfo-update-node' and
;; `texinfo-make-menu' functions do their jobs in the region.
;;
;; In brief, the functions for creating or updating nodes and menus, are:
;; 
;;     texinfo-update-node (&optional region-p)            
;;     texinfo-every-node-update ()                        
;;     texinfo-sequential-node-update (&optional region-p)
;; 
;;     texinfo-make-menu (&optional region-p)              
;;     texinfo-all-menus-update ()                         
;;     texinfo-master-menu ()
;;
;;     texinfo-insert-node-lines  (&optional title-p)
;; 
;;     texinfo-indent-menu-description (column &optional region-p)

;; The `texinfo-column-for-description' variable specifies the column to
;; which menu descriptions are indented. 

;; Texinfo file structure
;; ----------------------

;; To use the updating commands, you must structure your Texinfo file
;; hierarchically.  Each `@node' line, with the exception of the top
;; node, must be accompanied by some kind of section line, such as an
;; `@chapter' or `@section' line.  Each node-line/section-line
;; combination must look like this:

;;      @node    Lists and Tables, Cross References, Structuring, Top
;;      @comment node-name,        next,             previous,    up
;;      @chapter Making Lists and Tables

;; or like this (without the `@comment' line):

;;      @node    Lists and Tables, Cross References, Structuring, Top
;;      @chapter Making Lists and Tables

;; If the file has a `top' node, it must be called `top' or `Top' and
;; be the first node in the file.


;;; The update node functions described in detail

;; The `texinfo-update-node' function without an argument inserts
;; the correct next, previous and up pointers for the node in which
;; point is located (i.e., for the node preceding point).

;; With an argument, the `texinfo-update-node' function inserts the
;; correct next, previous and up pointers for the nodes inside the
;; region.

;; It does not matter whether the  `@node' line has pre-existing
;; `Next', `Previous', or `Up' pointers in it.  They are removed.

;; The `texinfo-every-node-update' function runs `texinfo-update-node'
;; on the whole buffer.

;; The `texinfo-sequential-node-update' function inserts the
;; immediately following and preceding node into the `Next' or
;; `Previous' pointers regardless of their hierarchical level.  This is
;; only useful for certain kinds of text, like a novel, which you go
;; through sequentially.


;;; The menu making functions described in detail

;; The `texinfo-make-menu' function without an argument creates or
;; updates a menu for the section encompassing the node that follows
;; point.  With an argument, it makes or updates menus for the nodes
;; within or part of the marked region.

;; Whenever an existing menu is updated, the descriptions from
;; that menu are incorporated into the new menu.  This is done by copying
;; descriptions from the existing menu to the entries in the new menu
;; that have the same node names.  If the node names are different, the
;; descriptions are not copied to the new menu.

;; Menu entries that refer to other Info files are removed since they
;; are not a node within current buffer.  This is a deficiency.

;; The `texinfo-all-menus-update' function runs `texinfo-make-menu'
;; on the whole buffer.

;; The `texinfo-master-menu' function creates an extended menu located
;; after the top node.  (The file must have a top node.)  The function
;; first updates all the regular menus in the buffer (incorporating the
;; descriptions from pre-existing menus), and then constructs a master
;; menu that includes every entry from every other menu.  (However, the
;; function cannot update an already existing master menu; if one
;; exists, it must be removed before calling the function.)

;; The `texinfo-indent-menu-description' function indents every
;; description in the menu following point, to the specified column.
;; Non-nil argument (prefix, if interactive) means indent every
;; description in every menu in the region.  This function does not
;; indent second and subsequent lines of a multi-line description.

;; The `texinfo-insert-node-lines' function inserts `@node' before the
;; `@chapter', `@section', and such like lines of a region in a Texinfo
;; file where the `@node' lines are missing.
;; 
;; With a non-nil argument (prefix, if interactive), the function not
;; only inserts `@node' lines but also inserts the chapter or section
;; titles as the names of the corresponding nodes; and inserts titles
;; as node names in pre-existing `@node' lines that lack names.
;; 
;; Since node names should be more concise than section or chapter
;; titles, node names so inserted will need to be edited manually.

 
;;; Code:

;;; The menu making functions

(defun texinfo-make-menu (&optional region-p)
  "Without any prefix argument, make or update a menu.
Make the menu for the section enclosing the node found following point.

Non-nil argument (prefix, if interactive) means make or update menus
for nodes within or part of the marked region.

Whenever a menu exists, and is being updated, the descriptions that
are associated with node names in the pre-existing menu are
incorporated into the new menu.  Otherwise, the nodes' section titles
are inserted as descriptions."
  
  (interactive "P")
  (if (not region-p)
      (let ((level (texinfo-hierarchic-level)))
        (texinfo-make-one-menu level)
        (message "Done...updated the menu.  You may save the buffer."))
    ;; else
    (message "Making or updating menus in %s... " (buffer-name))
    (let ((beginning (region-beginning))
          (region-end (region-end))
          (level (progn         ; find section type following point
                   (goto-char (region-beginning))
                   (texinfo-hierarchic-level))))
      (if (= region-end beginning)
          (error "Please mark a region!"))
      (save-excursion
        (save-restriction
          (widen)
          
          (while  (texinfo-find-lower-level-node level region-end)
            (setq level (texinfo-hierarchic-level)) ; new, lower level
            (texinfo-make-one-menu level))
          
          (while (and (< (point) region-end)
                      (texinfo-find-higher-level-node level region-end))
            (setq level (texinfo-hierarchic-level))
            (while (texinfo-find-lower-level-node level region-end)
              (setq level (texinfo-hierarchic-level)) ; new, lower level
              (texinfo-make-one-menu level))))))
    (message "Done...updated menus.  You may save the buffer.")))

(defun texinfo-make-one-menu (level)
  "Make a menu of all the appropriate nodes in this section.
`Appropriate nodes' are those associated with sections that are 
at the level specified by LEVEL.  Point is left at the end of menu."
  (let*
      ((case-fold-search t)
       (beginning
        (save-excursion
          (goto-char (texinfo-update-menu-region-beginning level))
          (end-of-line)
          (point)))
       (end (texinfo-update-menu-region-end level))
       (first (texinfo-menu-first-node beginning end))
       (node-name (progn
                    (goto-char beginning)
                    (beginning-of-line)
                    (texinfo-copy-node-name)))
       (new-menu-list (texinfo-make-menu-list beginning end level)))
    (if (texinfo-old-menu-p beginning first)
        (progn
          (texinfo-incorporate-descriptions new-menu-list)
          (texinfo-incorporate-menu-entry-names new-menu-list)
          (texinfo-delete-old-menu beginning first)))
    (texinfo-insert-menu new-menu-list node-name)))

(defun texinfo-all-menus-update (&optional update-all-nodes-p)
  "Update every regular menu in a Texinfo file.
Update pre-existing master menu, if there is one.

If called with a non-nil argument, this function first updates all the
nodes in the buffer before updating the menus."
  (interactive "P")
  (let ((case-fold-search t)
        master-menu-p)
    (save-excursion
      (push-mark (point-max) t)
      (goto-char (point-min))
      (message "Checking for a master menu in %s ... "(buffer-name))
      (save-excursion
        (if (re-search-forward texinfo-master-menu-header nil t)
            ;; Remove detailed master menu listing
            (progn
              (setq master-menu-p t)
              (goto-char (match-beginning 0))
              (let ((end-of-detailed-menu-descriptions
                     (save-excursion     ; beginning of end menu line
                       (goto-char (texinfo-menu-end))
                       (beginning-of-line) (forward-char -1)
                       (point))))
                (delete-region (point) end-of-detailed-menu-descriptions)))))
      
      (if update-all-nodes-p
          (progn
            (message "Updating all nodes in %s ... " (buffer-name))
            (sleep-for 2)
            (push-mark (point-max) t)
            (goto-char (point-min))
            ;; Using the mark to pass bounds this way
            ;; is kludgy, but it's not worth fixing. -- rms.
            (let ((mark-active t))
              (texinfo-update-node t))))
      
      (message "Updating all menus in %s ... " (buffer-name))        
      (sleep-for 2)
      (push-mark (point-max) t)
      (goto-char (point-min))
      ;; Using the mark to pass bounds this way
      ;; is kludgy, but it's not worth fixing. -- rms.
      (let ((mark-active t))
        (texinfo-make-menu t))
      
      (if master-menu-p
          (progn
            (message "Updating the master menu in %s... " (buffer-name))
            (sleep-for 2)
            (texinfo-master-menu nil))))
    
    (message "Done...updated all the menus.  You may save the buffer.")))

(defun texinfo-find-lower-level-node (level region-end)
  "Search forward from point for node at any level lower than LEVEL.
Search is limited to the end of the marked region, REGION-END, 
and to the end of the menu region for the level.

Return t if the node is found, else nil.  Leave point at the beginning
of the node if one is found; else do not move point."
  (let ((case-fold-search t))
    (if (and (< (point) region-end)
             (re-search-forward
              (concat
               "\\(^@node\\).*\n"         ; match node line
               "\\(\\(\\(^@c\\).*\n\\)"   ; match comment line, if any
               "\\|"                      ; or
               "\\(^@ifinfo[ ]*\n\\)\\)?" ; ifinfo line, if any
               (eval (cdr (assoc level texinfo-update-menu-lower-regexps))))
              ;; the next higher level node marks the end of this
              ;; section, and no lower level node will be found beyond
              ;; this position even if region-end is farther off
              (texinfo-update-menu-region-end level) 
              t))
        (goto-char (match-beginning 1)))))

(defun texinfo-find-higher-level-node (level region-end)
  "Search forward from point for node at any higher level than argument LEVEL.
Search is limited to the end of the marked region, REGION-END.

Return t if the node is found, else nil.  Leave point at the beginning
of the node if one is found; else do not move point."
  (let ((case-fold-search t))
    (cond
     ((or (string-equal "top" level) (string-equal "chapter" level))
      (if (re-search-forward "^@node [ \t]*top[ \t]*\\(,\\|$\\)" region-end t)
          (progn (beginning-of-line) t)))
     (t
      (if (re-search-forward
           (concat
            "\\(^@node\\).*\n"              ; match node line
            "\\(\\(\\(^@c\\).*\n\\)"        ; match comment line, if any
            "\\|"                           ; or
            "\\(^@ifinfo[ ]*\n\\)\\)?"      ; ifinfo line, if any
            (eval (cdr (assoc level texinfo-update-menu-higher-regexps))))
           region-end t)
          (progn (beginning-of-line) t))))))


;;; Making the list of new menu entries

(defun texinfo-make-menu-list (beginning end level)
  "Make a list of node names and their descriptions.
Point is left at the end of the menu region, but the menu is not inserted.

First argument is position from which to start making menu list; 
second argument is end of region in which to try to locate entries;
third argument is the level of the nodes that are the entries.

Node names and descriptions are dotted pairs of strings.  Each pair is
an element of the list.  If the description does not exist, the
element consists only of the node name."
  (goto-char beginning)
  (let (new-menu-list)
    (while (texinfo-menu-locate-entry-p level end)
      (setq new-menu-list 
            (cons (cons 
                   (texinfo-copy-node-name)
                   (prog1 "" (forward-line 1)))
                   ;; Use following to insert section titles automatically.
                   ;; (texinfo-copy-section-title))
                  new-menu-list)))
    (reverse new-menu-list)))

(defun texinfo-menu-locate-entry-p (level search-end)
  "Find a node that will be part of menu for this section.
First argument is a string such as \"section\" specifying the general
hierarchical level of the menu; second argument is a position
specifying the end of the search.

The function returns t if the node is found, else nil.  It searches
forward from point, and leaves point at the beginning of the node.

The function finds entries of the same type.  Thus `subsections' and
`unnumberedsubsecs' will appear in the same menu."
  (let ((case-fold-search t))
    (if (re-search-forward
         (concat
          "\\(^@node\\).*\n"              ; match node line
          "\\(\\(\\(^@c\\).*\n\\)"        ; match comment line, if any
          "\\|"                           ; or
          "\\(^@ifinfo[ ]*\n\\)\\)?"      ; ifinfo line, if any
          (eval
           (cdr (assoc level texinfo-update-menu-same-level-regexps))))
         search-end
         t)
        (goto-char (match-beginning 1)))))

(defun texinfo-copy-node-name ()
  "Return the node name as a string.

Start with point at the beginning of the node line; copy the text
after the node command up to the first comma on the line, if any, and
return the text as a string.  Leaves point at the beginning of the
line.  If there is no node name, returns an empty string."
  
  (save-excursion
    (buffer-substring
     (progn (forward-word 1)              ; skip over node command
            (skip-chars-forward " \t")    ; and over spaces
            (point))
     (if (search-forward
          ","
          (save-excursion (end-of-line) (point)) t) ; bound search
         (1- (point))
       (end-of-line) (point)))))

(defun texinfo-copy-section-title ()
  "Return the title of the section as a string.
The title is used as a description line in the menu when one does not
already exist.

Move point to the beginning of the appropriate section line by going
to the start of the text matched by last regexp searched for, which
must have been done by `texinfo-menu-locate-entry-p'."

  ;; could use the same re-search as in `texinfo-menu-locate-entry-p'
  ;; instead of using `match-beginning'; such a variation would be
  ;; more general, but would waste information already collected

  (goto-char (match-beginning 7))       ; match section name 

  (buffer-substring
   (progn (forward-word 1)              ; skip over section type
          (skip-chars-forward " \t")    ; and over spaces
          (point))
   (progn (end-of-line) (point))))


;;; Handling the old menu

(defun texinfo-old-menu-p (beginning first)
  "Move point to the beginning of the menu for this section, if any.
Otherwise move point to the end of the first node of this section.
Return t if a menu is found, nil otherwise.

First argument is the position of the beginning of the section in which
the menu will be located; second argument is the position of the first
node within the section.

If no menu is found, the function inserts two newlines just before the
end of the section, and leaves point there where a menu ought to be."
  (goto-char beginning)
  (if (not (re-search-forward "^@menu" first 'goto-end))
      (progn (insert "\n\n") (forward-line -2) nil)
    t))

(defun texinfo-incorporate-descriptions (new-menu-list)
  "Copy the old menu line descriptions that exist to the new menu.

Point must be at beginning of old menu.

If the node-name of the new menu is found in the old menu, insert the
old description into the new entry.

For this function, the new menu is a list made up of lists of dotted
pairs in which the first element of the pair is the node name and the
second element the description.  The new menu is changed destructively.
The old menu is the menu as it appears in the texinfo file."
  
  (let ((new-menu-list-pointer new-menu-list)
        (end-of-menu (texinfo-menu-end)))
    (while new-menu-list
      (save-excursion                   ; keep point at beginning of menu 
        (if (re-search-forward
             ;; Existing nodes can have the form
             ;;     * NODE NAME:: DESCRIPTION
             ;; or
             ;;     * MENU ITEM: NODE NAME.     DESCRIPTION.
             ;; 
             ;; Recognize both when looking for the description.
             (concat "\\* \\("              ; so only menu entries are found
                     (car (car new-menu-list)) "::"
                     "\\|"
                     ".*: " (car (car new-menu-list)) "[.,\t\n]"
                     "\\)"
                     )               ; so only complete entries are found
             end-of-menu
             t) 
            (setcdr (car new-menu-list) 
                    (texinfo-menu-copy-old-description end-of-menu))))
      (setq new-menu-list (cdr new-menu-list))) 
    (setq new-menu-list new-menu-list-pointer)))

(defun texinfo-incorporate-menu-entry-names (new-menu-list)
  "Copy any old menu entry names to the new menu.

Point must be at beginning of old menu.

If the node-name of the new menu entry cannot be found in the old
menu, do nothing.

For this function, the new menu is a list made up of lists of dotted
pairs in which the first element of the pair is the node name and the
second element is the description (or nil).

If we find an existing menu entry name, we change the first element of
the pair to be another dotted pair in which the car is the menu entry
name and the cdr is the node name.

NEW-MENU-LIST is changed destructively.  The old menu is the menu as it
appears in the texinfo file."
  
  (let ((new-menu-list-pointer new-menu-list)
        (end-of-menu (texinfo-menu-end)))
    (while new-menu-list
      (save-excursion                   ; keep point at beginning of menu 
        (if (re-search-forward
             ;; Existing nodes can have the form
             ;;     * NODE NAME:: DESCRIPTION
             ;; or
             ;;     * MENU ITEM: NODE NAME.     DESCRIPTION.
             ;; 
             ;; We're interested in the second case.
             (concat "\\* "              ; so only menu entries are found
                     "\\(.*\\): " (car (car new-menu-list))  "[.,\t\n]")
             end-of-menu
             t)
            (setcar
              (car new-menu-list)  ; replace the node name
              (cons (buffer-substring (match-beginning 1) (match-end 1))
                    (car (car new-menu-list)))))
      (setq new-menu-list (cdr new-menu-list))))
    (setq new-menu-list new-menu-list-pointer)))

(defun texinfo-menu-copy-old-description (end-of-menu)
  "Return description field of old menu line as string.
Point must be located just after the node name.  Point left before description.
Single argument, END-OF-MENU, is position limiting search."
  (skip-chars-forward "[:.,\t\n ]+")
  ;; don't copy a carriage return at line beginning with asterisk!
  ;; do copy a description that begins with an `@'!
  ;; !! Known bug: does not copy descriptions starting with ^|\{?* etc.
  (if (and (looking-at "\\(\\w+\\|@\\)")    
           (not (looking-at "\\(^\\* \\|^@end menu\\)")))  
      (buffer-substring
       (point)
       (save-excursion
         (re-search-forward "\\(^\\* \\|^@end menu\\)" end-of-menu t)
         (forward-line -1)
         (end-of-line)                  ; go to end of last description line
         (point)))
    ""))

(defun texinfo-menu-end ()
  "Return position of end of menu. Does not change location of point.
Signal an error if not end of menu."
  (save-excursion
    (if (re-search-forward "^@end menu" nil t)
        (point)
      (error "Menu does not have an end."))))

(defun texinfo-delete-old-menu (beginning first)
  "Delete the old menu.  Point must be in or after menu.
First argument is position of the beginning of the section in which
the menu will be located; second argument is the position of the first
node within the section."
  ;; No third arg to search, so error if search fails.
  (re-search-backward "^@menu" beginning)
  (delete-region (point)
                 (save-excursion
                   (re-search-forward "^@end menu" first)
                   (point))))


;;; Inserting new menu

;; try 32, but perhaps 24 is better
(defvar texinfo-column-for-description 32
  "*Column at which descriptions start in a Texinfo menu.")

(defun texinfo-insert-menu (menu-list node-name)
  "Insert formatted menu at point.
Indents the first line of the description, if any, to the value of
texinfo-column-for-description.

MENU-LIST has form:

    \(\(\"node-name1\" . \"description\"\) 
    \(\"node-name2\" . \"description\"\) ... \)

However, the description field might be nil.

Also, the node-name field might itself be a dotted pair (call it P) of
strings instead of just a string.  In that case, the car of P
is the menu entry name, and the cdr of P is the node name."
  
  (insert "@menu\n")
  (while menu-list
    ;; Every menu entry starts with a star and a space.
    (insert "* ")
    
    ;; Insert the node name (and menu entry name, if present).
    (let ((node-part (car (car menu-list))))
      (if (stringp node-part)
          ;; "Double colon" entry line; menu entry and node name are the same,
          (insert (format "%s::" node-part))  
        ;; "Single colon" entry line; menu entry and node name are different.
        (insert (format "%s: %s." (car node-part) (cdr node-part)))))
    
    ;; Insert the description, if present.
    (if (cdr (car menu-list))
        (progn
          ;; Move to right place.
          (indent-to texinfo-column-for-description 2) 
          ;; Insert description.
          (insert (format "%s" (cdr (car menu-list))))))  

    (insert "\n") ; end this menu entry
    (setq menu-list (cdr menu-list)))
  (insert "@end menu")
  (message 
   "Updated \"%s\" level menu following node: %s ... " level node-name))


;;; Starting menu descriptions by inserting titles

(defun texinfo-start-menu-description ()
  "In this menu entry, insert the node's section title as a description. 
Position point at beginning of description ready for editing.
Do not insert a title if the line contains an existing description.

You will need to edit the inserted text since a useful description
complements the node name rather than repeats it as a title does."
  
  (interactive)
  (let (beginning end node-name title)
    (save-excursion
    (beginning-of-line)  
      (if (search-forward "* " (save-excursion (end-of-line) (point)) t)
          (progn (skip-chars-forward " \t")
                 (setq beginning (point)))
        (error "This is not a line in a menu!"))
      
      (cond
        ;; "Double colon" entry line; menu entry and node name are the same,
       ((search-forward "::" (save-excursion (end-of-line) (point)) t)
        (if (looking-at "[ \t]*[^ \t\n]+")
            (error "Descriptive text already exists."))
        (skip-chars-backward ": \t")
        (setq node-name (buffer-substring beginning (point))))
       
       ;; "Single colon" entry line; menu entry and node name are different.
       ((search-forward ":" (save-excursion (end-of-line) (point)) t)
        (skip-chars-forward " \t")
        (setq beginning (point))
        ;; Menu entry line ends in a period, comma, or tab. 
        (if (re-search-forward "[.,\t]"
                               (save-excursion (forward-line 1) (point)) t)
            (progn
              (if (looking-at "[ \t]*[^ \t\n]+")
                  (error "Descriptive text already exists."))
              (skip-chars-backward "., \t")
              (setq node-name (buffer-substring beginning (point))))
          ;; Menu entry line ends in a return.
          (re-search-forward ".*\n"
                           (save-excursion (forward-line 1) (point)) t)
          (skip-chars-backward " \t\n")
          (setq node-name (buffer-substring beginning (point)))
          (if (= 0 (length node-name))
              (error "No node name on this line.")
            (insert "."))))
       (t (error "No node name on this line.")))
      ;; Search for node that matches node name, and copy the section title.
      (if (re-search-forward
           (concat 
            "^@node[ \t]+"
            node-name
            ".*\n"                             ; match node line
            "\\("
            "\\(\\(^@c \\|^@comment\\).*\n\\)" ; match comment line, if any
            "\\|"                              ; or
            "\\(^@ifinfo[ ]*\n\\)"             ; ifinfo line, if any
            "\\)?")
           nil t)
          (progn
            (setq title
                  (buffer-substring
                   ;; skip over section type
                   (progn (forward-word 1) 
                          ;; and over spaces
                          (skip-chars-forward " \t") 
                          (point))
                   (progn (end-of-line)
                          (skip-chars-backward " \t")
                          (point)))))
        (error "Cannot find node to match node name in menu entry.")))
    ;; Return point to the menu and insert the title.
    (end-of-line)
    (delete-region
     (point)
     (save-excursion (skip-chars-backward " \t") (point)))
    (indent-to texinfo-column-for-description 2)
    (save-excursion (insert title))))


;;; Handling description indentation

;; Since the make-menu functions indent descriptions, these functions
;; are useful primarily for indenting a single menu specially.

(defun texinfo-indent-menu-description (column &optional region-p)
  "Indent every description in menu following point to COLUMN.  
Non-nil argument (prefix, if interactive) means indent every
description in every menu in the region.  Does not indent second and
subsequent lines of a multi-line description."
  
  (interactive
   "nIndent menu descriptions to (column number): \nP")
  (save-excursion
    (save-restriction
      (widen)
      (if (not region-p)
          (progn
            (re-search-forward "^@menu")
            (texinfo-menu-indent-description column)
            (message
             "Indented descriptions in menu.  You may save the buffer."))
        ;;else
        (message "Indenting every menu description in region... ")
        (goto-char (region-beginning))
        (while (and (< (point) (region-end))
                    (texinfo-locate-menu-p))
          (forward-line 1)
          (texinfo-menu-indent-description column))
        (message "Indenting done.  You may save the buffer.")))))

(defun texinfo-menu-indent-description (to-column-number)
  "Indent the Texinfo file menu description to TO-COLUMN-NUMBER.
Start with point just after the word `menu' in the `@menu' line and
leave point on the line before the `@end menu' line.  Does not indent
second and subsequent lines of a multi-line description."
  (let* ((beginning-of-next-line (point)))
    (while (< beginning-of-next-line
              (save-excursion     ; beginning of end menu line
                (goto-char (texinfo-menu-end))
                (beginning-of-line)
                (point)))

      (if (re-search-forward  "\\* \\(.*::\\|.*: [^.,\t\n]+[.,\t]\\)" 
           (texinfo-menu-end) 
           t)
          (progn
            (let ((beginning-white-space (point)))
              (skip-chars-forward " \t")  ; skip over spaces
              (if (looking-at "\\(@\\|\\w\\)+") ; if there is text
                  (progn
                    ;; remove pre-existing indentation
                    (delete-region beginning-white-space (point))
                    (indent-to-column to-column-number))))))
      ;; position point at beginning of next line
      (forward-line 1)                  
      (setq beginning-of-next-line (point)))))


;;; Making the master menu

(defun texinfo-master-menu (update-all-nodes-menus-p)
  "Make a master menu for a whole Texinfo file.
Non-nil argument (prefix, if interactive) means first update all
existing nodes and menus.  Remove pre-existing master menu, if there is one.

This function creates a master menu that follows the top node.  The
master menu includes every entry from all the other menus.  It
replaces any existing ordinary menu that follows the top node.

If called with a non-nil argument, this function first updates all the
menus in the buffer (incorporating descriptions from pre-existing
menus) before it constructs the master menu.

The function removes the detailed part of an already existing master
menu.  This action depends on the pre-existing master menu using the
standard `texinfo-master-menu-header'.

The master menu has the following format, which is adapted from the
recommendation in the Texinfo Manual:

   * The first part contains the major nodes in the Texinfo file: the
     nodes for the chapters, chapter-like sections, and the major
     appendices.  This includes the indices, so long as they are in
     chapter-like sections, such as unnumbered sections.

   * The second and subsequent parts contain a listing of the other,
     lower level menus, in order.  This way, an inquirer can go
     directly to a particular node if he or she is searching for
     specific information.

Each of the menus in the detailed node listing is introduced by the
title of the section containing the menu."
  
  (interactive "P")
  (let ((case-fold-search t))
    (widen)
    (goto-char (point-min))
    
    ;; Move point to location after `top'.
    (if (not (re-search-forward "^@node [ \t]*top[ \t]*\\(,\\|$\\)" nil t))
        (error "This buffer needs a Top node!"))
    
    (let ((first-chapter                  
           (save-excursion
             (or (re-search-forward "^@node" nil t)
                 (error "Too few nodes for a master menu!"))
             (point))))
      (if (re-search-forward texinfo-master-menu-header first-chapter t)
          ;; Remove detailed master menu listing
          (progn
            (goto-char (match-beginning 0))
            (let ((end-of-detailed-menu-descriptions
                   (save-excursion     ; beginning of end menu line
                     (goto-char (texinfo-menu-end))
                     (beginning-of-line) (forward-char -1)
                     (point))))
              (delete-region (point) end-of-detailed-menu-descriptions)))))
    
    (if update-all-nodes-menus-p
        (progn
          (message "Making a master menu in %s ...first updating all nodes... "
                   (buffer-name))
          (sleep-for 2)
          (push-mark (point-max) t)
          (goto-char (point-min))
          (texinfo-update-node t)
          
          (message "Updating all menus in %s ... " (buffer-name))        
          (sleep-for 2)
          (push-mark (point-max) t)
          (goto-char (point-min))
          (texinfo-make-menu t)))
    
    (message "Now making the master menu in %s... " (buffer-name))
    (sleep-for 2)
    (goto-char (point-min))
    (texinfo-insert-master-menu-list
     (texinfo-master-menu-list))
    
    ;; Remove extra newlines that texinfo-insert-master-menu-list
    ;; may have inserted.
    
    (save-excursion
      (goto-char (point-min))
      
      (if (re-search-forward texinfo-master-menu-header nil t)
          (progn
            (goto-char (match-beginning 0))
            (insert "\n")
            (delete-blank-lines)
            (goto-char (point-min))))

      (re-search-forward "^@menu")
      (forward-line -1)
      (delete-blank-lines)
      
      (re-search-forward "^@end menu")
      (forward-line 1)
      (delete-blank-lines))
    
    (message
     "Done...completed making master menu.  You may save the buffer.")))

(defun texinfo-master-menu-list ()
  "Return a list of menu entries and header lines for the master menu.

Start with the menu for chapters and indices and then find each
following menu and the title of the node preceding that menu.

The master menu list has this form:

    \(\(\(... \"entry-1-2\"  \"entry-1\"\) \"title-1\"\)
      \(\(... \"entry-2-2\"  \"entry-2-1\"\) \"title-2\"\)
      ...\)

However, there does not need to be a title field."

  (let (master-menu-list)
    (while (texinfo-locate-menu-p)
      (setq master-menu-list 
            (cons (list
                   (texinfo-copy-menu)
                   (texinfo-copy-menu-title))
                  master-menu-list)))
    (reverse master-menu-list)))

(defun texinfo-insert-master-menu-list (master-menu-list)
  "Format and insert the master menu in the current buffer."
  (goto-char (point-min))
  ;; Insert a master menu only after `Top' node and before next node
  ;; \(or include file if there is no next node\).
  (if (not (re-search-forward "^@node [ \t]*top[ \t]*\\(,\\|$\\)" nil t))
      (error "This buffer needs a Top node!"))
  (let ((first-chapter
         (save-excursion (re-search-forward "^@node\\|^@include") (point))))
    (if (not (re-search-forward "^@menu" first-chapter t))
        (error
         "Buffer lacks ordinary `Top' menu in which to insert master.")))
  (beginning-of-line)
  (delete-region      ; buffer must have ordinary top menu
   (point)   
   (save-excursion (re-search-forward "^@end menu") (point)))
  
  (save-excursion                       ; leave point at beginning of menu
    ;; Handle top of menu
    (insert "\n@menu\n")
    ;; Insert chapter menu entries
    (setq this-very-menu-list (reverse (car (car master-menu-list))))
    ;; Tell user what is going on.
    (message "Inserting chapter menu entry: %s ... " this-very-menu-list)
    (while this-very-menu-list
      (insert "* " (car this-very-menu-list) "\n")
      (setq this-very-menu-list (cdr this-very-menu-list)))
    
    (setq master-menu-list (cdr master-menu-list))
    
    ;; Only insert detailed master menu if there is one....
    (if (car (car master-menu-list))
;; @detailmenu added 5 Sept 1996 at Karl Berry's request to avert a
;; bug in `makeinfo'; all agree this is a bad kluge and should
;; eventually be removed.  @detailmenu ... @end detailmenu is a noop
;; in `texinfmt.el'  See @end detailmenu below
;; also see `texinfo-all-menus-update' above, `texinfo-master-menu',
;; `texinfo-multiple-files-update'
          (insert texinfo-master-menu-header))
    
    ;; Now, insert all the other menus
    
    ;; The menu master-menu-list has a form like this:
    ;; ((("beta"  "alpha") "title-A")
    ;;  (("delta" "gamma") "title-B"))
    
    (while master-menu-list
      
      (message
       "Inserting menu for %s .... " (car (cdr (car master-menu-list))))
      ;; insert title of menu section
      (insert "\n" (car (cdr (car master-menu-list))) "\n\n")
      
      ;; insert each menu entry
      (setq this-very-menu-list (reverse (car (car master-menu-list))))
      (while this-very-menu-list
        (insert "* " (car this-very-menu-list) "\n")
        (setq this-very-menu-list (cdr this-very-menu-list)))
      
      (setq master-menu-list (cdr master-menu-list)))
    
    ;; Finish menu
;; @detailmenu (see note above)
    (insert "\n@end detailmenu")
    (insert "\n@end menu\n\n")))

(defvar texinfo-master-menu-header
  "\n@detailmenu\n --- The Detailed Node Listing ---\n"
  "String inserted before lower level entries in Texinfo master menu.
It comes after the chapter-level menu entries.")

(defun texinfo-locate-menu-p ()
  "Find the next menu in the texinfo file.
If found, leave point after word `menu' on the `@menu' line, and return t.
If a menu is not found, do not move point and return nil."
  (re-search-forward "\\(^@menu\\)" nil t))

(defun texinfo-copy-menu-title  ()
  "Return the title of the section preceding the menu as a string.
If such a title cannot be found, return an empty string.  Do not move
point."
  (let ((case-fold-search t))
    (save-excursion
      (if (re-search-backward
           (concat
            "\\(^@top"
            "\\|"                         ; or
            texinfo-section-types-regexp  ; all other section types
            "\\)")
           nil
           t)
          (progn
            (beginning-of-line)
            (forward-word 1)              ; skip over section type
            (skip-chars-forward " \t")    ; and over spaces
            (buffer-substring
             (point)
             (progn (end-of-line) (point))))
        ""))))

(defun texinfo-copy-menu ()
  "Return the entries of an existing menu as a list.
Start with point just after the word `menu' in the `@menu' line
and leave point on the line before the `@end menu' line."
  (let* (this-menu-list
         (end-of-menu (texinfo-menu-end)) ; position of end of `@end menu'
         (last-entry (save-excursion      ; position of beginning of
                                          ; last `* ' entry
                      (goto-char end-of-menu)
                      ;; handle multi-line description
                      (if (not (re-search-backward "^\\* " nil t))
                          (error "No entries in menu."))
                      (point))))
    (while (< (point) last-entry)
      (if (re-search-forward  "^\\* " end-of-menu t)
          (progn
            (setq this-menu-list
                  (cons
                   (buffer-substring 
                    (point)
                    ;; copy multi-line descriptions
                    (save-excursion
                      (re-search-forward "\\(^\\* \\|^@e\\)" nil t)
                      (- (point) 3)))
                   this-menu-list)))))
    this-menu-list))


;;; Determining the hierarchical level in the texinfo file

(defun texinfo-specific-section-type () 
  "Return the specific type of next section, as a string.
For example, \"unnumberedsubsec\".  Return \"top\" for top node.

Searches forward for a section.  Hence, point must be before the
section whose type will be found.  Does not move point.  Signal an
error if the node is not the top node and a section is not found."
  (let ((case-fold-search t))
    (save-excursion
      (cond
       ((re-search-forward "^@node [ \t]*top[ \t]*\\(,\\|$\\)"
;;; Following search limit by cph but causes a bug
;;;                      (save-excursion
;;;                        (end-of-line)
;;;                        (point))
                           nil
                           t)
        "top")
       ((re-search-forward texinfo-section-types-regexp nil t)
        (buffer-substring-no-properties
         (progn (beginning-of-line)     ; copy its name
                (1+ (point)))
         (progn (forward-word 1)
                (point))))
       (t
        (error
         "texinfo-specific-section-type: Chapter or section not found."))))))

(defun texinfo-hierarchic-level ()
  "Return the general hierarchal level of the next node in a texinfo file.
Thus, a subheading or appendixsubsec is of type subsection."
  (let ((case-fold-search t))
    (cdr (assoc
          (texinfo-specific-section-type)
          texinfo-section-to-generic-alist))))


;;; Locating the major positions

(defun texinfo-update-menu-region-beginning (level)  
  "Locate beginning of higher level section this section is within.
Return position of the beginning of the node line; do not move point.
Thus, if this level is subsection, searches backwards for section node.
Only argument is a string of the general type of section."
  (let ((case-fold-search t))
    ;; !! Known bug: if section immediately follows top node, this
    ;; returns the beginning of the buffer as the beginning of the
    ;; higher level section.
    (cond
     ((or (string-equal "top" level)
          (string-equal "chapter" level))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^@node [ \t]*top[ \t]*\\(,\\|$\\)" nil t)
        (beginning-of-line)
        (point)))
     (t
      (save-excursion
        (re-search-backward
         (concat
          "\\(^@node\\).*\n"              ; match node line
          "\\(\\(\\(^@c\\).*\n\\)"        ; match comment line, if any
          "\\|"                           ; or
          "\\(^@ifinfo[ ]*\n\\)\\)?"      ; ifinfo line, if any
          (eval
           (cdr (assoc level texinfo-update-menu-higher-regexps))))
         nil
         'goto-beginning)
        (point))))))

(defun texinfo-update-menu-region-end (level)  
  "Locate end of higher level section this section is within.
Return position; do not move point.  Thus, if this level is a
subsection, find the node for the section this subsection is within.
If level is top or chapter, returns end of file.  Only argument is a
string of the general type of section."
  (let ((case-fold-search t))
    (save-excursion
      (if (re-search-forward
           (concat
            "\\(^@node\\).*\n"            ; match node line
            "\\(\\(\\(^@c\\).*\n\\)"      ; match comment line, if any
            "\\|"                         ; or
            "\\(^@ifinfo[ ]*\n\\)\\)?"    ; ifinfo line, if any
            (eval
             ;; Never finds end of level above chapter so goes to end.
             (cdr (assoc level texinfo-update-menu-higher-regexps))))
           nil
           'goto-end)
          (match-beginning 1)
        (point-max)))))

(defun texinfo-menu-first-node (beginning end)
  "Locate first node of the section the menu will be placed in.  
Return position; do not move point.
The menu will be located just before this position.  

First argument is the position of the beginning of the section in
which the menu will be located; second argument is the position of the
end of that region; it limits the search."
  
  (save-excursion
    (goto-char beginning)
    (forward-line 1)
    (re-search-forward "^@node" end t)
    (beginning-of-line)
    (point)))


;;; Alists and regular expressions for defining hierarchical levels

(defvar texinfo-section-to-generic-alist
  '(("top" . "top")

    ("chapter" . "chapter")
    ("unnumbered" . "chapter")
    ("majorheading" . "chapter")
    ("chapheading" . "chapter")
    ("appendix" . "chapter")
    
    ("section" . "section")
    ("unnumberedsec" . "section")
    ("heading" . "section")
    ("appendixsec" . "section")
    
    ("subsection" . "subsection")
    ("unnumberedsubsec" . "subsection")
    ("subheading" . "subsection")
    ("appendixsubsec" . "subsection")
    
    ("subsubsection" . "subsubsection")
    ("unnumberedsubsubsec" . "subsubsection")
    ("subsubheading" . "subsubsection")
    ("appendixsubsubsec" . "subsubsection"))
  "*An alist of specific and corresponding generic Texinfo section types.
The keys are strings specifying specific types of section; the values
are strings of their corresponding general types.")

;; We used to look for just sub, but that found @subtitle.
(defvar texinfo-section-types-regexp
  "^@\\(chapter \\|sect\\|subs\\|subh\\|unnum\\|major\\|chapheading \\|heading \\|appendix\\)"
  "Regexp matching chapter, section, other headings (but not the top node).")

(defvar texinfo-chapter-level-regexp 
  "chapter\\|unnumbered \\|appendix \\|majorheading\\|chapheading"
  "Regular expression matching just the Texinfo chapter level headings.")

(defvar texinfo-section-level-regexp 
  "section\\|unnumberedsec\\|heading \\|appendixsec"
  "Regular expression matching just the Texinfo section level headings.")

(defvar texinfo-subsection-level-regexp 
  "subsection\\|unnumberedsubsec\\|subheading\\|appendixsubsec"
  "Regular expression matching just the Texinfo subsection level headings.")

(defvar texinfo-subsubsection-level-regexp
  "subsubsection\\|unnumberedsubsubsec\\|subsubheading\\|appendixsubsubsec"
  "Regular expression matching just the Texinfo subsubsection level headings.")

(defvar texinfo-update-menu-same-level-regexps
  '(("top" . "top[ \t]+")
    ("chapter" . 
     (concat "\\(^@\\)\\(" texinfo-chapter-level-regexp "\\)[ \t]*"))
    ("section" . 
     (concat "\\(^@\\)\\(" texinfo-section-level-regexp "\\)[ \t]*"))
    ("subsection" .  
     (concat "\\(^@\\)\\(" texinfo-subsection-level-regexp "\\)[ \t]+"))
    ("subsubsection" . 
     (concat "\\(^@\\)\\(" texinfo-subsubsection-level-regexp "\\)[ \t]+")))
  "*Regexps for searching for same level sections in a Texinfo file.
The keys are strings specifying the general hierarchical level in the
document; the values are regular expressions.")

(defvar texinfo-update-menu-higher-regexps
  '(("top" . "^@node [ \t]*DIR") 
    ("chapter" . "^@node [ \t]*top[ \t]*\\(,\\|$\\)")
    ("section" .
     (concat 
      "\\(^@\\("
      texinfo-chapter-level-regexp
      "\\)[ \t]*\\)"))
    ("subsection" .
     (concat 
      "\\(^@\\("
      texinfo-section-level-regexp
      "\\|"
      texinfo-chapter-level-regexp
      "\\)[ \t]*\\)"))
    ("subsubsection" .
     (concat 
      "\\(^@\\("
      texinfo-subsection-level-regexp
      "\\|"
      texinfo-section-level-regexp
      "\\|"
      texinfo-chapter-level-regexp
      "\\)[ \t]*\\)")))
  "*Regexps for searching for higher level sections in a Texinfo file.
The keys are strings specifying the general hierarchical level in the
document; the values are regular expressions.")

(defvar texinfo-update-menu-lower-regexps
  '(("top" . 
     (concat 
      "\\(^@\\("
      texinfo-chapter-level-regexp
      "\\|"
      texinfo-section-level-regexp
      "\\|"
      texinfo-subsection-level-regexp
      "\\|"
      texinfo-subsubsection-level-regexp
      "\\)[ \t]*\\)"))
    ("chapter" . 
     (concat 
      "\\(^@\\("
      texinfo-section-level-regexp
      "\\|"
      texinfo-subsection-level-regexp
      "\\|"
      texinfo-subsubsection-level-regexp
      "\\)[ \t]*\\)"))
    ("section" .
     (concat 
      "\\(^@\\("
      texinfo-subsection-level-regexp
      "\\|"
      texinfo-subsubsection-level-regexp
      "\\)[ \t]+\\)"))
    ("subsection" .
     (concat 
      "\\(^@\\("
      texinfo-subsubsection-level-regexp
      "\\)[ \t]+\\)"))
    ("subsubsection" . "nothing lower"))
  "*Regexps for searching for lower level sections in a Texinfo file.
The keys are strings specifying the general hierarchical level in the
document; the values are regular expressions.")


;;; Updating a node

;;;###autoload
(defun texinfo-update-node (&optional region-p)
  "Without any prefix argument, update the node in which point is located.
Non-nil argument (prefix, if interactive) means update the nodes in the
marked region.

The functions for creating or updating nodes and menus, and their
keybindings, are:

    texinfo-update-node (&optional region-p)    \\[texinfo-update-node]
    texinfo-every-node-update ()                \\[texinfo-every-node-update]
    texinfo-sequential-node-update (&optional region-p)

    texinfo-make-menu (&optional region-p)      \\[texinfo-make-menu]
    texinfo-all-menus-update ()                 \\[texinfo-all-menus-update]
    texinfo-master-menu ()

    texinfo-indent-menu-description (column &optional region-p)

The `texinfo-column-for-description' variable specifies the column to
which menu descriptions are indented. Its default value is 32."
  
  (interactive "P")
  (if (not region-p)
      ;; update a single node
      (let ((auto-fill-function nil) (auto-fill-hook nil))
        (if (not (re-search-backward "^@node" (point-min) t))
            (error "Node line not found before this position."))
        (texinfo-update-the-node)
        (message "Done...updated the node.  You may save the buffer."))
    ;; else
    (let ((auto-fill-function nil)
          (auto-fill-hook nil)
          (beginning (region-beginning))
          (end (region-end)))
      (if (= end beginning)
          (error "Please mark a region!"))
      (save-restriction
        (narrow-to-region beginning end)
        (goto-char beginning)
        (push-mark (point) t)
        (while (re-search-forward "^@node" (point-max) t)
          (beginning-of-line)            
          (texinfo-update-the-node))
        (message "Done...updated nodes in region.  You may save the buffer.")))))

;;;###autoload
(defun texinfo-every-node-update ()
  "Update every node in a Texinfo file."
  (interactive)
  (save-excursion
    (push-mark (point-max) t)
    (goto-char (point-min))
    ;; Using the mark to pass bounds this way
    ;; is kludgy, but it's not worth fixing. -- rms.
    (let ((mark-active t))
      (texinfo-update-node t))
    (message "Done...updated every node.       You may save the buffer.")))

(defun texinfo-update-the-node ()
  "Update one node.  Point must be at the beginning of node line.  
Leave point at the end of the node line."
  (texinfo-check-for-node-name)
  (texinfo-delete-existing-pointers)
  (message "Updating node: %s ... " (texinfo-copy-node-name))
  (save-restriction
    (widen)
    (let*
        ((case-fold-search t)
         (level (texinfo-hierarchic-level))
         (beginning (texinfo-update-menu-region-beginning level))
         (end (texinfo-update-menu-region-end level)))
      (if (string-equal level "top")
          (texinfo-top-pointer-case)
        ;; else
        (texinfo-insert-pointer beginning end level 'next)
        (texinfo-insert-pointer beginning end level 'previous)
        (texinfo-insert-pointer beginning end level 'up)
        (texinfo-clean-up-node-line)))))

(defun texinfo-top-pointer-case ()
  "Insert pointers in the Top node.  This is a special case.

The `Next' pointer is a pointer to a chapter or section at a lower
hierarchical level in the file.  The `Previous' and `Up' pointers are
to `(dir)'.  Point must be at the beginning of the node line, and is
left at the end of the node line."

  (texinfo-clean-up-node-line)
  (insert ", " 
          (save-excursion
            ;; There may be an @chapter or other such command between
            ;; the top node line and the next node line, as a title
            ;; for an `ifinfo' section. This @chapter command must
            ;; must be skipped.  So the procedure is to search for
            ;; the next `@node' line, and then copy its name.
            (if (re-search-forward "^@node" nil t)
                (progn
                  (beginning-of-line)
                  (texinfo-copy-node-name))
              " "))
          ", (dir), (dir)"))

(defun texinfo-check-for-node-name ()
  "Determine whether the node has a node name.  Prompt for one if not.
Point must be at beginning of node line.  Does not move point."
  (save-excursion
    (let ((initial (texinfo-copy-next-section-title)))
      ;; This is not clean.  Use `interactive' to read the arg.
      (forward-word 1)                    ; skip over node command
      (skip-chars-forward " \t")          ; and over spaces
      (if (not (looking-at "[^,\t\n ]+")) ; regexp based on what Info looks for
                                          ; alternatively, use "[a-zA-Z]+"
        (let ((node-name
               (read-from-minibuffer
                "Node name (use no @, commas, colons, or apostrophes): "
                initial)))
          (insert " " node-name))))))

(defun texinfo-delete-existing-pointers ()
  "Delete `Next', `Previous', and `Up' pointers.  
Starts from the current position of the cursor, and searches forward
on the line for a comma and if one is found, deletes the rest of the
line, including the comma.  Leaves point at beginning of line."
  (let ((eol-point (save-excursion (end-of-line) (point))))
    (if (search-forward "," eol-point t)
        (delete-region (1- (point)) eol-point)))
  (beginning-of-line))

(defun texinfo-find-pointer (beginning end level direction)
  "Move point to section associated with next, previous, or up pointer.
Return type of pointer (either 'normal or 'no-pointer).

The first and second arguments bound the search for a pointer to the
beginning and end, respectively, of the enclosing higher level
section.  The third argument is a string specifying the general kind
of section such as \"chapter\" or \"section\".  When looking for the
`Next' pointer, the section found will be at the same hierarchical
level in the Texinfo file; when looking for the `Previous' pointer,
the section found will be at the same or higher hierarchical level in
the Texinfo file; when looking for the `Up' pointer, the section found
will be at some level higher in the Texinfo file.  The fourth argument
\(one of 'next, 'previous, or 'up\) specifies whether to find the
`Next', `Previous', or `Up' pointer."
  (let ((case-fold-search t))
    (cond ((eq direction 'next)
           (forward-line 3)             ; skip over current node
           ;; Search for section commands accompanied by node lines;
           ;; ignore section commands in the middle of nodes.
           (if (re-search-forward
                ;; A `Top' node is never a next pointer, so won't find it.
                (concat
                 ;; Match node line.
                 "\\(^@node\\).*\n"         
                 ;; Match comment or ifinfo line, if any
                 "\\(\\(\\(^@c\\).*\n\\)\\|\\(^@ifinfo[ ]*\n\\)\\)?" 
                 (eval
                  (cdr (assoc level texinfo-update-menu-same-level-regexps))))
                end
                t)
               'normal
             'no-pointer))
          ((eq direction 'previous)
           (if (re-search-backward
                (concat
                 "\\("
                 ;; Match node line.
                 "\\(^@node\\).*\n"         
                 ;; Match comment or ifinfo line, if any
                 "\\(\\(\\(^@c\\).*\n\\)\\|\\(^@ifinfo[ ]*\n\\)\\)?" 
                 (eval
                  (cdr (assoc level texinfo-update-menu-same-level-regexps)))
                 "\\|"
                 ;; Match node line.
                 "\\(^@node\\).*\n"         
                 ;; Match comment or ifinfo line, if any
                 "\\(\\(\\(^@c\\).*\n\\)\\|\\(^@ifinfo[ ]*\n\\)\\)?" 
                 (eval
                  (cdr (assoc level texinfo-update-menu-higher-regexps)))
                 "\\|"
                 ;; Handle `Top' node specially.
                 "^@node [ \t]*top[ \t]*\\(,\\|$\\)"
                 "\\)")
                beginning
                t)
               'normal
             'no-pointer))
          ((eq direction 'up)
           (if (re-search-backward
                (concat
                 "\\("
                 ;; Match node line.
                 "\\(^@node\\).*\n"         
                 ;; Match comment or ifinfo line, if any
                 "\\(\\(\\(^@c\\).*\n\\)\\|\\(^@ifinfo[ ]*\n\\)\\)?" 
                 (eval (cdr (assoc level texinfo-update-menu-higher-regexps)))
                 "\\|"
                 ;; Handle `Top' node specially.
                 "^@node [ \t]*top[ \t]*\\(,\\|$\\)"
                 "\\)")
                (save-excursion
                  (goto-char beginning)
                  (beginning-of-line)
                  (point))
                t)
               'normal
             'no-pointer))
          (t
           (error "texinfo-find-pointer: lack proper arguments")))))

(defun texinfo-pointer-name (kind)
  "Return the node name preceding the section command.
The argument is the kind of section, either normal or no-pointer."
  (let (name)
    (cond ((eq kind 'normal)
           (end-of-line)                ; this handles prev node top case
           (re-search-backward          ; when point is already 
            "^@node"                    ; at the beginning of @node line
            (save-excursion (forward-line -3))
            t)
           (setq name (texinfo-copy-node-name)))
          ((eq kind 'no-pointer)
           (setq name " ")))    ; put a blank in the pointer slot
    name))

(defun texinfo-insert-pointer (beginning end level direction)
  "Insert the `Next', `Previous' or `Up' node name at point.
Move point forward.  

The first and second arguments bound the search for a pointer to the
beginning and end, respectively, of the enclosing higher level
section.  The third argument is the hierarchical level of the Texinfo
file, a string such as \"section\".  The fourth argument is direction
towards which the pointer is directed, one of `next, `previous, or
'up."

  (end-of-line)
  (insert
   ", "
   (save-excursion
     (texinfo-pointer-name
      (texinfo-find-pointer beginning end level direction)))))

(defun texinfo-clean-up-node-line ()
  "Remove extra commas, if any, at end of node line."
  (end-of-line)
  (skip-chars-backward ", ")
  (delete-region (point) (save-excursion (end-of-line) (point))))


;;; Updating nodes sequentially
;; These sequential update functions insert `Next' or `Previous'
;; pointers that point to the following or preceding nodes even if they
;; are at higher or lower hierarchical levels.  This means that if a
;; section contains one or more subsections, the section's `Next'
;; pointer will point to the subsection and not the following section.
;; (The subsection to which `Next' points will most likely be the first
;; item on the section's menu.)

;;;###autoload
(defun texinfo-sequential-node-update (&optional region-p)
  "Update one node (or many) in a Texinfo file with sequential pointers.

This function causes the `Next' or `Previous' pointer to point to the
immediately preceding or following node, even if it is at a higher or
lower hierarchical level in the document.  Continually pressing `n' or
`p' takes you straight through the file.

Without any prefix argument, update the node in which point is located.
Non-nil argument (prefix, if interactive) means update the nodes in the
marked region.

This command makes it awkward to navigate among sections and
subsections; it should be used only for those documents that are meant
to be read like a novel rather than a reference, and for which the
Info `g*' command is inadequate."
  
  (interactive "P")
  (if (not region-p)
      ;; update a single node
      (let ((auto-fill-function nil) (auto-fill-hook nil))
        (if (not (re-search-backward "^@node" (point-min) t))
            (error "Node line not found before this position."))
        (texinfo-sequentially-update-the-node)
        (message 
         "Done...sequentially updated the node .  You may save the buffer."))
    ;; else
    (let ((auto-fill-function nil)
          (auto-fill-hook nil)
          (beginning (region-beginning))
          (end (region-end)))
      (if (= end beginning)
          (error "Please mark a region!"))
      (save-restriction
        (narrow-to-region beginning end)
        (goto-char beginning)
        (push-mark (point) t)
        (while (re-search-forward "^@node" (point-max) t)
          (beginning-of-line)            
          (texinfo-sequentially-update-the-node))
        (message 
         "Done...updated the nodes in sequence.  You may save the buffer.")))))

(defun texinfo-sequentially-update-the-node ()
  "Update one node such that the pointers are sequential. 
A `Next' or `Previous' pointer points to any preceding or following node,
regardless of its hierarchical level."

        (texinfo-check-for-node-name)
        (texinfo-delete-existing-pointers)
        (message 
         "Sequentially updating node: %s ... " (texinfo-copy-node-name))
        (save-restriction
          (widen)
          (let*
              ((case-fold-search t)
               (level (texinfo-hierarchic-level)))
            (if (string-equal level "top")
                (texinfo-top-pointer-case)
              ;; else
              (texinfo-sequentially-insert-pointer level 'next)
              (texinfo-sequentially-insert-pointer level 'previous)
              (texinfo-sequentially-insert-pointer level 'up)
              (texinfo-clean-up-node-line)))))

(defun texinfo-sequentially-find-pointer (level direction)
  "Find next or previous pointer sequentially in Texinfo file, or up pointer.
Move point to section associated with the pointer.  Find point even if
it is in a different section.

Return type of pointer (either 'normal or 'no-pointer).

The first argument is a string specifying the general kind of section
such as \"chapter\" or \"section\".  The section found will be at the
same hierarchical level in the Texinfo file, or, in the case of the up
pointer, some level higher.  The second argument (one of 'next,
'previous, or 'up) specifies whether to find the `Next', `Previous',
or `Up' pointer."
  (let ((case-fold-search t))  
    (cond ((eq direction 'next)
           (forward-line 3)             ; skip over current node
           (if (re-search-forward 
                texinfo-section-types-regexp
                (point-max)
                t)
               'normal
             'no-pointer))
          ((eq direction 'previous)
           (if (re-search-backward 
                texinfo-section-types-regexp
                (point-min)
                t)
               'normal
             'no-pointer))
          ((eq direction 'up)
           (if (re-search-backward
                (eval (cdr (assoc level texinfo-update-menu-higher-regexps)))
                beginning
                t)
               'normal
             'no-pointer))
          (t
           (error "texinfo-sequential-find-pointer: lack proper arguments")))))

(defun texinfo-sequentially-insert-pointer (level direction)
  "Insert the `Next', `Previous' or `Up' node name at point.
Move point forward.  

The first argument is the hierarchical level of the Texinfo file, a
string such as \"section\".  The second argument is direction, one of
`next, `previous, or 'up."

  (end-of-line)
  (insert
   ", "
   (save-excursion
     (texinfo-pointer-name
      (texinfo-sequentially-find-pointer level direction)))))


;;; Inserting `@node' lines
;; The `texinfo-insert-node-lines' function inserts `@node' lines as needed
;; before the `@chapter', `@section', and such like lines of a region
;; in a Texinfo file.

(defun texinfo-insert-node-lines (beginning end &optional title-p)
  "Insert missing `@node' lines in region of Texinfo file.
Non-nil argument (prefix, if interactive) means also to insert the
section titles as node names; and also to insert the section titles as
node names in pre-existing @node lines that lack names."
  (interactive "r\nP")

  ;; Use marker; after inserting node lines, leave point at end of
  ;; region and mark at beginning.

  (let (beginning-marker end-marker title last-section-position)

    ;; Save current position on mark ring and set mark to end.
    (push-mark end t)                   
    (setq end-marker (mark-marker))        

    (goto-char beginning)
    (while (re-search-forward
            texinfo-section-types-regexp 
            end-marker
            'end)
      ;; Copy title if desired.
      (if title-p
          (progn 
            (beginning-of-line)
            (forward-word 1)
            (skip-chars-forward " \t")
            (setq title (buffer-substring
                         (point)
                         (save-excursion (end-of-line) (point))))))
      ;; Insert node line if necessary.
      (if (re-search-backward
           "^@node" 
           ;; Avoid finding previous node line if node lines are close.
           (or last-section-position    
               (save-excursion (forward-line -2) (point))) t)
          ;;  @node is present, and point at beginning of that line
          (forward-word 1)          ; Leave point just after @node.
        ;; Else @node missing; insert one.
        (beginning-of-line)         ; Beginning of `@section' line.
        (insert "@node\n")
        (backward-char 1))          ; Leave point just after `@node'.
      ;; Insert title if desired.
      (if title-p
          (progn
            (skip-chars-forward " \t")
            ;; Use regexp based on what info looks for
            ;; (alternatively, use "[a-zA-Z]+");
            ;; this means we only insert a title if none exists.
            (if (not (looking-at "[^,\t\n ]+")) 
                (progn
                  (beginning-of-line) 
                  (forward-word 1)
                  (insert " " title)
                  (message "Inserted title %s ... " title)))))
      ;; Go forward beyond current section title.
      (re-search-forward texinfo-section-types-regexp 
                         (save-excursion (forward-line 3) (point)) t)
      (setq last-section-position (point))
      (forward-line 1))

    ;; Leave point at end of region, mark at beginning.
    (set-mark beginning)

    (if title-p
      (message
       "Done inserting node lines and titles.  You may save the buffer.")
    (message "Done inserting node lines.  You may save the buffer."))))


;;; Update and create menus for multi-file Texinfo sources

;;  1. M-x texinfo-multiple-files-update 
;;
;;     Read the include file list of an outer Texinfo file and
;;     update all highest level nodes in the files listed and insert a
;;     main menu in the outer file after its top node.

;;  2. C-u M-x texinfo-multiple-files-update 
;;
;;     Same as 1, but insert a master menu.  (Saves reupdating lower
;;     level menus and nodes.)  This command simply reads every menu,
;;     so if the menus are wrong, the master menu will be wrong.
;;     Similarly, if the lower level node pointers are wrong, they
;;     will stay wrong.

;;  3. C-u 2 M-x texinfo-multiple-files-update 
;;
;;     Read the include file list of an outer Texinfo file and
;;     update all nodes and menus in the files listed and insert a
;;     master menu in the outer file after its top node.

;;; Note: these functions:
;;;
;;;   * Do not save or delete any buffers.  You may fill up your memory.
;;;   * Do not handle any pre-existing nodes in outer file.  
;;;     Hence, you may need a file for indices.


;;; Auxiliary functions for multiple file updating

(defun texinfo-multi-file-included-list (outer-file)
  "Return a list of the included files in OUTER-FILE."
  (let ((included-file-list (list outer-file))
        start)
    (save-excursion
      (switch-to-buffer (find-file-noselect outer-file))
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^@include" nil t)
        (skip-chars-forward " \t")
        (setq start (point))
        (end-of-line)
        (skip-chars-backward " \t")   
        (setq included-file-list
              (cons (buffer-substring start (point))
                    included-file-list)))
      (nreverse included-file-list))))

(defun texinfo-copy-next-section-title ()
  "Return the name of the immediately following section as a string.

Start with point at the beginning of the node line.  Leave point at the
same place.  If there is no title, returns an empty string."

  (save-excursion
    (end-of-line)
    (let ((node-end (or 
                        (save-excursion
                          (if (re-search-forward "\\(^@node\\)" nil t)
                              (match-beginning 0)))
                        (point-max))))
      (if (re-search-forward texinfo-section-types-regexp node-end t)
          (progn
            (beginning-of-line)
            ;; copy title
            (let ((title
                   (buffer-substring
                    (progn (forward-word 1)           ; skip over section type
                           (skip-chars-forward " \t") ; and over spaces
                           (point))
                    (progn (end-of-line) (point)))))
              title))
        ""))))

(defun texinfo-multi-file-update (files &optional update-everything)
  "Update first node pointers in each file in FILES.
Return a list of the node names.

The first file in the list is an outer file; the remaining are
files included in the outer file with `@include' commands.

If optional arg UPDATE-EVERYTHING non-nil, update every menu and
pointer in each of the included files.

Also update the `Top' level node pointers of the outer file.

Requirements:

  * the first file in the FILES list must be the outer file,
  * each of the included files must contain exactly one highest
    hierarchical level node, 
  * this node must be the first node in the included file,
  * each highest hierarchical level node must be of the same type.

Thus, normally, each included file contains one, and only one,
chapter."

;; The menu-list has the form:
;; 
;;     \(\(\"node-name1\" . \"title1\"\) 
;;       \(\"node-name2\" . \"title2\"\) ... \)
;; 
;; However, there does not need to be a title field and this function
;; does not fill it; however a comment tells you how to do so.
;; You would use the title field if you wanted to insert titles in the
;; description slot of a menu as a description.
  
  (let ((case-fold-search t)
        menu-list)
    
    ;; Find the name of the first node of the first included file.
    (switch-to-buffer (find-file-noselect (car (cdr files))))
    (widen)
    (goto-char (point-min))
    (if (not (re-search-forward "^@node" nil t))
        (error "No `@node' line found in %s !" (buffer-name)))
    (beginning-of-line)
    (texinfo-check-for-node-name)
    (setq next-node-name (texinfo-copy-node-name))
    
    (setq menu-list
          (cons (cons 
                 next-node-name
                 (prog1 "" (forward-line 1)))
                ;; Use following to insert section titles automatically.
                ;; (texinfo-copy-next-section-title)
                menu-list))

    ;; Go to outer file
    (switch-to-buffer (find-file-noselect (car files)))
    (goto-char (point-min))
    (if (not (re-search-forward "^@node [ \t]*top[ \t]*\\(,\\|$\\)" nil t))
        (error "This buffer needs a Top node!"))
    (beginning-of-line)
    (texinfo-delete-existing-pointers)
    (end-of-line)
    (insert ", " next-node-name ", (dir), (dir)")
    (beginning-of-line)
    (setq previous-node-name "Top")
    (setq files (cdr files))
    
    (while files
      
      (if (not (cdr files))
          ;; No next file
          (setq next-node-name "")
        ;; Else,
        ;; find the name of the first node in the next file.
        (switch-to-buffer (find-file-noselect (car (cdr files))))
        (widen)
        (goto-char (point-min))
        (if (not (re-search-forward "^@node" nil t))
            (error "No `@node' line found in %s !" (buffer-name)))
        (beginning-of-line)
        (texinfo-check-for-node-name)
        (setq next-node-name (texinfo-copy-node-name))
        (setq menu-list
              (cons (cons 
                     next-node-name
                     (prog1 "" (forward-line 1)))
                    ;; Use following to insert section titles automatically.
                    ;; (texinfo-copy-next-section-title)
                    menu-list)))

      ;; Go to node to be updated.
      (switch-to-buffer (find-file-noselect (car files)))
      (goto-char (point-min))
      (if (not (re-search-forward "^@node" nil t))
          (error "No `@node' line found in %s !" (buffer-name)))
      (beginning-of-line)
      
      ;; Update other menus and nodes if requested.
      (if update-everything (texinfo-all-menus-update t))

      (beginning-of-line)
      (texinfo-delete-existing-pointers)
      (end-of-line)
      (insert ", " next-node-name ", " previous-node-name ", " up-node-name)
      
      (beginning-of-line)
      (setq previous-node-name (texinfo-copy-node-name))
      
      (setq files (cdr files)))
    (nreverse menu-list)))

(defun texinfo-multi-files-insert-main-menu (menu-list)
  "Insert formatted main menu at point.
Indents the first line of the description, if any, to the value of
texinfo-column-for-description."

  (insert "@menu\n")
  (while menu-list
    ;; Every menu entry starts with a star and a space.
    (insert "* ")
    
    ;; Insert the node name (and menu entry name, if present).
    (let ((node-part (car (car menu-list))))
      (if (stringp node-part)
          ;; "Double colon" entry line; menu entry and node name are the same,
          (insert (format "%s::" node-part))  
        ;; "Single colon" entry line; menu entry and node name are different.
        (insert (format "%s: %s." (car node-part) (cdr node-part)))))
    
    ;; Insert the description, if present.
    (if (cdr (car menu-list))
        (progn
          ;; Move to right place.
          (indent-to texinfo-column-for-description 2) 
          ;; Insert description.
          (insert (format "%s" (cdr (car menu-list))))))  

    (insert "\n") ; end this menu entry
    (setq menu-list (cdr menu-list)))
  (insert "@end menu"))

(defun texinfo-multi-file-master-menu-list (files-list)
  "Return master menu list from files in FILES-LIST.
Menu entries in each file collected using `texinfo-master-menu-list'.

The first file in FILES-LIST must be the outer file; the others must
be the files included within it.  A main menu must already exist."
  (save-excursion
    (let (master-menu-list)
      (while files-list
        (switch-to-buffer (find-file-noselect (car files-list)))
        (message "Working on: %s " (current-buffer))
        (goto-char (point-min))
        (setq master-menu-list
              (append master-menu-list (texinfo-master-menu-list)))
        (setq files-list (cdr files-list)))
      master-menu-list)))


;;; The multiple-file update function

(defun texinfo-multiple-files-update
  (outer-file &optional update-everything make-master-menu)
  "Update first node pointers in each file included in OUTER-FILE;
create or update the `Top' level node pointers and the main menu in
the outer file that refers to such nodes.  This does not create or
update menus or pointers within the included files.

With optional MAKE-MASTER-MENU argument (prefix arg, if interactive),
insert a master menu in OUTER-FILE in addition to creating or updating
pointers in the first @node line in each included file and creating or
updating the `Top' level node pointers of the outer file.  This does
not create or update other menus and pointers within the included
files.

With optional UPDATE-EVERYTHING argument (numeric prefix arg, if
interactive), update all the menus and all the `Next', `Previous', and
`Up' pointers of all the files included in OUTER-FILE before inserting
a master menu in OUTER-FILE.  Also, update the `Top' level node
pointers of OUTER-FILE.

Notes: 

  * this command does NOT save any files--you must save the
    outer file and any modified, included files.

  * except for the `Top' node, this command does NOT handle any
    pre-existing nodes in the outer file; hence, indices must be
    enclosed in an included file.

Requirements:

  * each of the included files must contain exactly one highest
    hierarchical level node, 
  * this highest node must be the first node in the included file,
  * each highest hierarchical level node must be of the same type.

Thus, normally, each included file contains one, and only one,
chapter."
  
  (interactive (cons
                (read-string
                 "Name of outer `include' file: "
                 (buffer-file-name))
                (cond ((not current-prefix-arg)
                       '(nil nil))
                      ((listp current-prefix-arg)
                       '(t nil))   ; make-master-menu 
                      ((numberp current-prefix-arg)
                       '(t t))     ; update-everything
                      )))

  (let* ((included-file-list (texinfo-multi-file-included-list outer-file))
         (files included-file-list)
         main-menu-list
         next-node-name
         previous-node-name
         (up-node-name "Top"))

;;; Update the pointers 
;;; and collect the names of the nodes and titles
    (setq main-menu-list (texinfo-multi-file-update files update-everything))

;;; Insert main menu

  ;; Go to outer file
  (switch-to-buffer (find-file-noselect (car included-file-list)))
  (if (texinfo-old-menu-p
       (point-min)
       (save-excursion
         (re-search-forward "^@include")
         (beginning-of-line)
         (point)))

      ;; If found, leave point after word `menu' on the `@menu' line.
      (progn
        (texinfo-incorporate-descriptions main-menu-list)
        ;; Delete existing menu.
        (beginning-of-line)
        (delete-region
         (point)
         (save-excursion (re-search-forward "^@end menu") (point)))
        ;; Insert main menu
        (texinfo-multi-files-insert-main-menu main-menu-list))

    ;; Else no current menu; insert it before `@include'
    (texinfo-multi-files-insert-main-menu main-menu-list))

;;; Insert master menu

  (if make-master-menu
      (progn
        ;; First, removing detailed part of any pre-existing master menu
        (goto-char (point-min))
        (if (re-search-forward texinfo-master-menu-header nil t)
            ;; Remove detailed master menu listing
            (progn
              (goto-char (match-beginning 0))
              (let ((end-of-detailed-menu-descriptions
                     (save-excursion     ; beginning of end menu line
                       (goto-char (texinfo-menu-end))
                       (beginning-of-line) (forward-char -1)
                       (point))))
                (delete-region (point) end-of-detailed-menu-descriptions))))

        ;; Create a master menu and insert it
        (texinfo-insert-master-menu-list 
         (texinfo-multi-file-master-menu-list
          included-file-list)))))

  ;; Remove unwanted extra lines.
  (save-excursion
    (goto-char (point-min))
      
    (re-search-forward "^@menu")
    (forward-line -1)
    (insert  "\n") ; Ensure at least one blank line.
    (delete-blank-lines)
      
    (re-search-forward "^@end menu")
    (forward-line 1)
    (insert  "\n") ; Ensure at least one blank line.
    (delete-blank-lines))

  (message "Multiple files updated."))


;;; Place `provide' at end of file.
(provide 'texnfo-upd)

;;; texnfo-upd.el ends here
