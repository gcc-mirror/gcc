;;;; texnfo-tex.el

;;; Texinfo mode TeX and hardcopy printing commands.

;; These commands are for running TeX on a region of a Texinfo file in
;; GNU Emacs, or on the whole buffer, and for printing the resulting
;; DVI file.

;;; Version 2.07    22 October 1991
;;; Robert J. Chassell      
;;; Please send bug reports to:  bug-texinfo@prep.ai.mit.edu

;;; Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.


;;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,


;;; The Texinfo mode TeX related commands are:

; texinfo-tex-region        to run tex on the current region.
; texinfo-tex-buffer        to run tex on the current buffer.
; texinfo-texindex          to sort unsorted index files.
; texinfo-tex-print         to print the .dvi file made by tex.
; texinfo-kill-tex-job      to kill the currently running tex job.
; texinfo-recenter-tex-output-buffer    to redisplay tex output buffer.
; texinfo-show-tex-print-queue          to show the print queue.


;;; Keys common both to Texinfo mode and to TeX shell.

;; Defined in `texinfo.el'
; (defun texinfo-define-common-keys (keymap)
;   "Define the keys both in Texinfo mode and in the texinfo-tex-shell."
;   (define-key keymap "\C-c\C-t\C-k"    'texinfo-kill-tex-job)
;   (define-key keymap "\C-c\C-t\C-x"    'texinfo-quit-tex-job)
;   (define-key keymap "\C-c\C-t\C-l"    'texinfo-recenter-tex-output-buffer)
;   (define-key keymap "\C-c\C-t\C-d"    'texinfo-delete-from-tex-print-queue)
;   (define-key keymap "\C-c\C-t\C-q"    'texinfo-show-tex-print-queue)
;   (define-key keymap "\C-c\C-t\C-p"    'texinfo-tex-print)
;   (define-key keymap "\C-c\C-t\C-i"    'texinfo-texindex)
;   (define-key keymap "\C-c\C-t\C-r"    'texinfo-tex-region)
;   (define-key keymap "\C-c\C-t\C-b"    'texinfo-tex-buffer))

;; See also texinfo-tex-start-shell. 
;; The following is executed in the `texinfo.el' file
;(texinfo-define-common-keys texinfo-mode-map)


;;; Variable definitions:

(require 'shell)

(defvar texinfo-tex-shell-cd-command "cd"
  "Command to give to shell running TeX to change directory.")

(defvar texinfo-tex-command "tex"
  "*Command used by  texinfo-tex-region  to run tex on a region.")

(defvar texinfo-texindex-command "texindex"
  "*Command used by  texinfo-texindex  to sort unsorted index files.")

(defvar texinfo-tex-dvi-print-command "lpr -d"
  "*Command string used by \\[tex-print] to print a .dvi file.")

(defvar texinfo-show-tex-queue-command "lpq"
  "*Command string used to show the Texinfo TeX print queue.
Command is used by \\[texinfo-show-tex-print-queue] and it
should show the queue that \\[texinfo-tex-print] puts jobs on.")

(defvar texinfo-delete-from-print-queue-command "lprm"
  "*Command string used to delete a job from the line printer queue.
Command is used by \\[texinfo-delete-from-tex-print-queue] based on
number provided by a previous \\[texinfo-show-tex-print-queue]
command.")

(defvar texinfo-tex-trailer "@bye"
  "String appended after a region sent to TeX by texinfo-tex-region.")

(defvar texinfo-tex-original-file ""
  "Original name of file on which to run TeX.")

(defvar texinfo-tex-temp-file nil
  "Temporary file name used for text being sent as input to TeX.")

(defvar texinfo-tex-root-temp-file nil
  "Temporary file name used for text being sent as input to TeX.")


;;; Texinfo TeX main functions

(defun texinfo-tex-region (beginning end)
  "Run tex on the current region. 

A temporary file is written in the default directory, and tex is run
in that directory.  The first line of the file is copied to the
temporary file; and if the buffer has a header, it is written to the
temporary file before the region itself.  The buffer's header is all
lines between the strings defined by texinfo-start-of-header and
texinfo-end-of-header inclusive.  The header must start in the first 100
lines.  The value of texinfo-tex-trailer is appended to the temporary file
after the region."
  
  (interactive "r")
  (if (get-buffer "*texinfo-tex-shell*")
      (quit-process (get-process "texinfo-tex-shell") t)
    (texinfo-tex-start-shell))
  
  (setq texinfo-tex-root-temp-file
        (expand-file-name 
         (make-temp-name
          (prin1-to-string (read (buffer-name))))))
  
  (let ((texinfo-tex-temp-file (concat texinfo-tex-root-temp-file ".tex")))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line 100)
        (let ((search-end (point))
              (header-beginning (point-min)) (header-end (point-min)))
          (goto-char (point-min))
          ;; Copy first line, the `\input texinfo' line, to temp file
          (write-region (point) 
                        (save-excursion (forward-line 1) (point))
                        texinfo-tex-temp-file nil nil)
          ;; Don't copy first line twice if region includes it.
          (forward-line 1)
          (if (< beginning  (point)) (setq beginning  (point)))
          ;; Initialize the temp file with either the header or nothing
          (if (search-forward texinfo-start-of-header search-end t)
              (progn
                (beginning-of-line)
                (setq header-beginning (point)) ; Mark beginning of header.
                (if (search-forward texinfo-end-of-header nil t)
                    (progn (beginning-of-line)
                           (setq header-end (point)))   ; Mark end of header.
                  (setq header-beginning (point-min))))) ; Else no header.
          ;; Copy  header  to temp file.
          (write-region
           (min header-beginning beginning )
           header-end
           texinfo-tex-temp-file t nil)
          ;; Copy  region  to temp file.
          (write-region
           (max beginning  header-end)
           end
           texinfo-tex-temp-file t nil)
          ;; This is a kludge to insert the texinfo-tex-trailer into the
          ;; texinfo-tex-temp-file.  We have to create a special buffer
          ;; in which to insert the texinfo-tex-trailer first because there is
          ;; no function with which to append a literal string directly
          ;; to a file.
          (let ((local-tex-trailer texinfo-tex-trailer)
                (temp-buffer (get-buffer-create " texinfo-trailer-buffer")))
            (set-buffer temp-buffer)
            (erase-buffer)
            ;; make sure trailer isn't hidden by a comment
            (insert-string "\n")
            (if local-tex-trailer (insert local-tex-trailer))
            (write-region (point-min) (point-max) 
                          texinfo-tex-temp-file t nil)))
        (set-process-sentinel (get-process "texinfo-tex-shell") 
                              'texinfo-tex-shell-sentinel)
        (send-string "texinfo-tex-shell"
                     (concat texinfo-tex-shell-cd-command " "
                             default-directory "\n"))
        (send-string "texinfo-tex-shell"
                     (concat texinfo-tex-command " "
                             texinfo-tex-temp-file "\n  "))
        (texinfo-recenter-tex-output-buffer 0)))))

(defun texinfo-tex-buffer (buffer)
  "Run TeX on current buffer.
After running TeX the first time, you may have to run \\[texinfo-texindex]
and then \\[texinfo-tex-buffer] again."
  (interactive 
   (list
    ;; Sometimes you put point into *texinfo-tex-shell*; this prompts
    ;; you for the correct file regardless.
    (if (and 
         (string= (buffer-name (current-buffer)) "*texinfo-tex-shell*")
         texinfo-tex-root-temp-file)
        (read-string (format "Run TeX on: ")
                     texinfo-tex-original-file)
      (read-string (format "Run TeX on: ") (buffer-name (current-buffer))))))
  
  ;; Set to original buffer if in *texinfo-tex-shell*; otherwise,
  ;; record name of current buffer.
  (if (string= (buffer-name (current-buffer)) "*texinfo-tex-shell*")
      (set-buffer buffer)
    (setq texinfo-tex-original-file
           (buffer-name (current-buffer))))

  (if (get-buffer "*texinfo-tex-shell*")
      (quit-process (get-process "texinfo-tex-shell") t)
    (texinfo-tex-start-shell))
  (cond ((null buffer-file-name)
         (error "Buffer not visiting any file!"))
        ((buffer-modified-p)
         (error "Buffer has been modified since last saved!"))
        (t (set-process-sentinel (get-process "texinfo-tex-shell") 
                                 'texinfo-tex-shell-sentinel)
           (send-string "texinfo-tex-shell"
                        (concat texinfo-tex-shell-cd-command 
                                " "
                                (file-name-directory
                                 (buffer-file-name
                                  (get-buffer buffer)))
                                "\n"))
           (send-string "texinfo-tex-shell"
                        (concat texinfo-tex-command " " buffer "\n  "))
           
           ;; so the texinfo-tex-print command works
           (setq texinfo-tex-root-temp-file
                 (substring buffer 0
                            (or (string-match "\\.tex" buffer)
                                (length buffer))))
           
           (texinfo-recenter-tex-output-buffer 0))))

(defun texinfo-texindex ()
  "Run texindex on unsorted index files.
The index files are made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
Runs the shell command defined by texinfo-texindex-command."
  (interactive)
  (send-string "texinfo-tex-shell"
               (concat texinfo-texindex-command
                       " " texinfo-tex-root-temp-file ".??" "\n"))
  (texinfo-recenter-tex-output-buffer nil))

(defun texinfo-tex-print ()
  "Print .dvi file made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
Runs the shell command defined by texinfo-tex-dvi-print-command."
  (interactive)
  (send-string "texinfo-tex-shell"
               (concat texinfo-tex-dvi-print-command
                       " " texinfo-tex-root-temp-file ".dvi" "\n"))
  (texinfo-recenter-tex-output-buffer nil))


;;; Texinfo TeX utility functions

(defun texinfo-tex-start-shell ()
  (save-excursion
    (require 'texinfo)
    (set-buffer (make-shell "texinfo-tex-shell" "/bin/sh" nil "-v"))
    (setq texinfo-tex-shell-map (copy-keymap shell-mode-map))
    (texinfo-define-common-keys texinfo-tex-shell-map)
    (use-local-map texinfo-tex-shell-map)
    (run-hooks 'texinfo-tex-shell-hook)
    (if (zerop (buffer-size))
        (sleep-for 1))))

(defun texinfo-quit-tex-job ()
  "Quit currently running TeX job, by sending an `x' to it."
  (interactive)
  (if (not (get-process "texinfo-tex-shell"))
      (error "No TeX shell running."))
  (save-excursion
    (set-buffer (get-buffer "*texinfo-tex-shell*"))
    (goto-char (point-max))
    (insert "x")
    (shell-send-input)))

(defun texinfo-kill-tex-job ()
  "Kill the currently running TeX job."
  (interactive)
  (if (get-process "texinfo-tex-shell")
        ;; Use `texinfo-tex-shell-sentinel' to restart
        ;; texinfo-tex-shell after it is killed.
        (kill-process (get-process "texinfo-tex-shell"))))

(defun texinfo-tex-shell-sentinel (process event)
  "Restart texinfo-tex-shell after it is killed."
  (if (equal event "killed\n")
      (save-excursion
        (set-buffer  "*texinfo-tex-shell*")
        (insert "\n")
        (texinfo-tex-start-shell))))

(defun texinfo-recenter-tex-output-buffer (linenum)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((texinfo-tex-shell (get-buffer "*texinfo-tex-shell*"))
        (old-buffer (current-buffer)))
    (if (null texinfo-tex-shell)
        (message "No TeX output buffer")
      (pop-to-buffer texinfo-tex-shell)
      (bury-buffer texinfo-tex-shell)
      (goto-char (point-max))
      (recenter (if linenum
                    (prefix-numeric-value linenum)
                  (/ (window-height) 2)))
      (pop-to-buffer old-buffer)
      )))

(defun texinfo-show-tex-print-queue ()
  "Show the print queue that \\[texinfo-tex-print] put your job on.
Runs the shell command defined by texinfo-show-tex-queue-command."
  (interactive)
  (if (not (texinfo-tex-shell-running-p))
      (texinfo-tex-start-shell))
  (send-string "texinfo-tex-shell"
               (concat texinfo-show-tex-queue-command "\n"))
  (texinfo-recenter-tex-output-buffer nil))

(defun texinfo-delete-from-tex-print-queue (job-number)
  "Delete job from the line printer spooling queue.
You are prompted for the job number (shown by a previous
\\[texinfo-show-tex-print-queue] command."
  (interactive "nPrinter job number for deletion: ")
  (if (texinfo-tex-shell-running-p)
      (texinfo-kill-tex-job)
    (texinfo-tex-start-shell))
  (send-string "texinfo-tex-shell"
               (concat 
                texinfo-delete-from-print-queue-command
                " "
                job-number"\n"))
  (texinfo-recenter-tex-output-buffer nil))

(defun texinfo-tex-shell-running-p ()
  (and (get-process "texinfo-tex-shell")
       (eq (process-status (get-process "texinfo-tex-shell")) 'run)))


;;; Place `provide' at end of file.
(provide 'texnfo-tex)
;;;;;;;;;;;;;;;; end texnfo-tex.el ;;;;;;;;;;;;;;;;
