#!/usr/local/bin/guile -s
!#

;  Guile/JNI/JVM Testing Framework
;
;  Copyright (c) 1998 Free Software Foundation, Inc.
;  Written by Paul Fisher (rao@gnu.org)
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, 
;  USA.


; log filenames
(define verbose-log-file "classpath.log")
(define summary-log-file "classpath.sum")

; returns the number of times that ELEM appears in the toplevel of LS
(define count
  (lambda (elem ls)
    (letrec 
	((count-it 
	  (lambda (ls acc)
	    (cond
	     ((null? ls) acc)
	     ((equal? (car ls) elem) (count-it (cdr ls) (+ acc 1)))
	     (else (count-it (cdr ls) acc))))))
      (count-it ls 0))))

; returns a list of pairs containing an element of ELS along with the 
; number of times that element appears in LS
(define build-result-count
  (lambda (els ls)
    (cond
     ((null? els) '())
     (else (cons (cons (car els) (count (car els) ls)) 
		 (build-result-count (cdr els) ls))))))

; soft port which sends output to both (current-output-port) and
; the verbose-log-port
(define screen-and-log-port 
  (make-soft-port
   (vector
    (lambda (c) 
      (cond
       ((char=? c #\newline) 
	(newline (current-output-port))
	(newline verbose-log-port))
       (else
	(write c (current-output-port))
	(write c verbose-log-port))))
    (lambda (s) 
      (display s (current-output-port))
      (display s verbose-log-port))
    (lambda () 
      (force-output (current-output-port))
      (force-output verbose-log-port))
    #f
    #f)
   "w"))

; pretty prints the result of a single test
(define display-test-summary
  (lambda (result port)
    (let ((name (car result))
	  (code (cadr result))
	  (msg (caddr result)))
      (display "Name    : " port)
      (display name port)
      (newline port)
      (display "Result  : " port)
      (display code port)
      (newline port)
      (display "Message : " port)
      (if (= (string-length msg) 0)
	  (display "None" port)
	  (display msg port))
      (newline port)
      (newline port))))

; status message
(define display-running
  (lambda (class port)
    (display "Running " port) 
    (display class port) 
    (display "..." port)
    (newline port)))

; runs the test named CLASS
(define run-test
  (lambda (class)
    (display-running class screen-and-log-port)
    (force-output verbose-log-port)
    (let ((result (test class)))
      (display-test-summary result screen-and-log-port)
      (write (cons class result) summary-log-port)
      (newline summary-log-port)
      (cadr result))))

; run each and every test.  each test is read from PORT
; and delimited by a newline.  returns a list of all test result codes
(define parse-input-file
  (lambda (port)
    (letrec 
	((parse-line
	  (lambda (line)
	    (cond
	     ((eof-object? (car line)) '())
	     ((= (string-length (car line)) 0) 
	      (parse-line (read-line port 'split)))
	     (else (cons (run-test (car line)) 
			 (parse-line
			  (read-line port 'split))))))))
      (parse-line (read-line port 'split)))))

; pretty prints the result list
(define display-results
  (lambda (results port)
    (display "Summary information..." port)
    (newline port)
    (letrec ((display-results-l
	      (lambda (ls)
		(cond
		 ((null? ls))
		 (else
		  (let ((res (car ls)))
		    (display "# of " port)
		    (display (car res) port)
		    (display "'s " port)
		    (display (cdr res) port)
		    (newline port))
		  (display-results-l (cdr ls)))))))
      (display-results-l results))))

(if (batch-mode?) 
    (if (> (length (command-line)) 1)
	(define input-port (open-input-file (cadr (command-line))))
	(error "filename listing tests to execute must be specified.")))

; open up the log files
(define verbose-log-port (open verbose-log-file 
			       (logior O_WRONLY O_CREAT O_TRUNC)))
(define summary-log-port (open summary-log-file
			       (logior O_WRONLY O_CREAT O_TRUNC)))

; redirect stderr to the verbose log
(dup verbose-log-port 2)

; run the tests, and build the result table, and display the results
(display-results (build-result-count 
		  '(PASS XPASS FAIL XPAIL UNRESOLVED
			 UNSUPPORTED UNTESTED ERROR)
		  (parse-input-file input-port)) screen-and-log-port)
