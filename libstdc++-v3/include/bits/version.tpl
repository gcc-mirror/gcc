/*{ AutoGen5 template                                              -*- C++ -*-
h
(use-modules (srfi srfi-1))
}*/
// Copyright (C) 2023-2025 Free Software Foundation, Inc.

// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/*{ (dne "// ") }*/

/** @file bits/version.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{version}
 */

// Usage guide:
//
// In your usual header, do something like:
//
//   #define __glibcxx_want_ranges
//   #define __glibcxx_want_concepts
//   #include <bits/version.h>
//
// This will generate the FTMs you named, and let you use them in your code as
// if it was user code.  All macros are also exposed under __glibcxx_NAME even
// if unwanted, to permit bits and other FTMs to depend on them for condtional
// computation without exposing extra FTMs to user code.

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/c++config.h>
/*{

;; Helper for dealing with autogens redefined (error)
(define (ferror msg . args)
  (error (apply format (cons* #f msg args))))

  ;; Helper function that, in the context of a single FTM value, generates the
;; condition expression that fulfills the specification of this value.  See the
;; comment block in version.def for an explanation of the format this
;; function parses.
(define (generate-cond)
  (define c++min #f)
  (define gnu++min #f)
  (define gthread #f)
  (define hosted #f)
  (define c++11-abi #f)
  (define extra-cond (get "extra_cond"))

  (define conds '())
  (define (prepend x)
    (if x (set! conds (cons x conds))))

  (if (string-null? extra-cond)
      (set! extra-cond #f))

  (let ((cxxmin (get "cxxmin"))
	(gnuxxmin (get "gnuxxmin"))
	(getstdcond
	 (lambda (var)
	   (let ((ver (get var)))
	     (if (not (string-null? ver))
	       (let ((std-cond (get (format #f "stds[~a]" ver))))
		 (if (string-null? std-cond)
		     (ferror "Standard ~s invalid." ver))
		 std-cond)
	       #f)))))
    (set! c++min (getstdcond "cxxmin"))
    (set! gnu++min (getstdcond "gnuxxmin")))

  (let ((process-tristate
	 (lambda (name)
	   (let ((val (get name)))
	     (cond
	       ((equal? val "") #f)    ; Don't test
	       ((equal? val "yes") "") ; Test directly
	       ((equal? val "no") "!") ; Invert
	       (else (ferror "Bad ~a value ~a." name val)))))))
    (set! gthread (process-tristate "gthread"))
    (set! c++11-abi (process-tristate "cxx11abi"))
    (set! hosted (process-tristate "hosted")))

  (prepend (if extra-cond (format #f "(~a)" extra-cond) #f))
  (prepend (if hosted (format #f "~a~a" hosted "_GLIBCXX_HOSTED") #f))
  (prepend (if gthread (format #f "~a~a" gthread "defined(_GLIBCXX_HAS_GTHREADS)") #f))
  (prepend (if c++11-abi (format #f "~a~a" c++11-abi "_GLIBCXX_USE_CXX11_ABI") #f))

  (prepend
   (let ((strict "defined(__STRICT_ANSI__)")
	 (c++ "__cplusplus"))
     (cond
      ((or (and (equal? c++min gnu++min) c++min)
	   (and (not gnu++min) c++min))
       ;; If we have both values, and they are equal, or we only have gnu++min,
       ;; we want to output a simple check.
       (format #f "(~a ~a)" c++ c++min))
      ((and gnu++min c++min)
       ;; We have differing cases for strict and non-strict modes.
       (format #f "((~a && ~a ~a) || (!~a && ~a ~a))"
	       strict c++ c++min
	       strict c++ gnu++min))
      ((and gnu++min (not c++min))
       (format #f "(!~a && (~a ~a))" strict c++ gnu++min))
      (else #f))))
  (string-join conds " && " 'strict-infix))


  }*/
/*{ FOR ftms
  }*/#if !defined(__cpp_lib_/*{name}*/)
/*{ FOR values }*//*{ #

  This macro block defines two versions of each FTM:

  1. __glibcxx_NAME, which is defined unconditionally, and
  2. __cpp_lib_NAME, which is defined only if marked as wanted.

  This allows FTMs to depend on eachother in their definitions without messing
  with the exported values.

  This can also be used by bits that do not want to expose FTMs that they can't
  implement.

}*/# /*{(unless (first-for?) "el")}*/if /*{(generate-cond)}*/
#  define __glibcxx_/*{name}*/ /*{v}*/L
#  if defined(__glibcxx_want_all) || defined(__glibcxx_want_/*{name}*/)/*{
 IF (not (exist? "no_stdname")) }*/
#   define /*{
;; Compute the name for this FTM based on stdname/name.
(if (exist? "stdname")
    (get "stdname")
    (format #f "__cpp_lib_~a" (get "name")))
}*/ /*{v}*/L/*{
 ENDIF no_std_name }*/
#  endif
/*{ ENDFOR values
  }*/# endif
#endif /* !defined(__cpp_lib_/*{name}*/) && defined(__glibcxx_want_/*{name}*/) */
#undef __glibcxx_want_/*{name
}*//*{ (unless (last-for?) "\n\n" "\n")}*/
/*{ ENDFOR ftms }*//*{

;; Helper that generates [LO, HI].
(define (closed-int-set lo hi)
  (iota (+ (- hi lo) 1) lo))

;; Sanity checking for duplicates and for value order.
(let ((ht (make-hash-table (count "ftms"))))
  (for-each
   (lambda (idx)
     (let ((name (get (format #f "ftms[~a].name" idx))))
       (if (string-null? name) (ferror "No name for FTM ~a" idx))
       (let ((cur (cdr (or (hash-get-handle ht name) '(1 . 0)))))
	 (hash-set! ht name (+ cur 1)))))
   (closed-int-set (low-lim "ftms") (high-lim "ftms")))
  (if (hash-fold
       (lambda (name count prior)
	 (if (= 1 count)
	     prior
	     (begin
	       (printf "FTM %s appears %d times.\n" name count)
	       #t)))
       #f ht)
      (error "Duplicates found.")))

(define (check-value-order ftm key order)
  (let ((valmin 999999)) ; TODO(arsen): bump before year 10000
    (for-each
     (lambda (vidx)
       (let* ((sval (get (format #f "values[~a].~a" vidx key)))
	      (val (string->number sval)))
	 (unless (string-null? sval)
	   (unless val (ferror "Bad value in FTM ~a" ftm))
	   (if (order val valmin)
	       (ferror "Found inverted ~s value in FTM ~a: ~a" key ftm sval))
	   (set! valmin val))))
     (closed-int-set (low-lim "values") (high-lim "values")))))

}*//*{ FOR ftms }*//*{#
  Check for values that are in ascending order.  Because it is generally the
  case that FTMs increase as the values of tests they probe for do, we check
  them to prevent simple, silly errors.

  We're iterating in a separate FOR block rather than in pure Guile since, for
  some reason, high-lim and low-lim do not work with complex names that include
  periods and indices (whereas exist? and others do).
}*//*{
(let ((ftm (get "name")))
  (check-value-order (get "name") "v" >)
  (check-value-order (get "name") "cxxmin" >)
  (check-value-order (get "name") "gnuxxmin" >))
}*//*{ ENDFOR ftms }*/
#undef __glibcxx_want_all
