/*  Copyright (C) 2014-2025 Free Software Foundation, Inc.
    Contributed by Dimitar Dimitrov <dimitar@dinux.eu>

  This file is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 3, or (at your option) any
  later version.

  This file is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  Under Section 7 of GPL version 3, you are granted additional
  permissions described in the GCC Runtime Library Exception, version
  3.1, as published by the Free Software Foundation.

  You should have received a copy of the GNU General Public License and
  a copy of the GCC Runtime Library Exception along with this program;
  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  <http://www.gnu.org/licenses/>.  */

/* ANSI concatenation macros.  */

#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

/* Use the right prefix for global labels.  */

#define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

#define FUNC(X)		.type SYM(X),@function
#define HIDDEN_FUNC(X)	FUNC(X)` .hidden SYM(X)
#define ENDFUNC0(X)	CONCAT1(.Lfe_,X): .size X,CONCAT1(.Lfe_,X)-X
#define ENDFUNC(X)	ENDFUNC0(SYM(X))
