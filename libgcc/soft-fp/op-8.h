/* Software floating-point emulation.
   Basic eight-word fraction declaration and manipulation.
   Copyright (C) 1997-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson (rth@cygnus.com),
		  Jakub Jelinek (jj@ultra.linux.cz) and
		  Peter Maydell (pmaydell@chiark.greenend.org.uk).

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   In addition to the permissions in the GNU Lesser General Public
   License, the Free Software Foundation gives you unlimited
   permission to link the compiled version of this file into
   combinations with other programs, and to distribute those
   combinations without any restriction coming from the use of this
   file.  (The Lesser General Public License restrictions do apply in
   other respects; for example, they cover modification of the file,
   and distribution when not linked into a combine executable.)

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef SOFT_FP_OP_8_H
#define SOFT_FP_OP_8_H	1

/* We need just a few things from here for op-4, if we ever need some
   other macros, they can be added.  */
#define _FP_FRAC_DECL_8(X)	_FP_W_TYPE X##_f[8]
#define _FP_FRAC_HIGH_8(X)	(X##_f[7])
#define _FP_FRAC_LOW_8(X)	(X##_f[0])
#define _FP_FRAC_WORD_8(X, w)	(X##_f[w])

#define _FP_FRAC_SLL_8(X, N)						\
  do									\
    {									\
      _FP_I_TYPE _FP_FRAC_SLL_8_up, _FP_FRAC_SLL_8_down;		\
      _FP_I_TYPE _FP_FRAC_SLL_8_skip, _FP_FRAC_SLL_8_i;			\
      _FP_FRAC_SLL_8_skip = (N) / _FP_W_TYPE_SIZE;			\
      _FP_FRAC_SLL_8_up = (N) % _FP_W_TYPE_SIZE;			\
      _FP_FRAC_SLL_8_down = _FP_W_TYPE_SIZE - _FP_FRAC_SLL_8_up;	\
      if (!_FP_FRAC_SLL_8_up)						\
	for (_FP_FRAC_SLL_8_i = 7;					\
	     _FP_FRAC_SLL_8_i >= _FP_FRAC_SLL_8_skip;			\
	     --_FP_FRAC_SLL_8_i)					\
	  X##_f[_FP_FRAC_SLL_8_i]					\
	    = X##_f[_FP_FRAC_SLL_8_i-_FP_FRAC_SLL_8_skip];		\
      else								\
	{								\
	  for (_FP_FRAC_SLL_8_i = 7;					\
	       _FP_FRAC_SLL_8_i > _FP_FRAC_SLL_8_skip;			\
	       --_FP_FRAC_SLL_8_i)					\
	    X##_f[_FP_FRAC_SLL_8_i]					\
	      = ((X##_f[_FP_FRAC_SLL_8_i-_FP_FRAC_SLL_8_skip]		\
		  << _FP_FRAC_SLL_8_up)					\
		 | (X##_f[_FP_FRAC_SLL_8_i-_FP_FRAC_SLL_8_skip-1]	\
		    >> _FP_FRAC_SLL_8_down));				\
	  X##_f[_FP_FRAC_SLL_8_i--] = X##_f[0] << _FP_FRAC_SLL_8_up;	\
	}								\
      for (; _FP_FRAC_SLL_8_i >= 0; --_FP_FRAC_SLL_8_i)			\
	X##_f[_FP_FRAC_SLL_8_i] = 0;					\
    }									\
  while (0)

#define _FP_FRAC_SRL_8(X, N)						\
  do									\
    {									\
      _FP_I_TYPE _FP_FRAC_SRL_8_up, _FP_FRAC_SRL_8_down;		\
      _FP_I_TYPE _FP_FRAC_SRL_8_skip, _FP_FRAC_SRL_8_i;			\
      _FP_FRAC_SRL_8_skip = (N) / _FP_W_TYPE_SIZE;			\
      _FP_FRAC_SRL_8_down = (N) % _FP_W_TYPE_SIZE;			\
      _FP_FRAC_SRL_8_up = _FP_W_TYPE_SIZE - _FP_FRAC_SRL_8_down;	\
      if (!_FP_FRAC_SRL_8_down)						\
	for (_FP_FRAC_SRL_8_i = 0;					\
	     _FP_FRAC_SRL_8_i <= 7-_FP_FRAC_SRL_8_skip;			\
	     ++_FP_FRAC_SRL_8_i)					\
	  X##_f[_FP_FRAC_SRL_8_i]					\
	    = X##_f[_FP_FRAC_SRL_8_i+_FP_FRAC_SRL_8_skip];		\
      else								\
	{								\
	  for (_FP_FRAC_SRL_8_i = 0;					\
	       _FP_FRAC_SRL_8_i < 7-_FP_FRAC_SRL_8_skip;		\
	       ++_FP_FRAC_SRL_8_i)					\
	    X##_f[_FP_FRAC_SRL_8_i]					\
	      = ((X##_f[_FP_FRAC_SRL_8_i+_FP_FRAC_SRL_8_skip]		\
		  >> _FP_FRAC_SRL_8_down)				\
		 | (X##_f[_FP_FRAC_SRL_8_i+_FP_FRAC_SRL_8_skip+1]	\
		    << _FP_FRAC_SRL_8_up));				\
	  X##_f[_FP_FRAC_SRL_8_i++] = X##_f[7] >> _FP_FRAC_SRL_8_down;	\
	}								\
      for (; _FP_FRAC_SRL_8_i < 8; ++_FP_FRAC_SRL_8_i)			\
	X##_f[_FP_FRAC_SRL_8_i] = 0;					\
    }									\
  while (0)


/* Right shift with sticky-lsb.
   What this actually means is that we do a standard right-shift,
   but that if any of the bits that fall off the right hand side
   were one then we always set the LSbit.  */
#define _FP_FRAC_SRS_8(X, N, size)					\
  do									\
    {									\
      _FP_I_TYPE _FP_FRAC_SRS_8_up, _FP_FRAC_SRS_8_down;		\
      _FP_I_TYPE _FP_FRAC_SRS_8_skip, _FP_FRAC_SRS_8_i;			\
      _FP_W_TYPE _FP_FRAC_SRS_8_s;					\
      _FP_FRAC_SRS_8_skip = (N) / _FP_W_TYPE_SIZE;			\
      _FP_FRAC_SRS_8_down = (N) % _FP_W_TYPE_SIZE;			\
      _FP_FRAC_SRS_8_up = _FP_W_TYPE_SIZE - _FP_FRAC_SRS_8_down;	\
      for (_FP_FRAC_SRS_8_s = _FP_FRAC_SRS_8_i = 0;			\
	   _FP_FRAC_SRS_8_i < _FP_FRAC_SRS_8_skip;			\
	   ++_FP_FRAC_SRS_8_i)						\
	_FP_FRAC_SRS_8_s |= X##_f[_FP_FRAC_SRS_8_i];			\
      if (!_FP_FRAC_SRS_8_down)						\
	for (_FP_FRAC_SRS_8_i = 0;					\
	     _FP_FRAC_SRS_8_i <= 7-_FP_FRAC_SRS_8_skip;			\
	     ++_FP_FRAC_SRS_8_i)					\
	  X##_f[_FP_FRAC_SRS_8_i]					\
	    = X##_f[_FP_FRAC_SRS_8_i+_FP_FRAC_SRS_8_skip];		\
      else								\
	{								\
	  _FP_FRAC_SRS_8_s						\
	    |= X##_f[_FP_FRAC_SRS_8_i] << _FP_FRAC_SRS_8_up;		\
	  for (_FP_FRAC_SRS_8_i = 0;					\
	       _FP_FRAC_SRS_8_i < 7-_FP_FRAC_SRS_8_skip;		\
	       ++_FP_FRAC_SRS_8_i)					\
	    X##_f[_FP_FRAC_SRS_8_i]					\
	      = ((X##_f[_FP_FRAC_SRS_8_i+_FP_FRAC_SRS_8_skip]		\
		  >> _FP_FRAC_SRS_8_down)				\
		 | (X##_f[_FP_FRAC_SRS_8_i+_FP_FRAC_SRS_8_skip+1]	\
		    << _FP_FRAC_SRS_8_up));				\
	  X##_f[_FP_FRAC_SRS_8_i++] = X##_f[7] >> _FP_FRAC_SRS_8_down;	\
	}								\
      for (; _FP_FRAC_SRS_8_i < 8; ++_FP_FRAC_SRS_8_i)			\
	X##_f[_FP_FRAC_SRS_8_i] = 0;					\
      /* Don't fix the LSB until the very end when we're sure f[0] is	\
	 stable.  */							\
      X##_f[0] |= (_FP_FRAC_SRS_8_s != 0);				\
    }									\
  while (0)

#endif /* !SOFT_FP_OP_8_H */
