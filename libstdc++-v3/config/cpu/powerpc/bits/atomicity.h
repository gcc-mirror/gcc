/* Low-level functions for atomic operations.  PowerPC version.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef _BITS_ATOMICITY_H
#define _BITS_ATOMICITY_H	1

typedef int _Atomic_word;

static inline _Atomic_word
__attribute__ ((__unused__))
__exchange_and_add (volatile _Atomic_word* __mem, int __val)
{
  _Atomic_word __tmp, __res;
  __asm__ __volatile__ (
	"/* Inline exchange & add */\n"
	"0:\t"
	"lwarx    %0,0,%2 \n\t"
	"add%I3   %1,%0,%3 \n\t"
	"stwcx.   %1,0,%2 \n\t"
	"bne-     0b \n\t"
	"/* End exchange & add */"
	: "=&b"(__res), "=&r"(__tmp)
	: "r" (__mem), "Ir"(__val)
	: "cr0", "memory");
  return __res;
}

static inline void
__attribute__ ((__unused__))
__atomic_add (volatile _Atomic_word *__mem, int __val)
{
  _Atomic_word __tmp;
  __asm__ __volatile__ (
	"/* Inline atomic add */\n"
	"0:\t"
	"lwarx    %0,0,%1 \n\t"
	"add%I2   %0,%0,%2 \n\t"
	"stwcx.   %0,0,%1 \n\t"
	"bne-     0b \n\t"
	"/* End atomic add */"
	: "=&b"(__tmp)
	: "r" (__mem), "Ir"(__val)
	: "cr0", "memory");
}

static inline int
__attribute__ ((__unused__))
__compare_and_swap (volatile long *__p, long int __oldval, long int __newval)
{
  int __res;
  __asm__ __volatile__ (
	"/* Inline compare & swap */\n"
	"0:\t"
	"lwarx    %0,0,%1  \n\t"
	"sub%I2c. %0,%0,%2 \n\t"
	"cntlzw   %0,%0 \n\t"
	"bne-     1f \n\t"
	"stwcx.   %3,0,%1 \n\t"
	"bne-     0b \n"
	"1:\n\t"
	"/* End compare & swap */"
	: "=&b"(__res)
	: "r"(__p), "Ir"(__oldval), "r"(__newval)
	: "cr0", "memory");
  return __res >> 5;
}

static inline long
__attribute__ ((__unused__))
__always_swap (volatile long *__p, long int __newval)
{
  long __res;
  __asm__ __volatile__ (
	"/* Inline always swap */\n"
	"0:\t"
	"lwarx    %0,0,%1 \n\t"
	"stwcx.   %2,0,%1 \n\t"
	"bne-     0b \n\t"
	"/* End always swap */"
	: "=&r"(__res)
	: "r"(__p), "r"(__newval)
	: "cr0", "memory");
  return __res;
}

static inline int
__attribute__ ((__unused__))
__test_and_set (volatile long *__p, long int __newval)
{
  int __res;
  __asm__ __volatile__ (
	"/* Inline test & set */\n"
	"0:\t"
	"lwarx    %0,0,%1 \n\t"
	"cmpwi    %0,0 \n\t"
	"bne-     1f \n\t"
	"stwcx.   %2,0,%1 \n\t"
	"bne-     0b \n"
	"1:\n\t"
	"/* End test & set */"
	: "=&r"(__res)
	: "r"(__p), "r"(__newval)
	: "cr0", "memory");
  return __res;
}

#endif /* atomicity.h */

