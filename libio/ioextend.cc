/* 
Copyright (C) 1993 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

#include <iostream.h>

static int __xalloc = 0;

int ios::xalloc()
{
  return __xalloc++;
}

static ios::fmtflags __used_fmt_flags
= ios::skipws | ios::left | ios::right | ios::internal
| ios::dec | ios::oct | ios::hex | ios::showbase | ios::showpoint
| ios::uppercase | ios::showpos | ios::scientific | ios::fixed
#ifndef _IO_NEW_STREAMS
| ios::dont_close
#endif
| ios::unitbuf | ios::stdio;

ios::fmtflags ios::bitalloc()
{
  fmtflags bit_to_try = (fmtflags)1;
  for (; bit_to_try; bit_to_try <<= 1)
    {
      if ((__used_fmt_flags & bit_to_try) == 0)
	{
	  __used_fmt_flags |= bit_to_try;
	  return bit_to_try;
	}
    }
  return 0;
}

// NOTE:  This implementation of ios::iword and ios::pword assumes
// that these methods are seldom used, so we want to minimize
// the space and time overhead when NOT using these methods.
//
// ANSI specifies two conceptually-infinite arrays, one whose
// elements are longs, and one whose elements are (void*)s.
// We implement this as a single array, each of whose element is
// a (struct ptr_and_long), which has space for both a long and a void*.
// We also specify that (the i field of) the 0'th element of the array
// contains the allocated length of the array (not counting that
// initial element).

struct ptr_and_long {
  void *p;
  long i;
};

static struct ptr_and_long&
get_array_element(ios& io, int index)
{
  if (index < 0)
    io._throw_failure();
  register struct ptr_and_long *array = (ptr_and_long*)io._arrays;
  int old_length = array == NULL ? 0 : array[0].i;
  if (index >= old_length)
    {
      register int i;
      int new_length = index + 10;
      register struct ptr_and_long *new_array
	= new ptr_and_long[1 + new_length];
      if (array != NULL)
	{
	  for (i = 1; i <= old_length; i++)
	    new_array[i] = array[i];
	  delete [] array;
	}
      for (i = old_length + 1; i <= new_length; i++)
	{
	  new_array[i].i = 0;
	  new_array[i].p = NULL;
	}
      new_array[0].i = new_length;
      new_array[0].p = NULL;
      io._arrays = (void*)new_array;
      array = new_array;
    }
  return array[index+1];
}

long& ios::iword(int index)
{
  return get_array_element(*this, index).i;
}

void*& ios::pword(int index)
{
  return get_array_element(*this, index).p;
}

long ios::iword(int index) const
{
  if (index < 0)
    _throw_failure();
  register struct ptr_and_long *pl_array = (ptr_and_long*)_arrays;
  int len = pl_array == NULL ? 0 : pl_array[0].i;
  return index >= len ? 0 : pl_array[index+1].i;
}

void* ios::pword(int index) const
{
  if (index < 0)
    _throw_failure();
  register struct ptr_and_long *pl_array = (ptr_and_long*)_arrays;
  int len = pl_array == NULL ? 0 : pl_array[0].i;
  return index >= len ? 0 : pl_array[index+1].p;
}
