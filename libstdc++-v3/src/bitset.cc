/* Low-level implementation of bitset template class from ISO C++.  */

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.


#include <bits/std_bitset.h>
#include <bits/std_climits.h>


static const size_t word_bits = sizeof (__bitset_word) * CHAR_BIT;


void
__bitset_base_lshift (__bitset_word *arr, size_t n, size_t nlast,
		      size_t offset, size_t nbits)
{
  arr[n - 1] = arr[n - 1 - offset] << nbits;

  for (size_t cnt = n - 1; cnt > offset; --cnt)
    {
      arr[cnt] |= arr[cnt - 1 - offset] >> (word_bits - nbits);
      arr[cnt - 1] = arr[cnt - 1 - offset] << nbits;
    }

  memset (arr, '\0', offset * sizeof (__bitset_word));
}


void
__bitset_base_rshift (__bitset_word *arr, size_t n, size_t nlast,
		      size_t offset, size_t nbits)
{
  arr[0] = arr[offset] >> nbits;

  for (size_t cnt = offset + 1; cnt < n; ++cnt)
    {
      arr[cnt - 1 - offset] |= arr[cnt] << (word_bits - nbits);
      arr[cnt - offset] = arr[cnt] >> nbits;
    }

  memset (&arr[n - offset], '\0', offset * sizeof (__bitset_word));
}


size_t
__bitset_base_count (__bitset_word *arr, size_t n)
{
  size_t result = 0;

  while (n > 0)
    {
      __bitset_word word = arr[n];

      if (sizeof (__bitset_word) == 4)
	{
	  word = (word & 0x55555555) + ((word >> 1) & 0x55555555);
	  word = (word & 0x33333333) + ((word >> 2) & 0x33333333);
	  word = (word + (word >> 4)) & 0x0f0f0f0f;
	  word = word + (word >> 8);
	  word = (word + (word >> 16)) & 0xff;
	}
      else
	{
	  // The only other possibility is a 64 bit word.
	  word = ((word & ((0x55555555UL << 16) << 16 | 0x55555555UL))
		  + ((word >> 1)
		     & ((0x55555555UL << 16) << 16 | 0x55555555UL)));
	  word = ((word & ((0x33333333UL << 16) << 16 | 0x33333333UL))
		  + ((word >> 1)
		     & ((0x33333333UL << 16) << 16 | 0x33333333UL)));
	  word = ((word + (word >> 4))
		  & ((0x0f0f0f0fUL << 16) << 16 | 0x0f0f0f0fUL));
	  word = (word + (word >> 8));
	  word = (word + (word >> 16));
	  word = (word + ((word >> 16) >> 16)) & 0xff;
	}

      result += word;
    }

  return result;
}
