// Explicit instantiation file.

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2009
// Free Software Foundation, Inc.
//
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

//
// ISO C++ 14882:
//

#include <valarray>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Some explicit instantiations.
  template void
     __valarray_fill(size_t* __restrict__, size_t, const size_t&);
  
  template void
     __valarray_copy(const size_t* __restrict__, size_t, size_t* __restrict__);
  
  template valarray<size_t>::valarray(size_t);
  template valarray<size_t>::valarray(const valarray<size_t>&);
  template valarray<size_t>::~valarray();
  template size_t valarray<size_t>::size() const;
  template size_t& valarray<size_t>::operator[](size_t);

  inline size_t
  __valarray_product(const valarray<size_t>& __a)
  {
    const size_t __n = __a.size();
    // XXX: This ugly cast is necessary because
    //      valarray::operator[]() const return a VALUE!
    //      Try to get the committee to correct that gross error.
    valarray<size_t>& __t = const_cast<valarray<size_t>&>(__a);
    return __valarray_product(&__t[0], &__t[0] + __n);
  }
  
  // Map a gslice, described by its multidimensional LENGTHS
  // and corresponding STRIDES, to a linear array of INDEXES
  // for the purpose of indexing a flat, one-dimensional array
  // representation of a gslice_array.
  void
  __gslice_to_index(size_t __o, const valarray<size_t>& __l,
                    const valarray<size_t>& __s, valarray<size_t>& __i)
  {
    // There are as many dimensions as there are strides.
    const size_t __n = __l.size();

    // Holds current multi-index as we go through the gslice for the
    // purpose of computing its linear-image.
    valarray<size_t> __t(__l);

    // Note that this should match the product of all numbers appearing
    // in __l which describes the multidimensional sizes of the
    // generalized slice.
    const size_t __z = __i.size();

    for (size_t __j = 0; __j < __z; ++__j)
      {
	// Compute the linear-index image of (t_0, ... t_{n-1}).
	__i[__j] = __o;

	--__t[__n - 1];
	__o += __s[__n - 1];

        // Process the next multi-index.  The loop ought to be
        // backward since we're making a lexicographical visit.
        for (size_t __k2 = __n - 1; __k2 && !__t[__k2]; --__k2)
          {
	    __o -= __s[__k2] * __l[__k2];
	    __t[__k2] = __l[__k2];

	    --__t[__k2 - 1];
	    __o += __s[__k2 - 1];
          }
      }
  }
  
  gslice::_Indexer::_Indexer(size_t __o, const valarray<size_t>& __l,
                             const valarray<size_t>& __s)
  : _M_count(1), _M_start(__o), _M_size(__l), _M_stride(__s),
    _M_index(__l.size() == 0 ? 0 : __valarray_product(__l))
  { __gslice_to_index(__o, __l, __s, _M_index); }  

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
