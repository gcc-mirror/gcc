// Explicit instantiation file.

// Copyright (C) 1999, 2001 Free Software Foundation, Inc.
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

//
// ISO C++ 14882:
//

#include <bits/c++config.h>
#include <bits/stl_alloc.h>
#include <bits/std_vector.h>
#include <bits/std_ostream.h>

namespace std
{

  template class __malloc_alloc_template<0>;

#ifndef __USE_MALLOC
  template class __default_alloc_template<__NODE_ALLOCATOR_THREADS, 0>;
#endif

  template
    void
    vector<unsigned int>::
    _M_insert_aux(vector<unsigned int>::iterator, unsigned int const &);

#ifdef _GLIBCPP_CONCEPT_CHECKS
  template
    void __sink_unused_warning<unsigned int>(unsigned int);

  template
    void __sink_unused_warning<locale::facet*>(locale::facet*);

  template
    void __sink_unused_warning<char>(char);

  template
    void __sink_unused_warning<ostreambuf_iterator<char> >
    (ostreambuf_iterator<char>);

# ifdef _GLIBCPP_USE_WCHAR_T
  template
    void __sink_unused_warning<wchar_t>(wchar_t);

  template
    void __sink_unused_warning<ostreambuf_iterator<wchar_t> > 
    (ostreambuf_iterator<wchar_t>);
# endif
#endif
} //std
