// Copyright (C) 2004 Free Software Foundation, Inc.
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

// rope (SGI extension)

#include <ext/rope>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;
  using namespace __gnu_cxx;
  bool test __attribute__((unused)) = true;

  crope r(10000, 'x');
  crope r2 = r + "abc" + r;
  crope r3 = r2.substr(10000, 3); 
  crope r4 = r2.substr(10000, 10000); 

  reverse(r2.mutable_begin(), r2.mutable_end());
  VERIFY( r2[10000] == 'c' );

  crope r5('a');
  r5.push_front('b');
  VERIFY( r5[0] == 'b' );
  VERIFY( r5[1] == 'a' );
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<__gnu_cxx::_Rope_RopeLeaf<char, std::allocator<char> > >;
template class __gnu_cxx::__mt_alloc<__gnu_cxx::_Rope_RopeFunction<char, std::allocator<char> > >;
template class __gnu_cxx::__mt_alloc<__gnu_cxx::_Rope_RopeSubstring<char, std::allocator<char> > >;
template class __gnu_cxx::__mt_alloc<__gnu_cxx::_Rope_RopeConcatenation<char, std::allocator<char> > >;
#endif

int main()
{
  test01();
  return 0;
}

