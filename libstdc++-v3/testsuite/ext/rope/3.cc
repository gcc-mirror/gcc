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

const char base[] =
"Happy families are all alike; every unhappy family is unhappy in   \
its own way.							    \
								    \
Everything was in confusion in the Oblonskys' house.  The wife	    \
had discovered that the husband was carrying on an intrigue with    \
a French girl, who had been a governess in their family, and she    \
had announced to her husband that she could not go on living in	    \
the same house with him.  This position of affairs had now lasted   \
three days, and not only the husband and wife themselves, but all   \
the members of their family and household, were painfully	    \
conscious of it.  Every person in the house felt that there was	    \
so sense in their living together, and that the stray people	    \
brought together by chance in any inn had more in common with one   \
another than they, the members of the family and household of the   \
Oblonskys.  The wife did not leave her own room, the husband had    \
not been at home for three days.  The children ran wild all over    \
the house; the English governess quarreled with the housekeeper,    \
and wrote to a friend asking her to look out for a new situation    \
for her; the man-cook had walked off the day before just at	    \
dinner time; the kitchen-maid, and the coachman had given	    \
warning."							
  ;

int baselen = sizeof(base) - 1;

template<class StringType>
StringType
multiply(const StringType& s, int n)
{
  StringType result;
  while (n > 0)
    {
      result += s;
      --n;
    }
  return result;
}

template <class StringType>
StringType
mung_substrings(const StringType& s, int len, int n, int skip)
{
  StringType result;
  int start = 0;
  while (n > 0)
    {
      StringType tmp = s.substr (start, len);
      result += tmp;
      --n;
      start += skip;
    }
  return result;
}

void 
test01()
{
  using namespace __gnu_cxx;
  bool test __attribute__((unused)) = true;

  crope r;
  r = multiply(crope(base), 100000);

  crope r1;
  r1 = mung_substrings(r, 100000, 500, 73);

  VERIFY( r1.size() == 50000000 );
  VERIFY( r1.substr(88888, 6)[0] == 's' );
  VERIFY( r1.substr(88888, 6)[2] == 'h' );
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
