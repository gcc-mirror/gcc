// 1999-07-28 bkoz

// Copyright (C) 1999, 2001, 2003 Free Software Foundation
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

// 27.6.1.2.3 basic_istream::operator>>
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <istream>
#include <fstream>
#include <testsuite_hooks.h>

// filebufs.
void test02() 
{
  bool test __attribute__((unused)) = true;
  typedef std::ios::traits_type ctraits_type;
  const char name_01[] = "istream_extractor_other-1.txt"; //read 
  const char name_02[] = "istream_extractor_other-2.txt"; //write

  std::filebuf fbin, fbout;
  fbin.open(name_01, std::ios_base::in);
  fbout.open(name_02, std::ios_base::out | std::ios_base::trunc);
  VERIFY( fbin.is_open() );
  VERIFY( fbout.is_open() );

  if (test)
    {
      std::istream is(&fbin);
      is.unsetf(std::ios_base::skipws);
      is >> &fbout;
    }

  fbout.close();
  fbin.close();
  VERIFY( !fbin.is_open() );
  VERIFY( !fbout.is_open() );
}

int main()
{
  test02();
  return 0;
}
