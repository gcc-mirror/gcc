// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 6.3.4.6 unordered_multimap::swap

#include <tr1/unordered_map>
#include <map>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator as a non-empty allocator.
void
test01()
{
  using namespace std::tr1;
  using std::pair;
  using std::equal_to;
  using std::map;
  using std::multimap;

  typedef pair<const char, int> my_pair;
  typedef __gnu_test::uneq_allocator<my_pair> my_alloc;
  typedef unordered_multimap<char, int, hash<char>, equal_to<char>, my_alloc>
    my_ummap;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  typedef multimap<char, int> my_mmap;
  my_mmap mmap01_ref;
  for (size_t i = 0; i < N1; ++i)
    mmap01_ref.insert(my_pair(title01[i], i));
  my_mmap mmap02_ref;
  for (size_t i = 0; i < N2; ++i)
    mmap02_ref.insert(my_pair(title02[i], i));
  my_mmap mmap03_ref;
  for (size_t i = 0; i < N3; ++i)
    mmap03_ref.insert(my_pair(title03[i], i));
  my_mmap mmap04_ref;
  for (size_t i = 0; i < N4; ++i)
    mmap04_ref.insert(my_pair(title04[i], i));

  typedef map<char, int> my_map;

  my_ummap::size_type size01, size02;

  my_alloc alloc01(1);

  my_ummap ummap01(10, hash<char>(), equal_to<char>(), alloc01);
  size01 = ummap01.size();
  my_ummap ummap02(10, hash<char>(), equal_to<char>(), alloc01);
  size02 = ummap02.size();

  ummap01.swap(ummap02);
  VERIFY( ummap01.size() == size02 );
  VERIFY( ummap01.empty() );
  VERIFY( ummap02.size() == size01 );
  VERIFY( ummap02.empty() );

  my_ummap ummap03(10, hash<char>(), equal_to<char>(), alloc01);
  size01 = ummap03.size();
  my_ummap ummap04(mmap02_ref.begin(), mmap02_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = ummap04.size();

  ummap03.swap(ummap04);
  VERIFY( ummap03.size() == size02 );
  VERIFY( my_map(ummap03.begin(), ummap03.end())
	  == my_map(mmap02_ref.begin(), mmap02_ref.end()) );
  VERIFY( ummap04.size() == size01 );
  VERIFY( ummap04.empty() );

  my_ummap ummap05(mmap01_ref.begin(), mmap01_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = ummap05.size();
  my_ummap ummap06(mmap02_ref.begin(), mmap02_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = ummap06.size();

  ummap05.swap(ummap06);
  VERIFY( ummap05.size() == size02 );
  VERIFY( my_map(ummap05.begin(), ummap05.end())
	  == my_map(mmap02_ref.begin(), mmap02_ref.end()) );
  VERIFY( ummap06.size() == size01 );
  VERIFY( my_map(ummap06.begin(), ummap06.end())
	  == my_map(mmap01_ref.begin(), mmap01_ref.end()) );

  my_ummap ummap07(mmap01_ref.begin(), mmap01_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = ummap07.size();
  my_ummap ummap08(mmap03_ref.begin(), mmap03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = ummap08.size();

  ummap07.swap(ummap08);
  VERIFY( ummap07.size() == size02 );
  VERIFY( my_map(ummap07.begin(), ummap07.end())
	  == my_map(mmap03_ref.begin(), mmap03_ref.end()) );
  VERIFY( ummap08.size() == size01 );
  VERIFY( my_map(ummap08.begin(), ummap08.end())
	  == my_map(mmap01_ref.begin(), mmap01_ref.end()) );

  my_ummap ummap09(mmap03_ref.begin(), mmap03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = ummap09.size();
  my_ummap ummap10(mmap04_ref.begin(), mmap04_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = ummap10.size();

  ummap09.swap(ummap10);
  VERIFY( ummap09.size() == size02 );
  VERIFY( my_map(ummap09.begin(), ummap09.end())
	  == my_map(mmap04_ref.begin(), mmap04_ref.end()) );
  VERIFY( ummap10.size() == size01 );
  VERIFY( my_map(ummap10.begin(), ummap10.end())
	  == my_map(mmap03_ref.begin(), mmap03_ref.end()) );

  my_ummap ummap11(mmap04_ref.begin(), mmap04_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = ummap11.size();
  my_ummap ummap12(mmap01_ref.begin(), mmap01_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = ummap12.size();

  ummap11.swap(ummap12);
  VERIFY( ummap11.size() == size02 );
  VERIFY( my_map(ummap11.begin(), ummap11.end())
	  == my_map(mmap01_ref.begin(), mmap01_ref.end()) );
  VERIFY( ummap12.size() == size01 );
  VERIFY( my_map(ummap12.begin(), ummap12.end())
	  == my_map(mmap04_ref.begin(), mmap04_ref.end()) );

  my_ummap ummap13(mmap03_ref.begin(), mmap03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = ummap13.size();
  my_ummap ummap14(mmap03_ref.begin(), mmap03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = ummap14.size();

  ummap13.swap(ummap14);
  VERIFY( ummap13.size() == size02 );
  VERIFY( my_map(ummap13.begin(), ummap13.end())
	  == my_map(mmap03_ref.begin(), mmap03_ref.end()) );
  VERIFY( ummap14.size() == size01 );
  VERIFY( my_map(ummap14.begin(), ummap14.end())
	  == my_map(mmap03_ref.begin(), mmap03_ref.end()) );
}

int main()
{
  test01();
  return 0;
}
