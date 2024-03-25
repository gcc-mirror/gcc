// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

// 23.3.2 multimap::swap

#include <map>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator as a non-empty allocator.
void
test01()
{
  using namespace std;

  typedef pair<const char, int> my_pair;
  typedef __gnu_test::uneq_allocator<my_pair> my_alloc;
  typedef multimap<char, int, less<char>, my_alloc> my_mmap;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  multimap<char, int> mmap01_ref;
  for (size_t i = 0; i < N1; ++i)
    mmap01_ref.insert(my_pair(title01[i], i));
  multimap<char, int> mmap02_ref;
  for (size_t i = 0; i < N2; ++i)
    mmap02_ref.insert(my_pair(title02[i], i));
  multimap<char, int> mmap03_ref;
  for (size_t i = 0; i < N3; ++i)
    mmap03_ref.insert(my_pair(title03[i], i));
  multimap<char, int> mmap04_ref;
  for (size_t i = 0; i < N4; ++i)
    mmap04_ref.insert(my_pair(title04[i], i));

  my_mmap::size_type size01, size02;

  my_alloc alloc01(1);

  my_mmap mmap01(less<char>(), alloc01);
  size01 = mmap01.size();
  my_mmap mmap02(less<char>(), alloc01);
  size02 = mmap02.size();
  
  mmap01.swap(mmap02);
  VERIFY( mmap01.size() == size02 );
  VERIFY( mmap01.empty() );
  VERIFY( mmap02.size() == size01 );
  VERIFY( mmap02.empty() );

  my_mmap mmap03(less<char>(), alloc01);
  size01 = mmap03.size();
  my_mmap mmap04(mmap02_ref.begin(), mmap02_ref.end(), less<char>(), alloc01);
  size02 = mmap04.size();

  mmap03.swap(mmap04);
  VERIFY( mmap03.size() == size02 );
  VERIFY( equal(mmap03.begin(), mmap03.end(), mmap02_ref.begin()) );
  VERIFY( mmap04.size() == size01 );
  VERIFY( mmap04.empty() );
  
  my_mmap mmap05(mmap01_ref.begin(), mmap01_ref.end(), less<char>(), alloc01);
  size01 = mmap05.size();
  my_mmap mmap06(mmap02_ref.begin(), mmap02_ref.end(), less<char>(), alloc01);
  size02 = mmap06.size();

  mmap05.swap(mmap06);
  VERIFY( mmap05.size() == size02 );
  VERIFY( equal(mmap05.begin(), mmap05.end(), mmap02_ref.begin()) );
  VERIFY( mmap06.size() == size01 );
  VERIFY( equal(mmap06.begin(), mmap06.end(), mmap01_ref.begin()) );

  my_mmap mmap07(mmap01_ref.begin(), mmap01_ref.end(), less<char>(), alloc01);
  size01 = mmap07.size();
  my_mmap mmap08(mmap03_ref.begin(), mmap03_ref.end(), less<char>(), alloc01);
  size02 = mmap08.size();

  mmap07.swap(mmap08);
  VERIFY( mmap07.size() == size02 );
  VERIFY( equal(mmap07.begin(), mmap07.end(), mmap03_ref.begin()) );
  VERIFY( mmap08.size() == size01 );
  VERIFY( equal(mmap08.begin(), mmap08.end(), mmap01_ref.begin()) );

  my_mmap mmap09(mmap03_ref.begin(), mmap03_ref.end(), less<char>(), alloc01);
  size01 = mmap09.size();
  my_mmap mmap10(mmap04_ref.begin(), mmap04_ref.end(), less<char>(), alloc01);
  size02 = mmap10.size();

  mmap09.swap(mmap10);
  VERIFY( mmap09.size() == size02 );
  VERIFY( equal(mmap09.begin(), mmap09.end(), mmap04_ref.begin()) );
  VERIFY( mmap10.size() == size01 );
  VERIFY( equal(mmap10.begin(), mmap10.end(), mmap03_ref.begin()) );

  my_mmap mmap11(mmap04_ref.begin(), mmap04_ref.end(), less<char>(), alloc01);
  size01 = mmap11.size();
  my_mmap mmap12(mmap01_ref.begin(), mmap01_ref.end(), less<char>(), alloc01);
  size02 = mmap12.size();

  mmap11.swap(mmap12);
  VERIFY( mmap11.size() == size02 );
  VERIFY( equal(mmap11.begin(), mmap11.end(), mmap01_ref.begin()) );
  VERIFY( mmap12.size() == size01 );
  VERIFY( equal(mmap12.begin(), mmap12.end(), mmap04_ref.begin()) );

  my_mmap mmap13(mmap03_ref.begin(), mmap03_ref.end(), less<char>(), alloc01);
  size01 = mmap13.size();
  my_mmap mmap14(mmap03_ref.begin(), mmap03_ref.end(), less<char>(), alloc01);
  size02 = mmap14.size();

  mmap13.swap(mmap14);
  VERIFY( mmap13.size() == size02 );
  VERIFY( equal(mmap13.begin(), mmap13.end(), mmap03_ref.begin()) );
  VERIFY( mmap14.size() == size01 );
  VERIFY( equal(mmap14.begin(), mmap14.end(), mmap03_ref.begin()) );
}

int main()
{ 
  test01();
  return 0;
}
