// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

// 23.3.1 map::swap

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
  typedef map<char, int, less<char>, my_alloc> my_map;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  map<char, int> map01_ref;
  for (size_t i = 0; i < N1; ++i)
    map01_ref.insert(my_pair(title01[i], i));
  map<char, int> map02_ref;
  for (size_t i = 0; i < N2; ++i)
    map02_ref.insert(my_pair(title02[i], i));
  map<char, int> map03_ref;
  for (size_t i = 0; i < N3; ++i)
    map03_ref.insert(my_pair(title03[i], i));
  map<char, int> map04_ref;
  for (size_t i = 0; i < N4; ++i)
    map04_ref.insert(my_pair(title04[i], i));

  my_map::size_type size01, size02;

  my_alloc alloc01(1);

  my_map map01(less<char>(), alloc01);
  size01 = map01.size();
  my_map map02(less<char>(), alloc01);
  size02 = map02.size();
  
  map01.swap(map02);
  VERIFY( map01.size() == size02 );
  VERIFY( map01.empty() );
  VERIFY( map02.size() == size01 );
  VERIFY( map02.empty() );

  my_map map03(less<char>(), alloc01);
  size01 = map03.size();
  my_map map04(map02_ref.begin(), map02_ref.end(), less<char>(), alloc01);
  size02 = map04.size();

  map03.swap(map04);
  VERIFY( map03.size() == size02 );
  VERIFY( equal(map03.begin(), map03.end(), map02_ref.begin()) );
  VERIFY( map04.size() == size01 );
  VERIFY( map04.empty() );
  
  my_map map05(map01_ref.begin(), map01_ref.end(), less<char>(), alloc01);
  size01 = map05.size();
  my_map map06(map02_ref.begin(), map02_ref.end(), less<char>(), alloc01);
  size02 = map06.size();

  map05.swap(map06);
  VERIFY( map05.size() == size02 );
  VERIFY( equal(map05.begin(), map05.end(), map02_ref.begin()) );
  VERIFY( map06.size() == size01 );
  VERIFY( equal(map06.begin(), map06.end(), map01_ref.begin()) );

  my_map map07(map01_ref.begin(), map01_ref.end(), less<char>(), alloc01);
  size01 = map07.size();
  my_map map08(map03_ref.begin(), map03_ref.end(), less<char>(), alloc01);
  size02 = map08.size();

  map07.swap(map08);
  VERIFY( map07.size() == size02 );
  VERIFY( equal(map07.begin(), map07.end(), map03_ref.begin()) );
  VERIFY( map08.size() == size01 );
  VERIFY( equal(map08.begin(), map08.end(), map01_ref.begin()) );

  my_map map09(map03_ref.begin(), map03_ref.end(), less<char>(), alloc01);
  size01 = map09.size();
  my_map map10(map04_ref.begin(), map04_ref.end(), less<char>(), alloc01);
  size02 = map10.size();

  map09.swap(map10);
  VERIFY( map09.size() == size02 );
  VERIFY( equal(map09.begin(), map09.end(), map04_ref.begin()) );
  VERIFY( map10.size() == size01 );
  VERIFY( equal(map10.begin(), map10.end(), map03_ref.begin()) );

  my_map map11(map04_ref.begin(), map04_ref.end(), less<char>(), alloc01);
  size01 = map11.size();
  my_map map12(map01_ref.begin(), map01_ref.end(), less<char>(), alloc01);
  size02 = map12.size();

  map11.swap(map12);
  VERIFY( map11.size() == size02 );
  VERIFY( equal(map11.begin(), map11.end(), map01_ref.begin()) );
  VERIFY( map12.size() == size01 );
  VERIFY( equal(map12.begin(), map12.end(), map04_ref.begin()) );

  my_map map13(map03_ref.begin(), map03_ref.end(), less<char>(), alloc01);
  size01 = map13.size();
  my_map map14(map03_ref.begin(), map03_ref.end(), less<char>(), alloc01);
  size02 = map14.size();

  map13.swap(map14);
  VERIFY( map13.size() == size02 );
  VERIFY( equal(map13.begin(), map13.end(), map03_ref.begin()) );
  VERIFY( map14.size() == size01 );
  VERIFY( equal(map14.begin(), map14.end(), map03_ref.begin()) );
}

int main()
{ 
  test01();
  return 0;
}
