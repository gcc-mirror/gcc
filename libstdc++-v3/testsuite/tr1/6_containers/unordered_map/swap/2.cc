// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 6.3.4.4 unordered_map::swap

#include <tr1/unordered_map>
#include <map>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator, two different personalities.
void
test01()
{
  using namespace std::tr1;
  using std::pair;
  using std::equal_to;
  using std::map;

  typedef pair<const char, int> my_pair;
  typedef __gnu_test::uneq_allocator<my_pair> my_alloc;
  typedef unordered_map<char, int, hash<char>, equal_to<char>, my_alloc>
    my_umap;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  typedef map<char, int> my_map;
  my_map map01_ref;
  for (size_t i = 0; i < N1; ++i)
    map01_ref.insert(my_pair(title01[i], i));
  my_map map02_ref;
  for (size_t i = 0; i < N2; ++i)
    map02_ref.insert(my_pair(title02[i], i));
  my_map map03_ref;
  for (size_t i = 0; i < N3; ++i)
    map03_ref.insert(my_pair(title03[i], i));
  my_map map04_ref;
  for (size_t i = 0; i < N4; ++i)
    map04_ref.insert(my_pair(title04[i], i));

  my_umap::size_type size01, size02;

  my_alloc alloc01(1), alloc02(2);
  int personality01, personality02;

  my_umap umap01(10, hash<char>(), equal_to<char>(), alloc01);
  size01 = umap01.size();
  personality01 = umap01.get_allocator().get_personality();
  my_umap umap02(10, hash<char>(), equal_to<char>(), alloc02);
  size02 = umap02.size();
  personality02 = umap02.get_allocator().get_personality();

  umap01.swap(umap02);
  VERIFY( umap01.size() == size02 );
  VERIFY( umap01.empty() );
  VERIFY( umap02.size() == size01 );
  VERIFY( umap02.empty() );
  VERIFY( umap01.get_allocator().get_personality() == personality02 );
  VERIFY( umap02.get_allocator().get_personality() == personality01 );

  my_umap umap03(10, hash<char>(), equal_to<char>(), alloc02);
  size01 = umap03.size();
  personality01 = umap03.get_allocator().get_personality();
  my_umap umap04(map02_ref.begin(), map02_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size02 = umap04.size();
  personality02 = umap04.get_allocator().get_personality();

  umap03.swap(umap04);
  VERIFY( umap03.size() == size02 );
  VERIFY( my_map(umap03.begin(), umap03.end()) == map02_ref );
  VERIFY( umap04.size() == size01 );
  VERIFY( umap04.empty() );
  VERIFY( umap03.get_allocator().get_personality() == personality02 );
  VERIFY( umap04.get_allocator().get_personality() == personality01 );

  my_umap umap05(map01_ref.begin(), map01_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size01 = umap05.size();
  personality01 = umap05.get_allocator().get_personality();
  my_umap umap06(map02_ref.begin(), map02_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size02 = umap06.size();
  personality02 = umap06.get_allocator().get_personality();

  umap05.swap(umap06);
  VERIFY( umap05.size() == size02 );
  VERIFY( my_map(umap05.begin(), umap05.end()) == map02_ref );
  VERIFY( umap06.size() == size01 );
  VERIFY( my_map(umap06.begin(), umap06.end()) == map01_ref );
  VERIFY( umap05.get_allocator().get_personality() == personality02 );
  VERIFY( umap06.get_allocator().get_personality() == personality01 );

  my_umap umap07(map01_ref.begin(), map01_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size01 = umap07.size();
  personality01 = umap07.get_allocator().get_personality();
  my_umap umap08(map03_ref.begin(), map03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size02 = umap08.size();
  personality02 = umap08.get_allocator().get_personality();

  umap07.swap(umap08);
  VERIFY( umap07.size() == size02 );
  VERIFY( my_map(umap07.begin(), umap07.end()) == map03_ref );
  VERIFY( umap08.size() == size01 );
  VERIFY( my_map(umap08.begin(), umap08.end()) == map01_ref );
  VERIFY( umap07.get_allocator().get_personality() == personality02 );
  VERIFY( umap08.get_allocator().get_personality() == personality01 );

  my_umap umap09(map03_ref.begin(), map03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size01 = umap09.size();
  personality01 = umap09.get_allocator().get_personality();
  my_umap umap10(map04_ref.begin(), map04_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size02 = umap10.size();
  personality02 = umap10.get_allocator().get_personality();

  umap09.swap(umap10);
  VERIFY( umap09.size() == size02 );
  VERIFY( my_map(umap09.begin(), umap09.end()) == map04_ref );
  VERIFY( umap10.size() == size01 );
  VERIFY( my_map(umap10.begin(), umap10.end()) == map03_ref );
  VERIFY( umap09.get_allocator().get_personality() == personality02 );
  VERIFY( umap10.get_allocator().get_personality() == personality01 );

  my_umap umap11(map04_ref.begin(), map04_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size01 = umap11.size();
  personality01 = umap11.get_allocator().get_personality();
  my_umap umap12(map01_ref.begin(), map01_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size02 = umap12.size();
  personality02 = umap12.get_allocator().get_personality();

  umap11.swap(umap12);
  VERIFY( umap11.size() == size02 );
  VERIFY( my_map(umap11.begin(), umap11.end()) == map01_ref );
  VERIFY( umap12.size() == size01 );
  VERIFY( my_map(umap12.begin(), umap12.end()) == map04_ref );
  VERIFY( umap11.get_allocator().get_personality() == personality02 );
  VERIFY( umap12.get_allocator().get_personality() == personality01 );

  my_umap umap13(map03_ref.begin(), map03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size01 = umap13.size();
  personality01 = umap13.get_allocator().get_personality();
  my_umap umap14(map03_ref.begin(), map03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size02 = umap14.size();
  personality02 = umap14.get_allocator().get_personality();

  umap13.swap(umap14);
  VERIFY( umap13.size() == size02 );
  VERIFY( my_map(umap13.begin(), umap13.end()) == map03_ref );
  VERIFY( umap14.size() == size01 );
  VERIFY( my_map(umap14.begin(), umap14.end()) == map03_ref );
  VERIFY( umap13.get_allocator().get_personality() == personality02 );
  VERIFY( umap14.get_allocator().get_personality() == personality01 );
}

int main()
{
  test01();
  return 0;
}
