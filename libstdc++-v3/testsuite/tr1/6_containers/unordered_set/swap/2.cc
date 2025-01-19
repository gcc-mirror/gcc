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

// 6.3.4.3 unordered_set::swap

#include <tr1/unordered_set>
#include <set>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator, two different personalities.
void
test01()
{
  using namespace std::tr1;
  using std::equal_to;
  using std::set;

  typedef __gnu_test::uneq_allocator<char> my_alloc;
  typedef unordered_set<char, hash<char>, equal_to<char>, my_alloc> my_uset;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  typedef set<char> my_set;
  const my_set set01_ref(title01, title01 + N1);
  const my_set set02_ref(title02, title02 + N2);
  const my_set set03_ref(title03, title03 + N3);
  const my_set set04_ref(title04, title04 + N4);

  my_uset::size_type size01, size02;

  my_alloc alloc01(1), alloc02(2);
  int personality01, personality02;

  my_uset uset01(10, hash<char>(), equal_to<char>(), alloc01);
  size01 = uset01.size();
  personality01 = uset01.get_allocator().get_personality();
  my_uset uset02(10, hash<char>(), equal_to<char>(), alloc02);
  size02 = uset02.size();
  personality02 = uset02.get_allocator().get_personality();

  uset01.swap(uset02);
  VERIFY( uset01.size() == size02 );
  VERIFY( uset01.empty() );
  VERIFY( uset02.size() == size01 );
  VERIFY( uset02.empty() );
  VERIFY( uset01.get_allocator().get_personality() == personality02 );
  VERIFY( uset02.get_allocator().get_personality() == personality01 );

  my_uset uset03(10, hash<char>(), equal_to<char>(), alloc02);
  size01 = uset03.size();
  personality01 = uset03.get_allocator().get_personality();
  my_uset uset04(set02_ref.begin(), set02_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size02 = uset04.size();
  personality02 = uset04.get_allocator().get_personality();

  uset03.swap(uset04);
  VERIFY( uset03.size() == size02 );
  VERIFY( my_set(uset03.begin(), uset03.end()) == set02_ref );
  VERIFY( uset04.size() == size01 );
  VERIFY( uset04.empty() );
  VERIFY( uset03.get_allocator().get_personality() == personality02 );
  VERIFY( uset04.get_allocator().get_personality() == personality01 );

  my_uset uset05(set01_ref.begin(), set01_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size01 = uset05.size();
  personality01 = uset05.get_allocator().get_personality();
  my_uset uset06(set02_ref.begin(), set02_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size02 = uset06.size();
  personality02 = uset06.get_allocator().get_personality();

  uset05.swap(uset06);
  VERIFY( uset05.size() == size02 );
  VERIFY( my_set(uset05.begin(), uset05.end()) == set02_ref );
  VERIFY( uset06.size() == size01 );
  VERIFY( my_set(uset06.begin(), uset06.end()) == set01_ref );
  VERIFY( uset05.get_allocator().get_personality() == personality02 );
  VERIFY( uset06.get_allocator().get_personality() == personality01 );

  my_uset uset07(set01_ref.begin(), set01_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size01 = uset07.size();
  personality01 = uset07.get_allocator().get_personality();
  my_uset uset08(set03_ref.begin(), set03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size02 = uset08.size();
  personality02 = uset08.get_allocator().get_personality();

  uset07.swap(uset08);
  VERIFY( uset07.size() == size02 );
  VERIFY( my_set(uset07.begin(), uset07.end()) == set03_ref );
  VERIFY( uset08.size() == size01 );
  VERIFY( my_set(uset08.begin(), uset08.end()) == set01_ref );
  VERIFY( uset07.get_allocator().get_personality() == personality02 );
  VERIFY( uset08.get_allocator().get_personality() == personality01 );

  my_uset uset09(set03_ref.begin(), set03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size01 = uset09.size();
  personality01 = uset09.get_allocator().get_personality();
  my_uset uset10(set04_ref.begin(), set04_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size02 = uset10.size();
  personality02 = uset10.get_allocator().get_personality();

  uset09.swap(uset10);
  VERIFY( uset09.size() == size02 );
  VERIFY( my_set(uset09.begin(), uset09.end()) == set04_ref );
  VERIFY( uset10.size() == size01 );
  VERIFY( my_set(uset10.begin(), uset10.end()) == set03_ref );
  VERIFY( uset09.get_allocator().get_personality() == personality02 );
  VERIFY( uset10.get_allocator().get_personality() == personality01 );

  my_uset uset11(set04_ref.begin(), set04_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size01 = uset11.size();
  personality01 = uset11.get_allocator().get_personality();
  my_uset uset12(set01_ref.begin(), set01_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size02 = uset12.size();
  personality02 = uset12.get_allocator().get_personality();

  uset11.swap(uset12);
  VERIFY( uset11.size() == size02 );
  VERIFY( my_set(uset11.begin(), uset11.end()) == set01_ref );
  VERIFY( uset12.size() == size01 );
  VERIFY( my_set(uset12.begin(), uset12.end()) == set04_ref );
  VERIFY( uset11.get_allocator().get_personality() == personality02 );
  VERIFY( uset12.get_allocator().get_personality() == personality01 );

  my_uset uset13(set03_ref.begin(), set03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc01);
  size01 = uset13.size();
  personality01 = uset13.get_allocator().get_personality();
  my_uset uset14(set03_ref.begin(), set03_ref.end(), 10, hash<char>(),
		 equal_to<char>(), alloc02);
  size02 = uset14.size();
  personality02 = uset14.get_allocator().get_personality();

  uset13.swap(uset14);
  VERIFY( uset13.size() == size02 );
  VERIFY( my_set(uset13.begin(), uset13.end()) == set03_ref );
  VERIFY( uset14.size() == size01 );
  VERIFY( my_set(uset14.begin(), uset14.end()) == set03_ref );
  VERIFY( uset13.get_allocator().get_personality() == personality02 );
  VERIFY( uset14.get_allocator().get_personality() == personality01 );
}

int main()
{
  test01();
  return 0;
}
