// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

// 23.3.3 set::swap

#include <set>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator, two different personalities.
void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef __gnu_test::uneq_allocator<char> my_alloc;
  typedef set<char, less<char>, my_alloc> my_set;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  const set<char> set01_ref(title01, title01 + N1);
  const set<char> set02_ref(title02, title02 + N2);
  const set<char> set03_ref(title03, title03 + N3);
  const set<char> set04_ref(title04, title04 + N4);

  my_set::size_type size01, size02;

  my_alloc alloc01(1), alloc02(2);
  int personality01, personality02;

  my_set set01(less<char>(), alloc01);
  size01 = set01.size();
  personality01 = set01.get_allocator().get_personality();
  my_set set02(less<char>(), alloc02);
  size02 = set02.size();
  personality02 = set02.get_allocator().get_personality();

  set01.swap(set02);
  VERIFY( set01.size() == size02 );
  VERIFY( set01.empty() );
  VERIFY( set02.size() == size01 );
  VERIFY( set02.empty() );
  VERIFY( set01.get_allocator().get_personality() == personality02 );
  VERIFY( set02.get_allocator().get_personality() == personality01 );

  my_set set03(less<char>(), alloc02);
  size01 = set03.size();
  personality01 = set03.get_allocator().get_personality();
  my_set set04(title02, title02 + N2, less<char>(), alloc01);
  size02 = set04.size();
  personality02 = set04.get_allocator().get_personality();

  set03.swap(set04);
  VERIFY( set03.size() == size02 );
  VERIFY( equal(set03.begin(), set03.end(), set02_ref.begin()) );
  VERIFY( set04.size() == size01 );
  VERIFY( set04.empty() );
  VERIFY( set03.get_allocator().get_personality() == personality02 );
  VERIFY( set04.get_allocator().get_personality() == personality01 );
  
  my_set set05(title01, title01 + N1, less<char>(), alloc01);
  size01 = set05.size();
  personality01 = set05.get_allocator().get_personality();
  my_set set06(title02, title02 + N2, less<char>(), alloc02);
  size02 = set06.size();
  personality02 = set06.get_allocator().get_personality();

  set05.swap(set06);
  VERIFY( set05.size() == size02 );
  VERIFY( equal(set05.begin(), set05.end(), set02_ref.begin()) );
  VERIFY( set06.size() == size01 );
  VERIFY( equal(set06.begin(), set06.end(), set01_ref.begin()) );
  VERIFY( set05.get_allocator().get_personality() == personality02 );
  VERIFY( set06.get_allocator().get_personality() == personality01 );

  my_set set07(title01, title01 + N1, less<char>(), alloc02);
  size01 = set07.size();
  personality01 = set07.get_allocator().get_personality();
  my_set set08(title03, title03 + N3, less<char>(), alloc01);
  size02 = set08.size();
  personality02 = set08.get_allocator().get_personality();

  set07.swap(set08);
  VERIFY( set07.size() == size02 );
  VERIFY( equal(set07.begin(), set07.end(), set03_ref.begin()) );
  VERIFY( set08.size() == size01 );
  VERIFY( equal(set08.begin(), set08.end(), set01_ref.begin()) );
  VERIFY( set07.get_allocator().get_personality() == personality02 );
  VERIFY( set08.get_allocator().get_personality() == personality01 );

  my_set set09(title03, title03 + N3, less<char>(), alloc01);
  size01 = set09.size();
  personality01 = set09.get_allocator().get_personality();
  my_set set10(title04, title04 + N4, less<char>(), alloc02);
  size02 = set10.size();
  personality02 = set10.get_allocator().get_personality();

  set09.swap(set10);
  VERIFY( set09.size() == size02 );
  VERIFY( equal(set09.begin(), set09.end(), set04_ref.begin()) );
  VERIFY( set10.size() == size01 );
  VERIFY( equal(set10.begin(), set10.end(), set03_ref.begin()) );
  VERIFY( set09.get_allocator().get_personality() == personality02 );
  VERIFY( set10.get_allocator().get_personality() == personality01 );

  my_set set11(title04, title04 + N4, less<char>(), alloc02);
  size01 = set11.size();
  personality01 = set11.get_allocator().get_personality();
  my_set set12(title01, title01 + N1, less<char>(), alloc01);
  size02 = set12.size();
  personality02 = set12.get_allocator().get_personality();

  set11.swap(set12);
  VERIFY( set11.size() == size02 );
  VERIFY( equal(set11.begin(), set11.end(), set01_ref.begin()) );
  VERIFY( set12.size() == size01 );
  VERIFY( equal(set12.begin(), set12.end(), set04_ref.begin()) );
  VERIFY( set11.get_allocator().get_personality() == personality02 );
  VERIFY( set12.get_allocator().get_personality() == personality01 );

  my_set set13(title03, title03 + N3, less<char>(), alloc01);
  size01 = set13.size();
  personality01 = set13.get_allocator().get_personality();
  my_set set14(title03, title03 + N3, less<char>(), alloc02);
  size02 = set14.size();
  personality02 = set14.get_allocator().get_personality();

  set13.swap(set14);
  VERIFY( set13.size() == size02 );
  VERIFY( equal(set13.begin(), set13.end(), set03_ref.begin()) );
  VERIFY( set14.size() == size01 );
  VERIFY( equal(set14.begin(), set14.end(), set03_ref.begin()) );
  VERIFY( set13.get_allocator().get_personality() == personality02 );
  VERIFY( set14.get_allocator().get_personality() == personality01 );
}

int main()
{ 
  test01();
  return 0;
}
