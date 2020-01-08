// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

// 23.2.4.3 vector::swap

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator, two different personalities.
void
test01()
{
  using namespace std;

  typedef __gnu_test::uneq_allocator<char> my_alloc;
  typedef vector<char, my_alloc> my_vector;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  my_vector::size_type size01, size02;

  my_alloc alloc01(1), alloc02(2);
  int personality01, personality02;

  my_vector vec01(alloc01);
  size01 = vec01.size();
  personality01 = vec01.get_allocator().get_personality();
  my_vector vec02(alloc02);
  size02 = vec02.size();
  personality02 = vec02.get_allocator().get_personality();

  vec01.swap(vec02);
  VERIFY( vec01.size() == size02 );
  VERIFY( vec01.empty() );
  VERIFY( vec02.size() == size01 );
  VERIFY( vec02.empty() );
  VERIFY( vec01.get_allocator().get_personality() == personality02 );
  VERIFY( vec02.get_allocator().get_personality() == personality01 );

  my_vector vec03(alloc02);
  size01 = vec03.size();
  personality01 = vec03.get_allocator().get_personality();
  my_vector vec04(title02, title02 + N2, alloc01);
  size02 = vec04.size();
  personality02 = vec04.get_allocator().get_personality();

  vec03.swap(vec04);
  VERIFY( vec03.size() == size02 );
  VERIFY( equal(vec03.begin(), vec03.end(), title02) );
  VERIFY( vec04.size() == size01 );
  VERIFY( vec04.empty() );
  VERIFY( vec03.get_allocator().get_personality() == personality02 );
  VERIFY( vec04.get_allocator().get_personality() == personality01 );
  
  my_vector vec05(title01, title01 + N1, alloc01);
  size01 = vec05.size();
  personality01 = vec05.get_allocator().get_personality();
  my_vector vec06(title02, title02 + N2, alloc02);
  size02 = vec06.size();
  personality02 = vec06.get_allocator().get_personality();

  vec05.swap(vec06);
  VERIFY( vec05.size() == size02 );
  VERIFY( equal(vec05.begin(), vec05.end(), title02) );
  VERIFY( vec06.size() == size01 );
  VERIFY( equal(vec06.begin(), vec06.end(), title01) );
  VERIFY( vec05.get_allocator().get_personality() == personality02 );
  VERIFY( vec06.get_allocator().get_personality() == personality01 );

  my_vector vec07(title01, title01 + N1, alloc02);
  size01 = vec07.size();
  personality01 = vec07.get_allocator().get_personality();
  my_vector vec08(title03, title03 + N3, alloc01);
  size02 = vec08.size();
  personality02 = vec08.get_allocator().get_personality();

  vec07.swap(vec08);
  VERIFY( vec07.size() == size02 );
  VERIFY( equal(vec07.begin(), vec07.end(), title03) );
  VERIFY( vec08.size() == size01 );
  VERIFY( equal(vec08.begin(), vec08.end(), title01) );
  VERIFY( vec07.get_allocator().get_personality() == personality02 );
  VERIFY( vec08.get_allocator().get_personality() == personality01 );

  my_vector vec09(title03, title03 + N3, alloc01);
  size01 = vec09.size();
  personality01 = vec09.get_allocator().get_personality();
  my_vector vec10(title04, title04 + N4, alloc02);
  size02 = vec10.size();
  personality02 = vec10.get_allocator().get_personality();

  vec09.swap(vec10);
  VERIFY( vec09.size() == size02 );
  VERIFY( equal(vec09.begin(), vec09.end(), title04) );
  VERIFY( vec10.size() == size01 );
  VERIFY( equal(vec10.begin(), vec10.end(), title03) );
  VERIFY( vec09.get_allocator().get_personality() == personality02 );
  VERIFY( vec10.get_allocator().get_personality() == personality01 );

  my_vector vec11(title04, title04 + N4, alloc02);
  size01 = vec11.size();
  personality01 = vec11.get_allocator().get_personality();
  my_vector vec12(title01, title01 + N1, alloc01);
  size02 = vec12.size();
  personality02 = vec12.get_allocator().get_personality();

  vec11.swap(vec12);
  VERIFY( vec11.size() == size02 );
  VERIFY( equal(vec11.begin(), vec11.end(), title01) );
  VERIFY( vec12.size() == size01 );
  VERIFY( equal(vec12.begin(), vec12.end(), title04) );
  VERIFY( vec11.get_allocator().get_personality() == personality02 );
  VERIFY( vec12.get_allocator().get_personality() == personality01 );

  my_vector vec13(title03, title03 + N3, alloc01);
  size01 = vec13.size();
  personality01 = vec13.get_allocator().get_personality();
  my_vector vec14(title03, title03 + N3, alloc02);
  size02 = vec14.size();
  personality02 = vec14.get_allocator().get_personality();

  vec13.swap(vec14);
  VERIFY( vec13.size() == size02 );
  VERIFY( equal(vec13.begin(), vec13.end(), title03) );
  VERIFY( vec14.size() == size01 );
  VERIFY( equal(vec14.begin(), vec14.end(), title03) );
  VERIFY( vec13.get_allocator().get_personality() == personality02 );
  VERIFY( vec14.get_allocator().get_personality() == personality01 );
}

int main()
{ 
  test01();
  return 0;
}
