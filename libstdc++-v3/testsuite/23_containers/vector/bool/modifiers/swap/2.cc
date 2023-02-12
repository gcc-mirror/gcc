// 2005-12-23  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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

// 23.2.5 vector<bool>::swap

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator, two different personalities.
void
test01()
{
  using namespace std;

  typedef __gnu_test::uneq_allocator<bool> my_alloc;
  typedef vector<bool, my_alloc> my_vector;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  vector<bool> vec01_ref;
  for (size_t i = 0; i < N1; ++i)
    vec01_ref.push_back(bool(title01[i] > 96 ? 1 : 0));
  vector<bool> vec02_ref;
  for (size_t i = 0; i < N2; ++i)
    vec02_ref.push_back(bool(title02[i] > 96 ? 1 : 0));
  vector<bool> vec03_ref;
  for (size_t i = 0; i < N3; ++i)
    vec03_ref.push_back(bool(title03[i] > 96 ? 1 : 0));
  vector<bool> vec04_ref;
  for (size_t i = 0; i < N4; ++i)
    vec04_ref.push_back(bool(title04[i] > 96 ? 1 : 0));

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
  my_vector vec04(vec02_ref.begin(), vec02_ref.end(), alloc01);
  size02 = vec04.size();
  personality02 = vec04.get_allocator().get_personality();

  vec03.swap(vec04);
  VERIFY( vec03.size() == size02 );
  VERIFY( equal(vec03.begin(), vec03.end(), vec02_ref.begin()) );
  VERIFY( vec04.size() == size01 );
  VERIFY( vec04.empty() );
  VERIFY( vec03.get_allocator().get_personality() == personality02 );
  VERIFY( vec04.get_allocator().get_personality() == personality01 );
  
  my_vector vec05(vec01_ref.begin(), vec01_ref.end(), alloc01);
  size01 = vec05.size();
  personality01 = vec05.get_allocator().get_personality();
  my_vector vec06(vec02_ref.begin(), vec02_ref.end(), alloc02);
  size02 = vec06.size();
  personality02 = vec06.get_allocator().get_personality();

  vec05.swap(vec06);
  VERIFY( vec05.size() == size02 );
  VERIFY( equal(vec05.begin(), vec05.end(), vec02_ref.begin()) );
  VERIFY( vec06.size() == size01 );
  VERIFY( equal(vec06.begin(), vec06.end(), vec01_ref.begin()) );
  VERIFY( vec05.get_allocator().get_personality() == personality02 );
  VERIFY( vec06.get_allocator().get_personality() == personality01 );

  my_vector vec07(vec01_ref.begin(), vec01_ref.end(), alloc02);
  size01 = vec07.size();
  personality01 = vec07.get_allocator().get_personality();
  my_vector vec08(vec03_ref.begin(), vec03_ref.end(), alloc01);
  size02 = vec08.size();
  personality02 = vec08.get_allocator().get_personality();

  vec07.swap(vec08);
  VERIFY( vec07.size() == size02 );
  VERIFY( equal(vec07.begin(), vec07.end(), vec03_ref.begin()) );
  VERIFY( vec08.size() == size01 );
  VERIFY( equal(vec08.begin(), vec08.end(), vec01_ref.begin()) );
  VERIFY( vec07.get_allocator().get_personality() == personality02 );
  VERIFY( vec08.get_allocator().get_personality() == personality01 );

  my_vector vec09(vec03_ref.begin(), vec03_ref.end(), alloc01);
  size01 = vec09.size();
  personality01 = vec09.get_allocator().get_personality();
  my_vector vec10(vec04_ref.begin(), vec04_ref.end(), alloc02);
  size02 = vec10.size();
  personality02 = vec10.get_allocator().get_personality();

  vec09.swap(vec10);
  VERIFY( vec09.size() == size02 );
  VERIFY( equal(vec09.begin(), vec09.end(), vec04_ref.begin()) );
  VERIFY( vec10.size() == size01 );
  VERIFY( equal(vec10.begin(), vec10.end(), vec03_ref.begin()) );
  VERIFY( vec09.get_allocator().get_personality() == personality02 );
  VERIFY( vec10.get_allocator().get_personality() == personality01 );

  my_vector vec11(vec04_ref.begin(), vec04_ref.end(), alloc02);
  size01 = vec11.size();
  personality01 = vec11.get_allocator().get_personality();
  my_vector vec12(vec01_ref.begin(), vec01_ref.end(), alloc01);
  size02 = vec12.size();
  personality02 = vec12.get_allocator().get_personality();

  vec11.swap(vec12);
  VERIFY( vec11.size() == size02 );
  VERIFY( equal(vec11.begin(), vec11.end(), vec01_ref.begin()) );
  VERIFY( vec12.size() == size01 );
  VERIFY( equal(vec12.begin(), vec12.end(), vec04_ref.begin()) );
  VERIFY( vec11.get_allocator().get_personality() == personality02 );
  VERIFY( vec12.get_allocator().get_personality() == personality01 );

  my_vector vec13(vec03_ref.begin(), vec03_ref.end(), alloc01);
  size01 = vec13.size();
  personality01 = vec13.get_allocator().get_personality();
  my_vector vec14(vec03_ref.begin(), vec03_ref.end(), alloc02);
  size02 = vec14.size();
  personality02 = vec14.get_allocator().get_personality();

  vec13.swap(vec14);
  VERIFY( vec13.size() == size02 );
  VERIFY( equal(vec13.begin(), vec13.end(), vec03_ref.begin()) );
  VERIFY( vec14.size() == size01 );
  VERIFY( equal(vec14.begin(), vec14.end(), vec03_ref.begin()) );
  VERIFY( vec13.get_allocator().get_personality() == personality02 );
  VERIFY( vec14.get_allocator().get_personality() == personality01 );
}

int main()
{ 
  test01();
  return 0;
}
