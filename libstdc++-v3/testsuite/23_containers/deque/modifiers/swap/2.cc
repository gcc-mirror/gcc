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

// 23.2.1.3 deque::swap

#include <deque>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator as a non-empty allocator.
void
test01()
{
  using namespace std;

  typedef __gnu_test::uneq_allocator<char> my_alloc;
  typedef deque<char, my_alloc> my_deque;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  my_deque::size_type size01, size02;

  my_alloc alloc01(1);

  my_deque deq01(alloc01);
  size01 = deq01.size();
  my_deque deq02(alloc01);
  size02 = deq02.size();
  
  deq01.swap(deq02);
  VERIFY( deq01.size() == size02 );
  VERIFY( deq01.empty() );
  VERIFY( deq02.size() == size01 );
  VERIFY( deq02.empty() );

  my_deque deq03(alloc01);
  size01 = deq03.size();
  my_deque deq04(title02, title02 + N2, alloc01);
  size02 = deq04.size();
  
  deq03.swap(deq04);
  VERIFY( deq03.size() == size02 );
  VERIFY( equal(deq03.begin(), deq03.end(), title02) );
  VERIFY( deq04.size() == size01 );
  VERIFY( deq04.empty() );
  
  my_deque deq05(title01, title01 + N1, alloc01);
  size01 = deq05.size();
  my_deque deq06(title02, title02 + N2, alloc01);
  size02 = deq06.size();
  
  deq05.swap(deq06);
  VERIFY( deq05.size() == size02 );
  VERIFY( equal(deq05.begin(), deq05.end(), title02) );
  VERIFY( deq06.size() == size01 );
  VERIFY( equal(deq06.begin(), deq06.end(), title01) );

  my_deque deq07(title01, title01 + N1, alloc01);
  size01 = deq07.size();
  my_deque deq08(title03, title03 + N3, alloc01);
  size02 = deq08.size();

  deq07.swap(deq08);
  VERIFY( deq07.size() == size02 );
  VERIFY( equal(deq07.begin(), deq07.end(), title03) );
  VERIFY( deq08.size() == size01 );
  VERIFY( equal(deq08.begin(), deq08.end(), title01) );

  my_deque deq09(title03, title03 + N3, alloc01);
  size01 = deq09.size();
  my_deque deq10(title04, title04 + N4, alloc01);
  size02 = deq10.size();

  deq09.swap(deq10);
  VERIFY( deq09.size() == size02 );
  VERIFY( equal(deq09.begin(), deq09.end(), title04) );
  VERIFY( deq10.size() == size01 );
  VERIFY( equal(deq10.begin(), deq10.end(), title03) );

  my_deque deq11(title04, title04 + N4, alloc01);
  size01 = deq11.size();
  my_deque deq12(title01, title01 + N1, alloc01);
  size02 = deq12.size();

  deq11.swap(deq12);
  VERIFY( deq11.size() == size02 );
  VERIFY( equal(deq11.begin(), deq11.end(), title01) );
  VERIFY( deq12.size() == size01 );
  VERIFY( equal(deq12.begin(), deq12.end(), title04) );

  my_deque deq13(title03, title03 + N3, alloc01);
  size01 = deq13.size();
  my_deque deq14(title03, title03 + N3, alloc01);
  size02 = deq14.size();

  deq13.swap(deq14);
  VERIFY( deq13.size() == size02 );
  VERIFY( equal(deq13.begin(), deq13.end(), title03) );
  VERIFY( deq14.size() == size01 );
  VERIFY( equal(deq14.begin(), deq14.end(), title03) );
}

int main()
{ 
  test01();
  return 0;
}
