// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

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

// 23.2.2.3 list::swap

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator as a non-empty allocator.
template<typename _Tp>
void
swap2()
{
  using namespace std;

  typedef _Tp list_type;
  typedef typename list_type::allocator_type allocator_type;
  typedef typename list_type::size_type size_type;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  size_type size01, size02;

  allocator_type alloc01(1);

  list_type lis01(alloc01);
  size01 = lis01.size();
  list_type lis02(alloc01);
  size02 = lis02.size();
  
  lis01.swap(lis02);
  VERIFY( lis01.size() == size02 );
  VERIFY( lis01.empty() );
  VERIFY( lis02.size() == size01 );
  VERIFY( lis02.empty() );

  list_type lis03(alloc01);
  size01 = lis03.size();
  list_type lis04(title02, title02 + N2, alloc01);
  size02 = lis04.size();
  
  lis03.swap(lis04);
  VERIFY( lis03.size() == size02 );
  VERIFY( equal(lis03.begin(), lis03.end(), title02) );
  VERIFY( lis04.size() == size01 );
  VERIFY( lis04.empty() );
  
  list_type lis05(title01, title01 + N1, alloc01);
  size01 = lis05.size();
  list_type lis06(title02, title02 + N2, alloc01);
  size02 = lis06.size();
  
  lis05.swap(lis06);
  VERIFY( lis05.size() == size02 );
  VERIFY( equal(lis05.begin(), lis05.end(), title02) );
  VERIFY( lis06.size() == size01 );
  VERIFY( equal(lis06.begin(), lis06.end(), title01) );

  list_type lis07(title01, title01 + N1, alloc01);
  size01 = lis07.size();
  list_type lis08(title03, title03 + N3, alloc01);
  size02 = lis08.size();

  lis07.swap(lis08);
  VERIFY( lis07.size() == size02 );
  VERIFY( equal(lis07.begin(), lis07.end(), title03) );
  VERIFY( lis08.size() == size01 );
  VERIFY( equal(lis08.begin(), lis08.end(), title01) );

  list_type lis09(title03, title03 + N3, alloc01);
  size01 = lis09.size();
  list_type lis10(title04, title04 + N4, alloc01);
  size02 = lis10.size();

  lis09.swap(lis10);
  VERIFY( lis09.size() == size02 );
  VERIFY( equal(lis09.begin(), lis09.end(), title04) );
  VERIFY( lis10.size() == size01 );
  VERIFY( equal(lis10.begin(), lis10.end(), title03) );

  list_type lis11(title04, title04 + N4, alloc01);
  size01 = lis11.size();
  list_type lis12(title01, title01 + N1, alloc01);
  size02 = lis12.size();

  lis11.swap(lis12);
  VERIFY( lis11.size() == size02 );
  VERIFY( equal(lis11.begin(), lis11.end(), title01) );
  VERIFY( lis12.size() == size01 );
  VERIFY( equal(lis12.begin(), lis12.end(), title04) );

  list_type lis13(title03, title03 + N3, alloc01);
  size01 = lis13.size();
  list_type lis14(title03, title03 + N3, alloc01);
  size02 = lis14.size();

  lis13.swap(lis14);
  VERIFY( lis13.size() == size02 );
  VERIFY( equal(lis13.begin(), lis13.end(), title03) );
  VERIFY( lis14.size() == size01 );
  VERIFY( equal(lis14.begin(), lis14.end(), title03) );
}
