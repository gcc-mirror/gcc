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

// 6.3.4.5 unordered_multiset::swap

#include <tr1/unordered_set>
#include <set>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// uneq_allocator as a non-empty allocator.
void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std::tr1;
  using std::equal_to;
  using std::multiset;

  typedef __gnu_test::uneq_allocator<char> my_alloc;
  typedef unordered_multiset<char, hash<char>, equal_to<char>, my_alloc>
    my_umset;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  typedef multiset<char> my_mset;
  const my_mset mset01_ref(title01, title01 + N1);
  const my_mset mset02_ref(title02, title02 + N2);
  const my_mset mset03_ref(title03, title03 + N3);
  const my_mset mset04_ref(title04, title04 + N4);

  my_umset::size_type size01, size02;

  my_alloc alloc01(1);

  my_umset umset01(10, hash<char>(), equal_to<char>(), alloc01);
  size01 = umset01.size();
  my_umset umset02(10, hash<char>(), equal_to<char>(), alloc01);
  size02 = umset02.size();
  
  umset01.swap(umset02);
  VERIFY( umset01.size() == size02 );
  VERIFY( umset01.empty() );
  VERIFY( umset02.size() == size01 );
  VERIFY( umset02.empty() );

  my_umset umset03(10, hash<char>(), equal_to<char>(), alloc01);
  size01 = umset03.size();
  my_umset umset04(mset02_ref.begin(), mset02_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = umset04.size();
  
  umset03.swap(umset04);
  VERIFY( umset03.size() == size02 );
  VERIFY( my_mset(umset03.begin(), umset03.end()) == mset02_ref );
  VERIFY( umset04.size() == size01 );
  VERIFY( umset04.empty() );
  
  my_umset umset05(mset01_ref.begin(), mset01_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = umset05.size();
  my_umset umset06(mset02_ref.begin(), mset02_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = umset06.size();
  
  umset05.swap(umset06);
  VERIFY( umset05.size() == size02 );
  VERIFY( my_mset(umset05.begin(), umset05.end()) == mset02_ref );
  VERIFY( umset06.size() == size01 );
  VERIFY( my_mset(umset06.begin(), umset06.end()) == mset01_ref );

  my_umset umset07(mset01_ref.begin(), mset01_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = umset07.size();
  my_umset umset08(mset03_ref.begin(), mset03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = umset08.size();

  umset07.swap(umset08);
  VERIFY( umset07.size() == size02 );
  VERIFY( my_mset(umset07.begin(), umset07.end()) == mset03_ref );
  VERIFY( umset08.size() == size01 );
  VERIFY( my_mset(umset08.begin(), umset08.end()) == mset01_ref );

  my_umset umset09(mset03_ref.begin(), mset03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = umset09.size();
  my_umset umset10(mset04_ref.begin(), mset04_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = umset10.size();

  umset09.swap(umset10);
  VERIFY( umset09.size() == size02 );
  VERIFY( my_mset(umset09.begin(), umset09.end()) == mset04_ref );
  VERIFY( umset10.size() == size01 );
  VERIFY( my_mset(umset10.begin(), umset10.end()) == mset03_ref );

  my_umset umset11(mset04_ref.begin(), mset04_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = umset11.size();
  my_umset umset12(mset01_ref.begin(), mset01_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = umset12.size();

  umset11.swap(umset12);
  VERIFY( umset11.size() == size02 );
  VERIFY( my_mset(umset11.begin(), umset11.end()) == mset01_ref );
  VERIFY( umset12.size() == size01 );
  VERIFY( my_mset(umset12.begin(), umset12.end()) == mset04_ref );

  my_umset umset13(mset03_ref.begin(), mset03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size01 = umset13.size();
  my_umset umset14(mset03_ref.begin(), mset03_ref.end(), 10, hash<char>(),
		   equal_to<char>(), alloc01);
  size02 = umset14.size();

  umset13.swap(umset14);
  VERIFY( umset13.size() == size02 );
  VERIFY( my_mset(umset13.begin(), umset13.end()) == mset03_ref );
  VERIFY( umset14.size() == size01 );
  VERIFY( my_mset(umset14.begin(), umset14.end()) == mset03_ref );
}

int main()
{ 
  test01();
  return 0;
}
