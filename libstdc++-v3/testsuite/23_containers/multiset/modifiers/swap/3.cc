// 2005-12-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 23.3.4 multiset::swap

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
  typedef multiset<char, less<char>, my_alloc> my_multiset;

  const char title01[] = "Rivers of sand";
  const char title02[] = "Concret PH";
  const char title03[] = "Sonatas and Interludes for Prepared Piano";
  const char title04[] = "never as tired as when i'm waking up";

  const size_t N1 = sizeof(title01);
  const size_t N2 = sizeof(title02);
  const size_t N3 = sizeof(title03);
  const size_t N4 = sizeof(title04);

  const multiset<char> mset01_ref(title01, title01 + N1);
  const multiset<char> mset02_ref(title02, title02 + N2);
  const multiset<char> mset03_ref(title03, title03 + N3);
  const multiset<char> mset04_ref(title04, title04 + N4);

  my_multiset::size_type size01, size02;

  my_alloc alloc01(1), alloc02(2);
  int personality01, personality02;

  my_multiset mset01(less<char>(), alloc01);
  size01 = mset01.size();
  personality01 = mset01.get_allocator().get_personality();
  my_multiset mset02(less<char>(), alloc02);
  size02 = mset02.size();
  personality02 = mset02.get_allocator().get_personality();

  mset01.swap(mset02);
  VERIFY( mset01.size() == size02 );
  VERIFY( mset01.empty() );
  VERIFY( mset02.size() == size01 );
  VERIFY( mset02.empty() );
  VERIFY( mset01.get_allocator().get_personality() == personality02 );
  VERIFY( mset02.get_allocator().get_personality() == personality01 );

  my_multiset mset03(less<char>(), alloc02);
  size01 = mset03.size();
  personality01 = mset03.get_allocator().get_personality();
  my_multiset mset04(title02, title02 + N2, less<char>(), alloc01);
  size02 = mset04.size();
  personality02 = mset04.get_allocator().get_personality();

  mset03.swap(mset04);
  VERIFY( mset03.size() == size02 );
  VERIFY( equal(mset03.begin(), mset03.end(), mset02_ref.begin()) );
  VERIFY( mset04.size() == size01 );
  VERIFY( mset04.empty() );
  VERIFY( mset03.get_allocator().get_personality() == personality02 );
  VERIFY( mset04.get_allocator().get_personality() == personality01 );
  
  my_multiset mset05(title01, title01 + N1, less<char>(), alloc01);
  size01 = mset05.size();
  personality01 = mset05.get_allocator().get_personality();
  my_multiset mset06(title02, title02 + N2, less<char>(), alloc02);
  size02 = mset06.size();
  personality02 = mset06.get_allocator().get_personality();

  mset05.swap(mset06);
  VERIFY( mset05.size() == size02 );
  VERIFY( equal(mset05.begin(), mset05.end(), mset02_ref.begin()) );
  VERIFY( mset06.size() == size01 );
  VERIFY( equal(mset06.begin(), mset06.end(), mset01_ref.begin()) );
  VERIFY( mset05.get_allocator().get_personality() == personality02 );
  VERIFY( mset06.get_allocator().get_personality() == personality01 );

  my_multiset mset07(title01, title01 + N1, less<char>(), alloc02);
  size01 = mset07.size();
  personality01 = mset07.get_allocator().get_personality();
  my_multiset mset08(title03, title03 + N3, less<char>(), alloc01);
  size02 = mset08.size();
  personality02 = mset08.get_allocator().get_personality();

  mset07.swap(mset08);
  VERIFY( mset07.size() == size02 );
  VERIFY( equal(mset07.begin(), mset07.end(), mset03_ref.begin()) );
  VERIFY( mset08.size() == size01 );
  VERIFY( equal(mset08.begin(), mset08.end(), mset01_ref.begin()) );
  VERIFY( mset07.get_allocator().get_personality() == personality02 );
  VERIFY( mset08.get_allocator().get_personality() == personality01 );

  my_multiset mset09(title03, title03 + N3, less<char>(), alloc01);
  size01 = mset09.size();
  personality01 = mset09.get_allocator().get_personality();
  my_multiset mset10(title04, title04 + N4, less<char>(), alloc02);
  size02 = mset10.size();
  personality02 = mset10.get_allocator().get_personality();

  mset09.swap(mset10);
  VERIFY( mset09.size() == size02 );
  VERIFY( equal(mset09.begin(), mset09.end(), mset04_ref.begin()) );
  VERIFY( mset10.size() == size01 );
  VERIFY( equal(mset10.begin(), mset10.end(), mset03_ref.begin()) );
  VERIFY( mset09.get_allocator().get_personality() == personality02 );
  VERIFY( mset10.get_allocator().get_personality() == personality01 );

  my_multiset mset11(title04, title04 + N4, less<char>(), alloc02);
  size01 = mset11.size();
  personality01 = mset11.get_allocator().get_personality();
  my_multiset mset12(title01, title01 + N1, less<char>(), alloc01);
  size02 = mset12.size();
  personality02 = mset12.get_allocator().get_personality();

  mset11.swap(mset12);
  VERIFY( mset11.size() == size02 );
  VERIFY( equal(mset11.begin(), mset11.end(), mset01_ref.begin()) );
  VERIFY( mset12.size() == size01 );
  VERIFY( equal(mset12.begin(), mset12.end(), mset04_ref.begin()) );
  VERIFY( mset11.get_allocator().get_personality() == personality02 );
  VERIFY( mset12.get_allocator().get_personality() == personality01 );

  my_multiset mset13(title03, title03 + N3, less<char>(), alloc01);
  size01 = mset13.size();
  personality01 = mset13.get_allocator().get_personality();
  my_multiset mset14(title03, title03 + N3, less<char>(), alloc02);
  size02 = mset14.size();
  personality02 = mset14.get_allocator().get_personality();

  mset13.swap(mset14);
  VERIFY( mset13.size() == size02 );
  VERIFY( equal(mset13.begin(), mset13.end(), mset03_ref.begin()) );
  VERIFY( mset14.size() == size01 );
  VERIFY( equal(mset14.begin(), mset14.end(), mset03_ref.begin()) );
  VERIFY( mset13.get_allocator().get_personality() == personality02 );
  VERIFY( mset14.get_allocator().get_personality() == personality01 );
}

int main()
{ 
  test01();
  return 0;
}
