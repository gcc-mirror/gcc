// 1999-06-08 bkoz

// Copyright (C) 1999 Free Software Foundation, Inc.
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

// 21.3 template class basic_string

#include <string>
#include <stdexcept>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

// Do a quick sanity check on known problems with element access and
// ref-counted strings. These should all pass, regardless of the
// underlying string implementation, of course.
bool test01(void)
{
  bool test = true;
  typedef std::string::size_type csize_type;
  typedef std::string::iterator siterator;
  typedef std::string::reverse_iterator sriterator;
  csize_type npos = std::string::npos;
  csize_type csz01, csz02;
  siterator it1;
  sriterator rit1;  

  std::string str01("montara beach, half moon bay");
  const std::string str02("ocean beach, san francisco");
  std::string str03;

  // 21.3 p 5

  // References, pointers, and iterators referring to the elements of
  // a basic_string may be invalidated by the following uses of that
  // basic_string object:

  // ...

  // Susequent to any of the above uses except the forms of insert()
  // and erase() which return iterators, the first call to non-const
  // member functions operator[](), at(), begin(), rbegin(), end(), or
  // rend()

  str03 = str01;
  it1 = str01.begin();
  *it1 = 'x';
  test &= str01[0] == 'x';
  test &= str03[0] == 'm';

  str03 = str01; 
  csz01 = str01.size();
  rit1 = str01.rbegin(); // NB: Pointing at one-past the end, so ...
  *rit1 = 'z'; 		 // ... but it's taken care of here 
  test &= str01[csz01 - 1] == 'z';
  test &= str03[csz01 - 1] == 'y';

  str03 = str01;
  csz01 = str01.size();
  std::string::reference r1 = str01.at(csz01 - 2);
  test &= str03 == str01;
  r1 = 'd';
  test &= str01[csz01 - 2] == 'd';
  test &= str03[csz01 - 2] == 'a';

  str03 = str01; 
  csz01 = str01.size();
  std::string::reference r2 = str01[csz01 - 3];
  test &= str03 == str01;
  r2 = 'w'; 
  test &= str01[csz01 - 3] == 'w';
  test &= str03[csz01 - 3] == 'b';

  str03 = str01;
  csz02 = str01.size();
  it1 = str01.end();
  test &= str03 == str01;
  --it1;
  *it1 = 'q'; 
  test &= str01[csz02 - 1] == 'q';
  test &= str03[csz02 - 1] == 'z';

  str03 = str01;
  rit1 = str01.rend();
  test &= str03 == str01;
  --rit1; 	
  *rit1 = 'p'; 
  test &= str01[0] == 'p';
  test &= str03[0] == 'x';

  // need to also test for const begin/const end
#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return test;
}

// Do another sanity check, this time for member functions that return
// iterators, namely insert and erase.
bool test02(void)
{
  bool test = true;
  typedef std::string::size_type csize_type;
  typedef std::string::iterator siterator;
  typedef std::string::reverse_iterator sriterator;
  csize_type npos = std::string::npos;
  csize_type csz01, csz02;
  siterator it1;
  sriterator rit1;  

  const std::string str01("its beach, santa cruz");

  std::string str02 = str01;
  std::string str05 = str02; // optional, so that begin below causes a mutate
  std::string::iterator p = str02.insert(str02.begin(), ' ');
  std::string str03 = str02;
  test &= str03 == str02;
  *p = '!';
  test &= *str03.c_str() == ' ';
  str03[0] = '@';
  test &= str02[0] == '!';
  test &= *p == '!';
  test &= str02 != str05;
  test &= str02 != str03;

  std::string str10 = str01;
  std::string::iterator p2 = str10.insert(str10.begin(), 'a');
  std::string str11 = str10;
  *p2 = 'e';
  test &= str11 != str10;

  std::string str06 = str01;
  std::string str07 = str06; // optional, so that begin below causes a mutate
  p = str06.erase(str06.begin());
  std::string str08 = str06;
  test &= str08 == str06;
  *p = '!';
  test &=  *str08.c_str() == 't';
  str08[0] = '@';
  test &= str06[0] == '!';
  test &= *p == '!';
  test &= str06 != str07;
  test &= str06 != str08;

  std::string str12 = str01;
  p2 = str12.erase(str12.begin(), str12.begin() + str12.size() - 1);
  std::string str13 = str12;
  *p2 = 'e';
  test &= str12 != str13;

#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return test;
}

int main()
{ 
  test01();
  test02();
}





