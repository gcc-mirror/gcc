// 24.1.5 Random access iterators
// 24.3.1 Iterator traits
// (basic_string and vector implementations)
//
// Copyright (C) 1999-2025 Free Software Foundation, Inc.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or 
// (at your option) any later version.                             
//                                                         
// This program is distributed in the hope that it will be useful,   
// but WITHOUT ANY WARRANTY; without even the implied warranty of    
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <string>
#include <vector>
#include <testsuite_hooks.h>

void
string_stuff()
{
   std::string s("abcde");

   std::string::iterator i1(s.begin());
   VERIFY( *i1 == 'a' );

   ++i1;
   VERIFY( *i1 == 'b' );

   VERIFY( *i1++ == 'b' );
   VERIFY( *i1 == 'c' );

   ++ ++i1;
   VERIFY( *i1 == 'e' );

   --i1;
   VERIFY( *i1 == 'd' );

   VERIFY( *i1-- == 'd' );
   VERIFY( *i1 == 'c' );

   -- --i1;
   VERIFY( *i1 == 'a' );

   std::string::iterator i2;
   i2 = s.end();
   std::iterator_traits<std::string::iterator>::difference_type d1;
   d1 = i2 - i1;
   VERIFY( d1 == 5 );

   std::iterator_traits<std::string::iterator>::value_type v1;
   v1 = i1[0];
   VERIFY( v1 == 'a' );

   std::iterator_traits<std::string::iterator>::reference r1(i1[0]);
   VERIFY( r1 == 'a' );
   r1 = 'x';
   VERIFY( r1 == 'x' );
   r1 = 'a';

   VERIFY( (i1 != i2) == true );
   VERIFY( (i1 == i2) == false );
   VERIFY( (i1 <  i2) == true );
   VERIFY( (i1 >  i2) == false );
   VERIFY( (i1 <= i2) == true );
   VERIFY( (i1 >= i2) == false );

   std::string::iterator i3;
   i3 = i1;
   VERIFY( (i3 == i1) == true );

   i3 += 5;
   VERIFY( (i3 == i2) == true );

   i3 -= 5;
   VERIFY( (i3 == i1) == true );

   VERIFY( i3 + 5 == i2 );
   VERIFY( 5 + i3 == i2 );
   VERIFY( i2 - 5 == i3 );

   VERIFY( i1[0] == 'a' );

   i1[4] = 'x';
   VERIFY( i2[-1] == 'x' );
   i1[4] = 'e';

   i1[2] = 'x';
   VERIFY( i2[-3] == 'x' );
   i1[2] = 'c';

   std::string::const_iterator ci1(s.begin());
   VERIFY( *ci1 == 'a' );

   ++ci1;
   VERIFY( *ci1 == 'b' );

   VERIFY( *ci1++ == 'b' );
   VERIFY( *ci1 == 'c' );

   ++ ++ci1;
   VERIFY( *ci1 == 'e' );

   --ci1;
   VERIFY( *ci1 == 'd' );

   VERIFY( *ci1-- == 'd' );
   VERIFY( *ci1 == 'c' );

   -- --ci1;
   VERIFY( *ci1 == 'a' );

   std::string::const_iterator ci2;
   ci2 = s.end();
   std::iterator_traits<std::string::const_iterator>::difference_type d2;
   d2 = ci2 - ci1;
   VERIFY( d2 == 5 );

   std::iterator_traits<std::string::const_iterator>::value_type v2;
   v2 = ci1[0];
   VERIFY( v2 == 'a' );

   std::iterator_traits<std::string::const_iterator>::reference r2(ci1[0]);
   VERIFY( r2 == 'a' );

   VERIFY( (ci1 != ci2) == true );
   VERIFY( (ci1 == ci2) == false );
   VERIFY( (ci1 <  ci2) == true );
   VERIFY( (ci1 >  ci2) == false );
   VERIFY( (ci1 <= ci2) == true );
   VERIFY( (ci1 >= ci2) == false );

   std::string::const_iterator ci3;
   ci3 = ci1;
   VERIFY( ci3 == ci1 );

   ci3 += 5;
   VERIFY( ci3 == ci2 );

   ci3 -= 5;
   VERIFY( ci3 == ci1 );

   VERIFY( ci3 + 5 == ci2 );
   VERIFY( 5 + ci3 == ci2 );
   VERIFY( ci2 - 5 == ci3 );

   VERIFY( ci1[2] == 'c' );
   VERIFY( ci2[-1] == 'e' );

   // iterator and const_iterator
   std::string::const_iterator ci4(i1);
   VERIFY( (ci4 == i1) == true );
   VERIFY( (ci4 != i1) == false );
   VERIFY( (ci4 < i1) == false );
   VERIFY( (ci4 > i1) == false );
   VERIFY( (ci4 <= i1) == true );
   VERIFY( (ci4 >= i1) == true );
   ci4 = i2;
   VERIFY( (i2 == ci4) == true );
   VERIFY( (i2 < ci4) == false );
   VERIFY( (i2 > ci4) == false );
   VERIFY( (i2 <= ci4) == true );
   VERIFY( (i2 >= ci4) == true );

   const std::string cs("ABCDE");
   std::string::const_iterator ci5(cs.begin());
   VERIFY( ci5[0] == 'A' );
}

void
vector_stuff()
{
   int failures(0);

   std::vector<int> v;
   v.push_back(int(1));
   v.push_back(int(2));
   v.push_back(int(3));
   v.push_back(int(4));
   v.push_back(int(5));

   std::vector<int>::iterator i1(v.begin());
   VERIFY( *i1 == 1 );

   ++i1;
   VERIFY( *i1 == 2 );

   VERIFY( *i1++ == 2 );
   VERIFY( *i1 == 3 );

   ++ ++i1;
   VERIFY( *i1 == 5 );

   --i1;
   VERIFY( *i1 == 4 );

   VERIFY( *i1-- == 4 );
   VERIFY( *i1 == 3 );

   -- --i1;
   VERIFY( *i1 == 1 );

   std::vector<int>::iterator i2;
   i2 = v.end();
   std::iterator_traits<std::vector<int>::iterator>::difference_type d1;
   d1 = i2 - i1;
   VERIFY( d1 == 5 );

   std::iterator_traits<std::vector<int>::iterator>::value_type v1;
   v1 = i1[0];
   VERIFY( v1 == 1 );

   std::iterator_traits<std::vector<int>::iterator>::reference r1(i1[0]);
   VERIFY( r1 == 1 );
   r1 = 9;
   VERIFY( r1 == 9 );
   r1 = 1;

   VERIFY( (i1 != i2) == true );
   VERIFY( (i1 == i2) == false );
   VERIFY( (i1 <  i2) == true );
   VERIFY( (i1 >  i2) == false );
   VERIFY( (i1 <= i2) == true );
   VERIFY( (i1 >= i2) == false );

   std::vector<int>::iterator i3;
   i3 = i1;
   VERIFY( (i3 == i1) == true );

   i3 += 5;
   VERIFY( (i3 == i2) == true );

   i3 -= 5;
   VERIFY( (i3 == i1) == true );

   VERIFY( i3 + 5 == i2 );
   VERIFY( 5 + i3 == i2 );
   VERIFY( i2 - 5 == i3 );

   VERIFY( i1[0] == 1 );

   i1[4] = 9;
   VERIFY( i2[-1] == 9 );
   i1[4] = 5;

   i1[2] = 9;
   VERIFY( i2[-3] == 9 );
   i1[2] = 3;

   std::vector<int>::const_iterator ci1(v.begin());
   VERIFY( *ci1 == 1 );

   ++ci1;
   VERIFY( *ci1 == 2 );

   VERIFY( *ci1++ == 2 );
   VERIFY( *ci1 == 3 );

   ++ ++ci1;
   VERIFY( *ci1 == 5 );

   --ci1;
   VERIFY( *ci1 == 4 );

   VERIFY( *ci1-- == 4 );
   VERIFY( *ci1 == 3 );

   -- --ci1;
   VERIFY( *ci1 == 1 );

   std::vector<int>::const_iterator ci2;
   ci2 = v.end();
   std::iterator_traits<std::vector<int>::const_iterator>::difference_type d2;
   d2 = ci2 - ci1;
   VERIFY( d2 == 5 );

   std::iterator_traits<std::vector<int>::const_iterator>::value_type v2;
   v2 = ci1[0];
   VERIFY( v2 == 1 );

   std::iterator_traits<std::vector<int>::const_iterator>::reference
      r2(ci1[0]);
   VERIFY( r2 == 1 );

   VERIFY( (ci1 != ci2) == true );
   VERIFY( (ci1 == ci2) == false );
   VERIFY( (ci1 <  ci2) == true );
   VERIFY( (ci1 >  ci2) == false );
   VERIFY( (ci1 <= ci2) == true );
   VERIFY( (ci1 >= ci2) == false );

   std::vector<int>::const_iterator ci3;
   ci3 = ci1;
   VERIFY( (ci3 == ci1) == true );

   ci3 += 5;
   VERIFY( (ci3 == ci2) == true );

   ci3 -= 5;
   VERIFY( (ci3 == ci1) == true );

   VERIFY( ci3 + 5 == ci2 );
   VERIFY( 5 + ci3 == ci2 );
   VERIFY( ci2 - 5 == ci3 );

   VERIFY( ci1[2] == 3 );

   VERIFY( ci2[-1] == 5 );

   // iterator to const_iterator
   std::vector<int>::const_iterator ci4(i1);
   VERIFY( (ci4 == i1) == true );
   VERIFY( (ci4 != i1) == false );
   VERIFY( (ci4 < i1) == false );
   VERIFY( (ci4 > i1) == false );
   VERIFY( (ci4 <= i1) == true );
   VERIFY( (ci4 >= i1) == true );
   ci4 = i2;
   VERIFY( (i2 == ci4) == true );
   VERIFY( (i2 < ci4) == false );
   VERIFY( (i2 > ci4) == false );
   VERIFY( (i2 <= ci4) == true );
   VERIFY( (i2 >= ci4) == true );

   const std::vector<int> cv(v);
   std::vector<int>::const_iterator ci5(cv.begin());
   VERIFY( ci5[0] == 1 );

   std::vector<std::string> vs;
   vs.push_back(std::string("abc"));
   std::vector<std::string>::iterator ivs(vs.begin());
   VERIFY( ivs->c_str()[1] == 'b' );
}

void 
reverse_stuff()
{
   std::string s("abcde");

   std::string::reverse_iterator ri(s.rbegin());
   VERIFY( *ri == 'e' );

   std::iterator_traits<std::string::reverse_iterator>::difference_type d;
   d = s.rend() - ri;
   VERIFY( d == 5 );

   const std::string cs("abcde");
   std::string::const_reverse_iterator cri(cs.rend());
   VERIFY( cri - 5 == cs.rbegin() );
}

// libstdc++/6642
void
test6642()
{
   std::string s;
   std::string::iterator it = s.begin();
   std::string::const_iterator cit = s.begin();
   VERIFY( (it - cit) == 0 );
}

int
main()
{
   string_stuff();
   vector_stuff();
   reverse_stuff();
   test6642();
   return 0;
}
