// 2004-08-25  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation
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

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/17038
void test01()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;

  // create "C" time objects
  tm time1 = { 0, 0, 12, 4, 3, 71, 0, 93, 0 };

  // basic construction
  locale loc_c = locale::classic();
  locale loc_in = __gnu_test::try_named_locale("ta_IN");
  assert( loc_in != loc_c );

  // create an ostream-derived object, cache the time_put facet
  ostringstream oss;
  oss.imbue(loc_in);
  const time_put<char>& tim_put =
    use_facet<time_put<char> >(oss.getloc()); 

  iterator_type os_it01 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'c');
  string result1 = oss.str();

  char time_buffer[128];
  setlocale(LC_ALL, "ta_IN");
  VERIFY( strftime(time_buffer, 128, "%c", &time1) );
  
  VERIFY( result1 == time_buffer );
}

int main()
{
  test01();
  return 0;
}
