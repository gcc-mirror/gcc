 // Copyright (C) 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <ctime>
#include <iostream>
#include <string>

using namespace std;

void
test_append_char(int how_much)
{
  string buf; // no preallocation
  for (int i = 0; i < how_much; ++i)
     buf.append(static_cast<string::size_type>(1) , 'x');
}

void
test_append_string(int how_much)
{
  string s(static_cast<string::size_type>(1) , 'x');
  string buf; // no preallocation
  for (int i = 0; i < how_much; ++i)
     buf.append(s);
}

void 
run_benchmark1(int how_much)
{
  clock_t t0 = clock();
  test_append_char(how_much);
  clock_t t1 = clock();
  cout << "Execution time of " << how_much
       << " string::append(char) calls: " 
       << (static_cast<float>(t1 - t0)/CLOCKS_PER_SEC) << " sec."<< endl;
}

void 
run_benchmark2(int how_much)
{
  clock_t t0 = clock();
  test_append_string(how_much);
  clock_t t1 = clock();
  cout << "Execution time of " << how_much
       << " string::append(const string&) calls: " 
       << (static_cast<float>(t1 - t0)/CLOCKS_PER_SEC) << " sec." << endl;
}

// libstdc++/5380
// libstdc++/4960
int main()
{
  run_benchmark1(10000);
  run_benchmark2(10000);
  run_benchmark1(100000);
  run_benchmark2(100000);
  run_benchmark1(1000000);
  run_benchmark2(1000000);
}
