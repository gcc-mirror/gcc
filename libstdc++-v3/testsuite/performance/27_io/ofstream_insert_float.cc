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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <fstream>
#include <sstream>
#include <testsuite_performance.h>

// Based on libstdc++/8761 poor fstream performance (converted to float)
void test_insertion(int p = 6)
{
  using namespace std;
  using namespace __gnu_test;

  const char* filename = "tmp_perf_float.txt";
  const int iterations = 10000000;

  ostringstream oss;
  oss << "precision " << p;

  {
    time_counter time;
    resource_counter resource;

    ofstream out(filename);
    out.precision(p);
    start_counters(time, resource);
    for (int i = 0; i < iterations; ++i)
      {
	float f = i * 3.14159265358979323846;
	out << f << '\n';
      }
    stop_counters(time, resource);
    report_performance(__FILE__, oss.str(), time, resource);
  }

  unlink(filename);
};

int main()
{
  test_insertion(6);
  test_insertion(12);
  return 0;
}
