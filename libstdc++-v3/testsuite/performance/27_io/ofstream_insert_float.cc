// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

#include <unistd.h>
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
