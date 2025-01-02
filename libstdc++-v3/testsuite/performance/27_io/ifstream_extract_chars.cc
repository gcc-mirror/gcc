// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
#include <cstdio>
#include <fstream>
#include <string>
#include <testsuite_performance.h>

// libstdc++/22515
int main() 
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const char filename[] = "tmp_perf_chars.txt";
  const unsigned lines = 200000;
  const unsigned line_length = 200;

  char* line = new char[line_length + 2];

  // Construct data.
  {
    memset(line, 'x', line_length);
    line[line_length] = '\n';
    line[line_length + 1] = '\0';
    
    ofstream out(filename);
    for (unsigned i = 0; i < lines; ++i)
      out << line;
  }
  
  // operator>>(basic_istream<char>& __in, basic_string<char>& __str)
  {
    start_counters(time, resource);
    for (int iter = 0; iter < 25; ++iter)
      {
	ifstream file(filename);
	string string_line;
	
	while (file >> string_line);
      }
    stop_counters(time, resource);
    report_performance(__FILE__, "string&", time, resource);
    clear_counters(time, resource);
  }
  
  // operator>>(basic_istream<char>& __in, char* __s)
  {
    start_counters(time, resource);
    for (int iter = 0; iter < 25; ++iter)
      {
	ifstream file(filename);
	
	while (file >> line);
      }
    stop_counters(time, resource);
    report_performance(__FILE__, "char*", time, resource);
    clear_counters(time, resource);
  }

  delete[] line;
  unlink(filename);
  return 0;
}
