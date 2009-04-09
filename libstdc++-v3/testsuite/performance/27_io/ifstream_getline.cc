// Copyright (C) 2003, 2004, 2005, 2006, 2007, 2009
// Free Software Foundation, Inc.
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


#include <fstream>
#include <cstdlib>
#include <testsuite_performance.h>

// libstdc++/5001 (100,000 line input file)
int main ()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const char* name1 = "/usr/share/dict/words";
  const char* name2 = "/usr/share/dict/linux.words";
  ifstream in;
  in.open(name1);
  if (!in.is_open())
    {
      in.clear();
      in.open(name2);
    }
  if (!in.is_open())
    exit(1);

  char buffer[BUFSIZ];
  start_counters(time, resource);
  while (in.good()) 
    in.getline(buffer, BUFSIZ);
  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}
