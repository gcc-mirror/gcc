// Copyright (C) 2004-2020 Free Software Foundation, Inc.
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


#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <testsuite_performance.h>

// libstdc++/11722
int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const int iterations = 500000;
  const int chunksize = 100;

  char* chunk = new char[chunksize];
  const char* name1 = "/usr/share/dict/words";
  const char* name2 = "/usr/share/dict/linux.words";
  const char* name = name1;

  // C
  FILE* file;
  if (!(file = fopen(name, "r")))
    {
      name = name2;
      if (!(file = fopen(name, "r")))
	exit(1);
    }
  setvbuf(file, 0, _IONBF, 0);
  start_counters(time, resource);
  for (int i = 0; i < iterations; ++i)
    if (fread(chunk, 1, chunksize, file) < chunksize)
      fseek(file, 0, SEEK_SET);
  stop_counters(time, resource);
  fclose(file);
  report_performance(__FILE__, "C", time, resource);
  clear_counters(time, resource);

  // C unlocked
  file = fopen(name, "r");
  setvbuf(file, 0, _IONBF, 0);
  start_counters(time, resource);
  for (int i = 0; i < iterations; ++i)
    if (fread_unlocked(chunk, 1, chunksize, file) < chunksize)
      fseek(file, 0, SEEK_SET);
  stop_counters(time, resource);
  fclose(file);
  report_performance(__FILE__, "C unlocked", time, resource);
  clear_counters(time, resource);
  
  // C++
  filebuf buf;
  buf.pubsetbuf(0, 0);
  buf.open(name, ios_base::in);
  start_counters(time, resource);
  for (int i = 0; i < iterations; ++i)
    if (buf.sgetn(chunk, chunksize) < chunksize)
      buf.pubseekoff(0, ios::beg);
  stop_counters(time, resource);
  report_performance(__FILE__, "C++", time, resource);

  delete [] chunk;

  return 0;
}
