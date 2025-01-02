// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

// 2013-10-26  Tim Shen  <timshen91@gmail.com>

#include <regex>

using namespace std;

void split(string s)
{
    regex re("\\s+");
    for (auto it = sregex_token_iterator(s.begin(), s.end(), re, -1);
	 it != sregex_token_iterator();
	 ++it)
      {
      }
}

string source = "\
// Copyright (C) 2013-2025 Free Software Foundation, Inc.\n\
//\n\
// This file is part of the GNU ISO C++ Library.  This library is free\n\
// software; you can redistribute it and/or modify it under the\n\
// terms of the GNU General Public License as published by the\n\
// Free Software Foundation; either version 3, or (at your option)\n\
// any later version.\n\
\n\
// This library is distributed in the hope that it will be useful,\n\
// but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
// GNU General Public License for more details.\n\
\n\
// You should have received a copy of the GNU General Public License along\n\
// with this library; see the file COPYING3.  If not see\n\
// <http://www.gnu.org/licenses/>.\n\
\n\
// 2013-10-08  Tim Shen  <timshen91@gmail.com>\n\
\n\
#include <testsuite_performance.h>\n\
#include <regex>\n\
\n\
using namespace __gnu_test;\n\
using namespace std;\n\
\n\
void split(string s)\n\
{\n\
    regex re(\"\\s+\");\n\
    for (auto it = sregex_token_iterator(s.begin(), s.end(), re, -1);\n\
	 it != sregex_token_iterator();\n\
	 ++it)\n\
      {\n\
      }\n\
}\n\
\n\
int main()\n\
{\n\
  string source = \"\";\n\
  time_counter time;\n\
  resource_counter resource;\n\
\n\
  source = source + source;\n\
  source = source + source;\n\
  source = source + source;\n\
  source = source + source;\n\
  source = source + source;\n\
  source = source + source;\n\
  source = source + source;\n\
  source = source + source;\n\
\n\
  start_counters(time, resource);\n\
  split(source);\n\
  stop_counters(time, resource);\n\
  report_performance(__FILE__, \"\", time, resource);\n\
\n\
  return 0;\n\
}\n";
