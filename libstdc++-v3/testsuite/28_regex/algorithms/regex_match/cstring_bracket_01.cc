// { dg-do run { target c++11 } }

//
// 2013-08-01  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.11.2 regex_match
// Tests Extended bracket expression against a C-string.

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  bool test __attribute__((unused)) = true;

  {
    std::regex  re("pre/[za-x]", std::regex::extended);
    VERIFY( regex_match_debug("pre/z", re) );
    VERIFY( regex_match_debug("pre/a", re) );
    VERIFY( !regex_match_debug("pre/y", re) );
  }
  {
    std::regex  re("pre/[[:uPPer:]]", std::regex::extended);
    VERIFY( regex_match_debug("pre/Z", re) );
    VERIFY( !regex_match_debug("pre/_", re) );
    VERIFY( !regex_match_debug("pre/a", re) );
    VERIFY( !regex_match_debug("pre/0", re) );
  }
  {
    std::regex  re("pre/[[:lOWer:]]", std::regex::extended | std::regex::icase);
    VERIFY( regex_match_debug("pre/Z", re) );
    VERIFY( regex_match_debug("pre/a", re) );
  }
  {
    std::regex  re("pre/[[:w:][.tilde.]]", std::regex::extended);
    VERIFY( regex_match_debug("pre/~", re) );
    VERIFY( regex_match_debug("pre/_", re) );
    VERIFY( regex_match_debug("pre/a", re) );
    VERIFY( regex_match_debug("pre/0", re) );
  }
  {
    std::regex  re("pre/[[=a=]]", std::regex::extended);
    VERIFY( regex_match_debug("pre/a", re) );
    VERIFY( regex_match_debug("pre/A", re) );
  }
}

void
test02()
{
  bool test __attribute__((unused)) = true;

  try
  {
    std::regex re("[-----]", std::regex::extended);
    VERIFY(false);
  }
  catch (const std::regex_error& e)
  {
    VERIFY(e.code() == std::regex_constants::error_range);
  }
  std::regex re("[-----]", std::regex::ECMAScript);

  VERIFY(!regex_match("b", regex("[-ac]", regex_constants::extended)));
  VERIFY(!regex_match("b", regex("[ac-]", regex_constants::extended)));
  VERIFY(regex_match("b", regex("[^-ac]", regex_constants::extended)));
  VERIFY(regex_match("b", regex("[^ac-]", regex_constants::extended)));
  VERIFY(regex_match("&", regex("[%--]", regex_constants::extended)));
  VERIFY(regex_match(".", regex("[--@]", regex_constants::extended)));
  try
  {
    regex("[a--@]", regex_constants::extended);
    VERIFY(false);
  }
  catch (const std::regex_error& e)
  {
  }
  VERIFY(regex_match("].", regex("[][.hyphen.]-0]*", regex_constants::extended)));
}

void
test03()
{
  bool test __attribute__((unused)) = true;

  try
  {
    std::regex re("[z-a]", std::regex::extended);
    VERIFY(false);
  }
  catch (const std::regex_error& e)
  {
    VERIFY(e.code() == std::regex_constants::error_range);
  }
}

void
test04()
{
  bool test __attribute__((unused)) = true;

  std::regex re("[-0-9a-z]");
  VERIFY(regex_match_debug("-", re));
  VERIFY(regex_match_debug("1", re));
  VERIFY(regex_match_debug("w", re));
  re.assign("[-0-9a-z]", regex_constants::basic);
  VERIFY(regex_match_debug("-", re));
  VERIFY(regex_match_debug("1", re));
  VERIFY(regex_match_debug("w", re));
}

// libstdc++/67015
void
test05()
{
  bool test __attribute__((unused)) = true;

  regex lanana_namespace("^[a-z0-9]+$", regex::extended);
  regex lsb_namespace("^_?([a-z0-9_.]+-, regex::extended)+[a-z0-9]+$");
  regex debian_dpkg_conffile_cruft("dpkg-(old|dist|new|tmp, regex::extended)$");
  regex debian_cron_namespace("^[a-z0-9][a-z0-9-]*$", regex::extended);
  VERIFY(regex_match("test", debian_cron_namespace));
  VERIFY(!regex_match("-a", debian_cron_namespace));
  VERIFY(regex_match("a-", debian_cron_namespace));
  regex debian_cron_namespace_ok("^[a-z0-9][-a-z0-9]*$", regex::extended);
  VERIFY(regex_match("test", debian_cron_namespace_ok));
  VERIFY(!regex_match("-a", debian_cron_namespace_ok));
  VERIFY(regex_match("a-", debian_cron_namespace_ok));
}

// libstdc++/67015
void
test06()
{
  bool test __attribute__((unused)) = true;

  regex lanana_namespace("^[a-z0-9]+$");
  regex lsb_namespace("^_?([a-z0-9_.]+-)+[a-z0-9]+$");
  regex debian_dpkg_conffile_cruft("dpkg-(old|dist|new|tmp)$");
  regex debian_cron_namespace("^[a-z0-9][a-z0-9-]*$");
  VERIFY(regex_match("test", debian_cron_namespace));
  VERIFY(!regex_match("-a", debian_cron_namespace));
  VERIFY(regex_match("a-", debian_cron_namespace));
  regex debian_cron_namespace_ok("^[a-z0-9][-a-z0-9]*$");
  VERIFY(regex_match("test", debian_cron_namespace_ok));
  VERIFY(!regex_match("-a", debian_cron_namespace_ok));
  VERIFY(regex_match("a-", debian_cron_namespace_ok));
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();

  return 0;
}
