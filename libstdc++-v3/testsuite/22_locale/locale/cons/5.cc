// { dg-require-namedlocale "en_PH" }
// { dg-require-namedlocale "de_DE" }
// { dg-require-namedlocale "it_IT" }

// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000-2017 Free Software Foundation, Inc.
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <cwchar> // for mbstate_t
#include <cstring>
#include <cstdlib>
#include <locale>
#include <stdexcept>
#include <testsuite_hooks.h>

// More tests for locale("") == POSIX locale::name. 
void test04()
{
  using namespace std;

#ifdef _GLIBCXX_HAVE_SETENV

  char* LANG_orig = strdup(getenv("LANG") ? getenv("LANG") : "");

  // Check that a "POSIX" LC_ALL is equivalent to "C".
  if (!setenv("LC_ALL", "POSIX", 1))
    {
      locale loc = locale("");
      VERIFY( loc.name() == "C" );
    }
  setenv("LC_ALL", "", 1);

  // Check that a "en_PH" LC_ALL is equivalent to "en_PH".
  if (!setenv("LC_ALL", "en_PH", 1))
    {
      locale loc = locale("");
      VERIFY( loc.name() == "en_PH" );
    }
  setenv("LC_ALL", "", 1);

  // Explicit check that LC_ALL sets regardless of LC_* and LANG.
  if (!setenv("LANG", "es_MX", 1) && !setenv("LC_COLLATE", "de_DE", 1))
    {
      if (!setenv("LC_ALL", "en_PH", 1))
	{
	  locale loc = locale("");
	  VERIFY( loc.name() == "en_PH" );
	}
      setenv("LC_ALL", "", 1);
      setenv("LANG", LANG_orig, 1);
    }

  // NB: LANG checks all LC_* macro settings. As such, all LC_* macros
  // must be cleared for these tests.
  setenv("LC_ALL", "", 1);
  setenv("LC_CTYPE", "", 1);
  setenv("LC_NUMERIC", "", 1);
  setenv("LC_TIME", "", 1);
  setenv("LC_COLLATE", "", 1);
  setenv("LC_MONETARY", "", 1);
  setenv("LC_MESSAGES", "", 1);
#if _GLIBCXX_NUM_CATEGORIES
  setenv("LC_PAPER", "", 1);
  setenv("LC_NAME", "", 1);
  setenv("LC_ADDRESS", "", 1);
  setenv("LC_TELEPHONE", "", 1);
  setenv("LC_MEASUREMENT", "", 1);
  setenv("LC_IDENTIFICATION", "", 1);
#endif

  // Check the default set by LANG.
  if (!setenv("LANG", "fr_FR", 1))
    {
      locale loc = locale("");
      VERIFY( loc.name() == "fr_FR" );
    }

  // Check that a "POSIX" LANG is equivalent to "C".
  if (!setenv("LANG", "POSIX", 1))
    {
      locale loc(""); 
      VERIFY( loc.name() == "C" );
    }

  // Setting a category in the "C" default.
  if (!setenv("LC_COLLATE", "de_DE", 1))
    {
      locale loc = locale("");

#if _GLIBCXX_NUM_CATEGORIES
      VERIFY( loc.name() == "LC_CTYPE=C;LC_NUMERIC=C;LC_TIME=C;"
              "LC_COLLATE=de_DE;LC_MONETARY=C;LC_MESSAGES=C;LC_PAPER=C;"
	      "LC_NAME=C;LC_ADDRESS=C;LC_TELEPHONE=C;LC_MEASUREMENT=C;"
	      "LC_IDENTIFICATION=C" );
#else
      VERIFY( loc.name() == "LC_CTYPE=C;LC_NUMERIC=C;LC_TIME=C;"
	      "LC_COLLATE=de_DE;LC_MONETARY=C;LC_MESSAGES=C" );
#endif
    }

  // Changing the LANG default while LC_COLLATE is set.
  if (!setenv("LANG", "fr_FR", 1))
    {
      locale loc = locale("");
#if _GLIBCXX_NUM_CATEGORIES
      VERIFY( loc.name() == "LC_CTYPE=fr_FR;LC_NUMERIC=fr_FR;"
	      "LC_TIME=fr_FR;LC_COLLATE=de_DE;LC_MONETARY=fr_FR;"
	      "LC_MESSAGES=fr_FR;LC_PAPER=fr_FR;LC_NAME=fr_FR;"
	      "LC_ADDRESS=fr_FR;LC_TELEPHONE=fr_FR;LC_MEASUREMENT=fr_FR;"
	      "LC_IDENTIFICATION=fr_FR" );
#else
      VERIFY( loc.name() == "LC_CTYPE=fr_FR;LC_NUMERIC=fr_FR;"
	      "LC_TIME=fr_FR;LC_COLLATE=de_DE;LC_MONETARY=fr_FR;"
	      "LC_MESSAGES=fr_FR" );
#endif
    }
  
  // Changing another (C only) category.
#if _GLIBCXX_NUM_CATEGORIES
  if (!setenv("LC_IDENTIFICATION", "it_IT", 1))
    {
      locale loc = locale("");
      VERIFY( loc.name() == "LC_CTYPE=fr_FR;LC_NUMERIC=fr_FR;"
	      "LC_TIME=fr_FR;LC_COLLATE=de_DE;LC_MONETARY=fr_FR;"
	      "LC_MESSAGES=fr_FR;LC_PAPER=fr_FR;LC_NAME=fr_FR;"
	      "LC_ADDRESS=fr_FR;LC_TELEPHONE=fr_FR;LC_MEASUREMENT=fr_FR;"
	      "LC_IDENTIFICATION=it_IT" );
    }
#endif

  free(LANG_orig);

#endif // _GLIBCXX_HAVE_SETENV
}

int main()
{
  test04();
  return 0;
}
