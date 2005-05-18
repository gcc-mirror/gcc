// { dg-require-namedlocale "" }

// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000, 2001, 2002, 2003, 2005 Free Software Foundation
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <cwchar> // for mbstate_t
#include <locale>
#include <stdexcept>
#include <testsuite_hooks.h>

// More tests for locale("") == POSIX locale::name. 
void test04()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

#ifdef _GLIBCXX_HAVE_SETENV

  const char* LANG_orig = getenv("LANG") ? strdup(getenv("LANG")) : "";
  const char* LC_ALL_orig = getenv("LC_ALL") ? strdup(getenv("LC_ALL")) : "";
  const char* LC_CTYPE_orig = 
    getenv("LC_CTYPE") ? strdup(getenv("LC_CTYPE")) : "";
  const char* LC_NUMERIC_orig = 
    getenv("LC_NUMERIC") ? strdup(getenv("LC_NUMERIC")) : "";
  const char* LC_TIME_orig = 
    getenv("LC_TIME") ? strdup(getenv("LC_TIME")) : "";
  const char* LC_COLLATE_orig =
    getenv("LC_COLLATE") ? strdup(getenv("LC_COLLATE")) : "";
  const char* LC_MONETARY_orig = 
    getenv("LC_MONETARY") ? strdup(getenv("LC_MONETARY")) : "";
  const char* LC_MESSAGES_orig = 
    getenv("LC_MESSAGES") ? strdup(getenv("LC_MESSAGES")) : "";
#if _GLIBCXX_NUM_CATEGORIES
  const char* LC_PAPER_orig = 
    getenv("LC_PAPER") ? strdup(getenv("LC_PAPER")) : "";
  const char* LC_NAME_orig = 
    getenv("LC_NAME") ? strdup(getenv("LC_NAME")) : "";
  const char* LC_ADDRESS_orig = 
    getenv("LC_ADDRESS") ? strdup(getenv("LC_ADDRESS")) : "";
  const char* LC_TELEPHONE_orig = 
    getenv("LC_TELEPHONE") ? strdup(getenv("LC_TELEPHONE")) : "";
  const char* LC_MEASUREMENT_orig = 
    getenv("LC_MEASUREMENT") ? strdup(getenv("LC_MEASUREMENT")) : "";
  const char* LC_IDENTIFICATION_orig =
    getenv("LC_IDENTIFICATION") ? strdup(getenv("LC_IDENTIFICATION")) : "";
#endif

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
      setenv("LANG", LANG_orig ? LANG_orig : "", 1);
      setenv("LC_COLLATE", LC_COLLATE_orig ? LC_COLLATE_orig : "", 1);
    }

  // NB: LANG checks all LC_* macro settings. As such, all LC_* macros
  // must be cleared for these tests, and then restored.
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

  // Restore the environment.
  setenv("LANG", LANG_orig ? LANG_orig : "", 1);
  setenv("LC_ALL", LC_ALL_orig ? LC_ALL_orig : "", 1);
  setenv("LC_CTYPE", LC_CTYPE_orig ? LC_CTYPE_orig : "", 1);
  setenv("LC_NUMERIC", LC_NUMERIC_orig ? LC_NUMERIC_orig : "", 1);
  setenv("LC_TIME", LC_TIME_orig ? LC_TIME_orig : "", 1);
  setenv("LC_COLLATE", LC_COLLATE_orig ? LC_COLLATE_orig : "", 1);
  setenv("LC_MONETARY", LC_MONETARY_orig ? LC_MONETARY_orig : "", 1);
  setenv("LC_MESSAGES", LC_MESSAGES_orig ? LC_MESSAGES_orig : "", 1);
#if _GLIBCXX_NUM_CATEGORIES
  setenv("LC_PAPER", LC_PAPER_orig ? LC_PAPER_orig : "", 1);
  setenv("LC_NAME", LC_NAME_orig ? LC_NAME_orig : "", 1);
  setenv("LC_ADDRESS", LC_ADDRESS_orig ? LC_ADDRESS_orig : "", 1);
  setenv("LC_TELEPHONE", LC_TELEPHONE_orig ? LC_TELEPHONE_orig : "", 1);
  setenv("LC_MEASUREMENT", LC_MEASUREMENT_orig ? LC_MEASUREMENT_orig : "", 1);
  setenv("LC_IDENTIFICATION", 
         LC_IDENTIFICATION_orig ? LC_IDENTIFICATION_orig : "", 1);
#endif

#endif
}

int main()
{
  test04();
  return 0;
}
