/* Find the correct compiler.
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <string>
#include <dirent.h>
#include <stdlib.h>

#include "libiberty.h"
#include "xregex.h"
#include "findcomp.hh"
#include "system.h"

class scanner
{
public:

  scanner (const std::string &dir)
  {
    m_dir = opendir (dir.c_str ());
  }

  ~scanner ()
  {
    if (m_dir != NULL)
      closedir (m_dir);
  }

  const char *next ()
  {
    if (m_dir == NULL)
      return NULL;

    struct dirent *entry = readdir (m_dir);
    if (entry == NULL)
      return NULL;

    return entry->d_name;
  }

private:

  DIR *m_dir;
};

static bool
search_dir (const regex_t &regexp, const std::string &dir, std::string *result)
{
  scanner scan (dir);
  const char *filename;

  while ((filename = scan.next ()) != NULL)
    {
      if (regexec (&regexp, filename, 0, NULL, 0) == 0)
	{
	  *result = dir + DIR_SEPARATOR + filename;
	  return true;
	}
    }

  return false;
}

class tokenizer
{
public:

  tokenizer (const char *str)
    : m_str (str),
      m_pos (0)
  {
  }

  bool done () const
  {
    return m_pos == std::string::npos;
  }

  std::string next ()
  {
    std::string::size_type last_pos = m_pos;
    std::string::size_type colon = m_str.find(':', last_pos);

    std::string result;
    if (colon == std::string::npos)
      {
	m_pos = colon;
	result = m_str.substr(last_pos, colon);
      }
    else
      {
	m_pos = colon + 1;
	result = m_str.substr(last_pos, colon - last_pos);
      }
    if (result == "")
      result = ".";

    return result;
  }

private:

  std::string m_str;
  std::string::size_type m_pos;
};

bool
find_compiler (const regex_t &regexp, std::string *result)
{
  const char *cpath = getenv ("PATH");

  if (cpath == NULL)
    return false;

  tokenizer dirs (cpath);
  while (!dirs.done ())
    {
      std::string dir = dirs.next ();
      if (search_dir (regexp, dir, result))
	return true;
    }

  return false;
}
