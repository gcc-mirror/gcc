// Implementation file for Exception Support for -*- C++ -*-
// This file is part of the GNU ANSI C++ Library.

#ifdef __GNUG__
#pragma implementation "stdexcept"
#endif

#include <stdexcept>

// Entry points for string.

void
__out_of_range (const char *s)
{
  throw out_of_range (s);
}

void __length_error (const char *s)
{
  throw length_error (s);
}
