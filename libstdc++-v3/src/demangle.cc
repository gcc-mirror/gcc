// C++ IA64 / g++ v3 demangler  -*- C++ -*-

// Copyright (C) 2003 Free Software Foundation, Inc.
// Written by Carlo Wood <carlo@alinoe.com>
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cxxabi.h>
#include "demangle.h"

// __cxa_demangle
//
// Demangle a C++ symbol or type name.
//
// `mangled-name' is a pointer to a null-terminated array of characters.
// It may be either an external name, i.e. with a "_Z" prefix, or an
// internal NTBS mangling, e.g. of a type for type_info.
//
// `buf' may be null.  If it is non-null, then n must also be non-null,
// and buf is a pointer to an array, of at least *n characters, that
// was allocated using malloc.
//
// `status' points to an int that is used as an error indicator. It is
// permitted to be null, in which case the user just doesn't get any
// detailed error information. 
//
// Returns: a pointer to a null-terminated array of characters, the
//          demangled name.  Or NULL in case of failure.
//
// If there is an error in demangling, the return value is a null pointer.
// The user can examine *status to find out what kind of error occurred.
// Meaning of error indications:
//
//     *  0: success
//     * -1: memory allocation failure
//     * -2: invalid mangled name
//     * -3: invalid arguments (e.g. buf nonnull and n null) 
//

namespace __cxxabiv1 
{
  namespace 
  {
    char* const error = 0;

    enum status_codes 
    {
      success = 0,
      memory_allocation_failure = -1,
      invalid_mangled_name = -2,
      invalid_argument = -3
    };

    inline char*
    failure(status_codes error_code, int* status)
    {
      if (status)
	*status = error_code;
      return error;
    }

    char*
    finish(char const* demangled_name, size_t demangled_name_size,
	   char* buf, size_t* n, int* status)
    {
      if (!buf || *n < demangled_name_size + 1)
      {
	if (n)
	  *n = demangled_name_size + 1;
	buf = (char*)realloc(buf, demangled_name_size + 1);
	if (!buf)
	  return failure(memory_allocation_failure, status);
      }
      if (status)
	*status = success;
      std::strncpy(buf, demangled_name, demangled_name_size);
      buf[demangled_name_size] = 0;
      return buf;
    }
  } // namespace

  char*
  __cxa_demangle(char const* mangled_name, char* buf, std::size_t* n, 
		 int* status)
  {
    using namespace __gnu_cxx;
    typedef demangler::session<std::allocator<char> > session_type;

    if (!mangled_name || (buf && !n))
      return failure(invalid_argument, status);

    std::string result;
    if (mangled_name[0] == '_')		
    {
      // External name?
      if (mangled_name[1] == 'Z')		
      {
	// C++ name?
	int cnt = session_type::
	    decode_encoding(result, mangled_name + 2, INT_MAX);
	if (cnt < 0 || mangled_name[cnt + 2] != 0)
	  return failure(invalid_mangled_name, status);
	return finish(result.data(), result.size(), buf, n, status);
      }
      else if (mangled_name[1] == 'G')	
      {
	// Possible _GLOBAL__ extension?
	if (!std::strncmp(mangled_name, "_GLOBAL__", 9) 
	    && (mangled_name[9] == 'D' || mangled_name[9] == 'I')
	    && mangled_name[10] == '_' && mangled_name[11] == '_' 
	    && mangled_name[12] == 'Z')
	{
	  if (mangled_name[9] == 'D')
	    result.assign("global destructors keyed to ", 28);
	  else
	    result.assign("global constructors keyed to ", 29);
	  int cnt = session_type::
	      decode_encoding(result, mangled_name + 13, INT_MAX);
	  if (cnt < 0 || mangled_name[cnt + 13] != 0)
	    return failure(invalid_mangled_name, status);
	  return finish(result.data(), result.size(), buf, n, status);
	}
      }
    }

    // Ambiguities are possible between extern "C" object names and
    // internal built-in type names, e.g. "i" may be either an object
    // named "i" or the built-in "int" type.  Such ambiguities should
    // be resolved to user names over built-in names.  Builtin types
    // are any single lower case character.  Any other single
    // character is not a mangled type so we can treat those the same
    // here.
    if (mangled_name[1] == 0)
      return finish(mangled_name, 1, buf, n, status);

    // Not a built-in type or external name, try to demangle input as
    // NTBS mangled type name.
    session_type demangler_session(mangled_name, INT_MAX);
    if (!demangler_session.decode_type(result) 
	|| demangler_session.remaining_input_characters())
    {
      // Failure to demangle, assume extern "C" name.
      result = mangled_name;		
    }
    return finish(result.data(), result.size(), buf, n, status);
  }
} // namespace __cxxabiv1
