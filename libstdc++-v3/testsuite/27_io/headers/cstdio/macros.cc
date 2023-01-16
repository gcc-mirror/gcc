// { dg-do compile }

// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

namespace gnu
{
#ifndef NULL
    #error "NULL_must_be_a_macro"
#endif

#ifndef BUFSIZ
    #error "BUFSIZ_must_be_a_macro"
#endif

#ifndef EOF
    #error "EOF_must_be_a_macro"
#endif

#ifndef FILENAME_MAX
    #error "FILENAME_MAX_must_be_a_macro"
#endif

#ifndef L_tmpnam
    #error "L_tmpnam_must_be_a_macro"
#endif

#ifndef FOPEN_MAX
    #error "FOPEN_MAX_must_be_a_macro"
#endif

#ifndef SEEK_CUR
    #error "SEEK_CUR_must_be_a_macro"
#endif

#ifndef SEEK_END
    #error "SEEK_END_must_be_a_macro"
#endif

#ifndef SEEK_SET
    #error "SEEK_SET_must_be_a_macro"
#endif

#ifndef TMP_MAX
    #error "TMP_MAX_must_be_a_macro"
#endif

#ifndef _IOFBF
    #error "_IOFBF_must_be_a_macro"
#endif

#ifndef _IOLBF
    #error "_IOLBF_must_be_a_macro"
#endif

#ifndef _IONBF
    #error "_IONBF_must_be_a_macro"
#endif

#ifndef stderr
    #error "stderr_must_be_a_macro"
#endif

#ifndef stdin
    #error "stdin_must_be_a_macro"
#endif

#ifndef stdout
    #error "stdout_must_be_a_macro"
#endif
}
