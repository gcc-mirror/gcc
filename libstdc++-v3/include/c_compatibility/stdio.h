// -*- C++ -*- compatibility header.

// Copyright (C) 2002-2020 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file stdio.h
 *  This is a Standard C++ Library header.
 */

#include <cstdio>

#ifndef _GLIBCXX_STDIO_H
#define _GLIBCXX_STDIO_H 1

#ifdef _GLIBCXX_NAMESPACE_C
using std::FILE;
using std::fpos_t;

using std::remove;
using std::rename;
using std::tmpfile;
using std::tmpnam;
using std::fclose;
using std::fflush;
using std::fopen;
using std::freopen;
using std::setbuf;
using std::setvbuf;
using std::fprintf;
using std::fscanf;
using std::printf;
using std::scanf;
using std::snprintf;
using std::sprintf;
using std::sscanf;
using std::vfprintf;
using std::vfscanf;
using std::vprintf;
using std::vscanf;
using std::vsnprintf;
using std::vsprintf;
using std::vsscanf;
using std::fgetc;
using std::fgets;
using std::fputc;
using std::fputs;
using std::getc;
using std::getchar;
using std::gets;
using std::putc;
using std::putchar;
using std::puts;
using std::ungetc;
using std::fread;
using std::fwrite;
using std::fgetpos;
using std::fseek;
using std::fsetpos;
using std::ftell;
using std::rewind;
using std::clearerr;
using std::feof;
using std::ferror;
using std::perror;
#endif

#endif
