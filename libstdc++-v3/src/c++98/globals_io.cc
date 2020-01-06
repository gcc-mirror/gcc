// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

#include "bits/c++config.h"
#include <fstream>
#include <istream>
#include <ostream>
#include <ext/stdio_filebuf.h>
#include <ext/stdio_sync_filebuf.h>

// On AIX, and perhaps other systems, library initialization order is
// not guaranteed.  For example, the static initializers for the main
// program might run before the static initializers for this library.
// That means that we cannot rely on static initialization in the
// library; there is no guarantee that things will get initialized in
// time.  This file contains definitions of all global variables that
// require initialization as arrays of characters.

// NB: asm directives can rename these non-exported, namespace
// __gnu_cxx symbols into exported, namespace std symbols with the
// appropriate symbol version name.
// The rename syntax is
//   asm (".symver currentname,oldname@@GLIBCXX_3.2")
// In macro form:
// _GLIBCXX_ASM_SYMVER(currentname, oldname, GLIBCXX_3.2)

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Standard stream objects.
  // NB: Iff <iostream> is included, these definitions become wonky.
  typedef char fake_istream[sizeof(istream)]
  __attribute__ ((aligned(__alignof__(istream))));
  typedef char fake_ostream[sizeof(ostream)]
  __attribute__ ((aligned(__alignof__(ostream))));
  fake_istream cin;
  fake_ostream cout;
  fake_ostream cerr;
  fake_ostream clog;

#ifdef _GLIBCXX_USE_WCHAR_T
  typedef char fake_wistream[sizeof(wistream)]
  __attribute__ ((aligned(__alignof__(wistream))));
  typedef char fake_wostream[sizeof(wostream)]
  __attribute__ ((aligned(__alignof__(wostream))));
  fake_wistream wcin;
  fake_wostream wcout;
  fake_wostream wcerr;
  fake_wostream wclog;
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

namespace __gnu_internal _GLIBCXX_VISIBILITY(hidden)
{
  using namespace std;
  using namespace __gnu_cxx;

  // We use different stream buffer types depending on whether
  // ios_base::sync_with_stdio(false) has been called.
  typedef char fake_stdiobuf[sizeof(stdio_sync_filebuf<char>)]
  __attribute__ ((aligned(__alignof__(stdio_sync_filebuf<char>))));
  fake_stdiobuf buf_cout_sync;
  fake_stdiobuf buf_cin_sync;
  fake_stdiobuf buf_cerr_sync;

  typedef char fake_filebuf[sizeof(stdio_filebuf<char>)]
  __attribute__ ((aligned(__alignof__(stdio_filebuf<char>))));
  fake_filebuf buf_cout;
  fake_filebuf buf_cin;
  fake_filebuf buf_cerr;

#ifdef _GLIBCXX_USE_WCHAR_T
  typedef char fake_wstdiobuf[sizeof(stdio_sync_filebuf<wchar_t>)]
  __attribute__ ((aligned(__alignof__(stdio_sync_filebuf<wchar_t>))));
  fake_wstdiobuf buf_wcout_sync;
  fake_wstdiobuf buf_wcin_sync;
  fake_wstdiobuf buf_wcerr_sync;

  typedef char fake_wfilebuf[sizeof(stdio_filebuf<wchar_t>)]
  __attribute__ ((aligned(__alignof__(stdio_filebuf<wchar_t>))));
  fake_wfilebuf buf_wcout;
  fake_wfilebuf buf_wcin;
  fake_wfilebuf buf_wcerr;
#endif
} // namespace __gnu_internal
