// Copyright (C) 2003 Free Software Foundation, Inc.
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

#include <cstdio>
#include <cstring>
#include <fstream>
#include <langinfo.h>
#include <iconv.h>
#include <testsuite_performance.h>

// libstdc++/11602 (do_in)
int main(int argc, char** argv)
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  const int iters = 400000;

  wchar_t wbuf[1024];
  char cbuf[1024];
  
  memset(cbuf, 'a', 1024);

  // C (iconv)
  iconv_t cd = iconv_open("WCHAR_T", nl_langinfo(CODESET));
  start_counters(time, resource);
  for (int i = 0; i < iters; ++i)
    {
      size_t inbytesleft = 1024;
      size_t outbytesleft = 1024 * sizeof(wchar_t);
      char* in = cbuf;
      char* out = reinterpret_cast<char*>(wbuf);
      iconv(cd, &in, &inbytesleft, &out, &outbytesleft);
    }
  stop_counters(time, resource);
  iconv_close(cd);
  report_performance(__FILE__, "C (iconv)", time, resource);
  clear_counters(time, resource);

  // C++ (codecvt)
  locale loc;
  const codecvt<wchar_t, char, mbstate_t>& cvt =
    use_facet<codecvt<wchar_t, char, mbstate_t> >(loc);
  mbstate_t state;
  memset(&state, 0, sizeof(state));
  start_counters(time, resource);
  for (int i = 0; i < iters; ++i)
    {
      const char* from_next;
      wchar_t* to_next;
      cvt.in(state, cbuf, cbuf + 1024, from_next,
	     wbuf, wbuf + 1024, to_next);
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "C++ (codecvt)", time, resource);

  return 0;
}
