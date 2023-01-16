// { dg-do run { target c++11 } }

// 2014-04-24 Rüdiger Sonderfeld

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

// [locale.codecvt], C++11 22.4.1.4.  specialization.

#include <locale>
#include <cstring>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;
  typedef codecvt<char32_t, char, mbstate_t> codecvt_c32;
  locale loc_c = locale::classic();
  VERIFY(has_facet<codecvt_c32>(loc_c));
  const codecvt_c32* const cvt = &use_facet<codecvt_c32>(loc_c);

  VERIFY(!cvt->always_noconv());
  VERIFY(cvt->max_length() == 4);
  VERIFY(cvt->encoding() == 0);

#ifndef _GLIBCXX_USE_CHAR8_T
  using char8_t = char;
#endif
  const char8_t u8dat_[] = u8"H\U000000E4ll\U000000F6 \U0001F63F \U000056FD "
    u8"\U0000222B f(\U000003BA) exp(-2\U000003C0\U000003C9) d\U000003BA "
    u8"\U0001F6BF \U0001F6BF \U0001F648 \U00000413\U00000435\U0000043E"
    u8"\U00000433\U00000440\U00000430\U00000444\U00000438\U0000044F \U0000FB05";
  const char* const u8dat = (const char*)u8dat_;
  const char* const u8dat_end = (const char*)std::end(u8dat_);

  const char32_t u32dat[] = U"H\U000000E4ll\U000000F6 \U0001F63F \U000056FD "
    U"\U0000222B f(\U000003BA) exp(-2\U000003C0\U000003C9) d\U000003BA "
    U"\U0001F6BF \U0001F6BF \U0001F648 \U00000413\U00000435\U0000043E"
    U"\U00000433\U00000440\U00000430\U00000444\U00000438\U0000044F \U0000FB05";
  const char32_t* const u32dat_end = std::end(u32dat);

  {
    const size_t len = u32dat_end - u32dat + 1;
    char32_t* const buffer = new char32_t[len];
    char32_t* const buffer_end = buffer + len;

    const char* from_next;
    char32_t* to_next;

    codecvt_c32::state_type state01;
    state01 = {};
    codecvt_base::result res = cvt->in(state01, u8dat, u8dat_end, from_next,
                                       buffer, buffer_end, to_next);

    VERIFY(res == codecvt_base::ok);
    VERIFY(from_next == u8dat_end);
    VERIFY(std::memcmp((void*)buffer, (void*)u32dat, sizeof(u32dat)) == 0);

    delete[] buffer;
  }

  {
    const size_t len = u8dat_end - u8dat + 1;
    char* const buffer = new char[len];
    char* const buffer_end = buffer + len;

    const char32_t* from_next;
    char* to_next;

    codecvt_c32::state_type state01;
    state01 = {};
    codecvt_base::result res = cvt->out(state01, u32dat, u32dat_end, from_next,
                                        buffer, buffer_end, to_next);

    VERIFY(res == codecvt_base::ok);
    VERIFY(from_next == u32dat_end);
    VERIFY(std::memcmp((void*)buffer, (void*)u8dat_, sizeof(u8dat_)) == 0);

    delete[] buffer;
  }
}

int
main()
{
  test01();
}
