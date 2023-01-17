// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include <algorithm>
#include <locale>
#include <string>
#include <testsuite_hooks.h>

struct test_offsets_ok
{
  size_t in_size, out_size;
};
struct test_offsets_partial
{
  size_t in_size, out_size, expected_in_next, expected_out_next;
};

template <class CharT> struct test_offsets_error
{
  size_t in_size, out_size, expected_in_next, expected_out_next;
  CharT replace_char;
  size_t replace_pos;
};

template <class T, size_t N>
auto constexpr array_size (const T (&)[N]) -> size_t
{
  return N;
}

template <class CharT>
void
utf8_to_utf32_in_ok (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char in[] = "bш\uAAAA\U0010AAAA";
  const char32_t exp_literal[] = U"bш\uAAAA\U0010AAAA";
  CharT exp[array_size (exp_literal)] = {};
  std::copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (in) == 11, "");
  static_assert (array_size (exp_literal) == 5, "");
  static_assert (array_size (exp) == 5, "");
  VERIFY (char_traits<char>::length (in) == 10);
  VERIFY (char_traits<char32_t>::length (exp_literal) == 4);
  VERIFY (char_traits<CharT>::length (exp) == 4);

  test_offsets_ok offsets[] = {{0, 0}, {1, 1}, {3, 2}, {6, 3}, {10, 4}};
  for (auto t : offsets)
    {
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<CharT>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }

  for (auto t : offsets)
    {
      CharT out[array_size (exp)] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res
	= cvt.in (state, in, in + t.in_size, in_next, out, end (out), out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<CharT>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }
}

template <class CharT>
void
utf8_to_utf32_in_partial (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char in[] = "bш\uAAAA\U0010AAAA";
  const char32_t exp_literal[] = U"bш\uAAAA\U0010AAAA";
  CharT exp[array_size (exp_literal)] = {};
  std::copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (in) == 11, "");
  static_assert (array_size (exp_literal) == 5, "");
  static_assert (array_size (exp) == 5, "");
  VERIFY (char_traits<char>::length (in) == 10);
  VERIFY (char_traits<char32_t>::length (exp_literal) == 4);
  VERIFY (char_traits<CharT>::length (exp) == 4);

  test_offsets_partial offsets[] = {
    {1, 0, 0, 0}, // no space for first CP

    {3, 1, 1, 1}, // no space for second CP
    {2, 2, 1, 1}, // incomplete second CP
    {2, 1, 1, 1}, // incomplete second CP, and no space for it

    {6, 2, 3, 2}, // no space for third CP
    {4, 3, 3, 2}, // incomplete third CP
    {5, 3, 3, 2}, // incomplete third CP
    {4, 2, 3, 2}, // incomplete third CP, and no space for it
    {5, 2, 3, 2}, // incomplete third CP, and no space for it

    {10, 3, 6, 3}, // no space for fourth CP
    {7, 4, 6, 3},  // incomplete fourth CP
    {8, 4, 6, 3},  // incomplete fourth CP
    {9, 4, 6, 3},  // incomplete fourth CP
    {7, 3, 6, 3},  // incomplete fourth CP, and no space for it
    {8, 3, 6, 3},  // incomplete fourth CP, and no space for it
    {9, 3, 6, 3},  // incomplete fourth CP, and no space for it
  };

  for (auto t : offsets)
    {
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.partial);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<CharT>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf8_to_utf32_in_error (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char valid_in[] = "bш\uAAAA\U0010AAAA";
  const char32_t exp_literal[] = U"bш\uAAAA\U0010AAAA";
  CharT exp[array_size (exp_literal)] = {};
  std::copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (valid_in) == 11, "");
  static_assert (array_size (exp_literal) == 5, "");
  static_assert (array_size (exp) == 5, "");
  VERIFY (char_traits<char>::length (valid_in) == 10);
  VERIFY (char_traits<char32_t>::length (exp_literal) == 4);
  VERIFY (char_traits<CharT>::length (exp) == 4);

  test_offsets_error<char> offsets[] = {

    // replace leading byte with invalid byte
    {1, 4, 0, 0, '\xFF', 0},
    {3, 4, 1, 1, '\xFF', 1},
    {6, 4, 3, 2, '\xFF', 3},
    {10, 4, 6, 3, '\xFF', 6},

    // replace first trailing byte with ASCII byte
    {3, 4, 1, 1, 'z', 2},
    {6, 4, 3, 2, 'z', 4},
    {10, 4, 6, 3, 'z', 7},

    // replace first trailing byte with invalid byte
    {3, 4, 1, 1, '\xFF', 2},
    {6, 4, 3, 2, '\xFF', 4},
    {10, 4, 6, 3, '\xFF', 7},

    // replace second trailing byte with ASCII byte
    {6, 4, 3, 2, 'z', 5},
    {10, 4, 6, 3, 'z', 8},

    // replace second trailing byte with invalid byte
    {6, 4, 3, 2, '\xFF', 5},
    {10, 4, 6, 3, '\xFF', 8},

    // replace third trailing byte
    {10, 4, 6, 3, 'z', 9},
    {10, 4, 6, 3, '\xFF', 9},

    // replace first trailing byte with ASCII byte, also incomplete at end
    {5, 4, 3, 2, 'z', 4},
    {8, 4, 6, 3, 'z', 7},
    {9, 4, 6, 3, 'z', 7},

    // replace first trailing byte with invalid byte, also incomplete at end
    {5, 4, 3, 2, '\xFF', 4},
    {8, 4, 6, 3, '\xFF', 7},
    {9, 4, 6, 3, '\xFF', 7},

    // replace second trailing byte with ASCII byte, also incomplete at end
    {9, 4, 6, 3, 'z', 8},

    // replace second trailing byte with invalid byte, also incomplete at end
    {9, 4, 6, 3, '\xFF', 8},
  };
  for (auto t : offsets)
    {
      char in[array_size (valid_in)] = {};
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      char_traits<char>::copy (in, valid_in, array_size (valid_in));
      in[t.replace_pos] = t.replace_char;

      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.error);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<CharT>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf8_to_utf32_in (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf8_to_utf32_in_ok (cvt);
  utf8_to_utf32_in_partial (cvt);
  utf8_to_utf32_in_error (cvt);
}

template <class CharT>
void
utf32_to_utf8_out_ok (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char32_t in_literal[] = U"bш\uAAAA\U0010AAAA";
  const char exp[] = "bш\uAAAA\U0010AAAA";
  CharT in[array_size (in_literal)] = {};
  copy (begin (in_literal), end (in_literal), begin (in));

  static_assert (array_size (in_literal) == 5, "");
  static_assert (array_size (in) == 5, "");
  static_assert (array_size (exp) == 11, "");
  VERIFY (char_traits<char32_t>::length (in_literal) == 4);
  VERIFY (char_traits<CharT>::length (in) == 4);
  VERIFY (char_traits<char>::length (exp) == 10);

  const test_offsets_ok offsets[] = {{0, 0}, {1, 1}, {2, 3}, {3, 6}, {4, 10}};
  for (auto t : offsets)
    {
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<char>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }
}

template <class CharT>
void
utf32_to_utf8_out_partial (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char32_t in_literal[] = U"bш\uAAAA\U0010AAAA";
  const char exp[] = "bш\uAAAA\U0010AAAA";
  CharT in[array_size (in_literal)] = {};
  copy (begin (in_literal), end (in_literal), begin (in));

  static_assert (array_size (in_literal) == 5, "");
  static_assert (array_size (in) == 5, "");
  static_assert (array_size (exp) == 11, "");
  VERIFY (char_traits<char32_t>::length (in_literal) == 4);
  VERIFY (char_traits<CharT>::length (in) == 4);
  VERIFY (char_traits<char>::length (exp) == 10);

  const test_offsets_partial offsets[] = {
    {1, 0, 0, 0}, // no space for first CP

    {2, 1, 1, 1}, // no space for second CP
    {2, 2, 1, 1}, // no space for second CP

    {3, 3, 2, 3}, // no space for third CP
    {3, 4, 2, 3}, // no space for third CP
    {3, 5, 2, 3}, // no space for third CP

    {4, 6, 3, 6}, // no space for fourth CP
    {4, 7, 3, 6}, // no space for fourth CP
    {4, 8, 3, 6}, // no space for fourth CP
    {4, 9, 3, 6}, // no space for fourth CP
  };
  for (auto t : offsets)
    {
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.partial);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<char>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf32_to_utf8_out_error (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  const char32_t valid_in[] = U"bш\uAAAA\U0010AAAA";
  const char exp[] = "bш\uAAAA\U0010AAAA";

  static_assert (array_size (valid_in) == 5, "");
  static_assert (array_size (exp) == 11, "");
  VERIFY (char_traits<char32_t>::length (valid_in) == 4);
  VERIFY (char_traits<char>::length (exp) == 10);

  test_offsets_error<CharT> offsets[] = {{4, 10, 0, 0, 0x00110000, 0},
					 {4, 10, 1, 1, 0x00110000, 1},
					 {4, 10, 2, 3, 0x00110000, 2},
					 {4, 10, 3, 6, 0x00110000, 3}};

  for (auto t : offsets)
    {
      CharT in[array_size (valid_in)] = {};
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      copy (begin (valid_in), end (valid_in), begin (in));
      in[t.replace_pos] = t.replace_char;

      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.error);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<char>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf32_to_utf8_out (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf32_to_utf8_out_ok (cvt);
  utf32_to_utf8_out_partial (cvt);
  utf32_to_utf8_out_error (cvt);
}

template <class CharT>
void
test_utf8_utf32_codecvts (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf8_to_utf32_in (cvt);
  utf32_to_utf8_out (cvt);
}

template <class CharT>
void
utf8_to_utf16_in_ok (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char in[] = "bш\uAAAA\U0010AAAA";
  const char16_t exp_literal[] = u"bш\uAAAA\U0010AAAA";
  CharT exp[array_size (exp_literal)] = {};
  copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (in) == 11, "");
  static_assert (array_size (exp_literal) == 6, "");
  static_assert (array_size (exp) == 6, "");
  VERIFY (char_traits<char>::length (in) == 10);
  VERIFY (char_traits<char16_t>::length (exp_literal) == 5);
  VERIFY (char_traits<CharT>::length (exp) == 5);

  test_offsets_ok offsets[] = {{0, 0}, {1, 1}, {3, 2}, {6, 3}, {10, 5}};
  for (auto t : offsets)
    {
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<CharT>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }

  for (auto t : offsets)
    {
      CharT out[array_size (exp)] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res
	= cvt.in (state, in, in + t.in_size, in_next, out, end (out), out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<CharT>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }
}

template <class CharT>
void
utf8_to_utf16_in_partial (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char in[] = "bш\uAAAA\U0010AAAA";
  const char16_t exp_literal[] = u"bш\uAAAA\U0010AAAA";
  CharT exp[array_size (exp_literal)] = {};
  copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (in) == 11, "");
  static_assert (array_size (exp_literal) == 6, "");
  static_assert (array_size (exp) == 6, "");
  VERIFY (char_traits<char>::length (in) == 10);
  VERIFY (char_traits<char16_t>::length (exp_literal) == 5);
  VERIFY (char_traits<CharT>::length (exp) == 5);

  test_offsets_partial offsets[] = {
    {1, 0, 0, 0}, // no space for first CP

    {3, 1, 1, 1}, // no space for second CP
    {2, 2, 1, 1}, // incomplete second CP
    {2, 1, 1, 1}, // incomplete second CP, and no space for it

    {6, 2, 3, 2}, // no space for third CP
    {4, 3, 3, 2}, // incomplete third CP
    {5, 3, 3, 2}, // incomplete third CP
    {4, 2, 3, 2}, // incomplete third CP, and no space for it
    {5, 2, 3, 2}, // incomplete third CP, and no space for it

    {10, 3, 6, 3}, // no space for fourth CP
    {10, 4, 6, 3}, // no space for fourth CP
    {7, 5, 6, 3},  // incomplete fourth CP
    {8, 5, 6, 3},  // incomplete fourth CP
    {9, 5, 6, 3},  // incomplete fourth CP
    {7, 3, 6, 3},  // incomplete fourth CP, and no space for it
    {8, 3, 6, 3},  // incomplete fourth CP, and no space for it
    {9, 3, 6, 3},  // incomplete fourth CP, and no space for it
    {7, 4, 6, 3},  // incomplete fourth CP, and no space for it
    {8, 4, 6, 3},  // incomplete fourth CP, and no space for it
    {9, 4, 6, 3},  // incomplete fourth CP, and no space for it

  };

  for (auto t : offsets)
    {
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.partial);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<CharT>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf8_to_utf16_in_error (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  const char valid_in[] = "bш\uAAAA\U0010AAAA";
  const char16_t exp_literal[] = u"bш\uAAAA\U0010AAAA";
  CharT exp[array_size (exp_literal)] = {};
  copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (valid_in) == 11, "");
  static_assert (array_size (exp_literal) == 6, "");
  static_assert (array_size (exp) == 6, "");
  VERIFY (char_traits<char>::length (valid_in) == 10);
  VERIFY (char_traits<char16_t>::length (exp_literal) == 5);
  VERIFY (char_traits<CharT>::length (exp) == 5);

  test_offsets_error<char> offsets[] = {

    // replace leading byte with invalid byte
    {1, 5, 0, 0, '\xFF', 0},
    {3, 5, 1, 1, '\xFF', 1},
    {6, 5, 3, 2, '\xFF', 3},
    {10, 5, 6, 3, '\xFF', 6},

    // replace first trailing byte with ASCII byte
    {3, 5, 1, 1, 'z', 2},
    {6, 5, 3, 2, 'z', 4},
    {10, 5, 6, 3, 'z', 7},

    // replace first trailing byte with invalid byte
    {3, 5, 1, 1, '\xFF', 2},
    {6, 5, 3, 2, '\xFF', 4},
    {10, 5, 6, 3, '\xFF', 7},

    // replace second trailing byte with ASCII byte
    {6, 5, 3, 2, 'z', 5},
    {10, 5, 6, 3, 'z', 8},

    // replace second trailing byte with invalid byte
    {6, 5, 3, 2, '\xFF', 5},
    {10, 5, 6, 3, '\xFF', 8},

    // replace third trailing byte
    {10, 5, 6, 3, 'z', 9},
    {10, 5, 6, 3, '\xFF', 9},

    // replace first trailing byte with ASCII byte, also incomplete at end
    {5, 5, 3, 2, 'z', 4},
    {8, 5, 6, 3, 'z', 7},
    {9, 5, 6, 3, 'z', 7},

    // replace first trailing byte with invalid byte, also incomplete at end
    {5, 5, 3, 2, '\xFF', 4},
    {8, 5, 6, 3, '\xFF', 7},
    {9, 5, 6, 3, '\xFF', 7},

    // replace second trailing byte with ASCII byte, also incomplete at end
    {9, 5, 6, 3, 'z', 8},

    // replace second trailing byte with invalid byte, also incomplete at end
    {9, 5, 6, 3, '\xFF', 8},
  };
  for (auto t : offsets)
    {
      char in[array_size (valid_in)] = {};
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      char_traits<char>::copy (in, valid_in, array_size (valid_in));
      in[t.replace_pos] = t.replace_char;

      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.error);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<CharT>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf8_to_utf16_in (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf8_to_utf16_in_ok (cvt);
  utf8_to_utf16_in_partial (cvt);
  utf8_to_utf16_in_error (cvt);
}

template <class CharT>
void
utf16_to_utf8_out_ok (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char16_t in_literal[] = u"bш\uAAAA\U0010AAAA";
  const char exp[] = "bш\uAAAA\U0010AAAA";
  CharT in[array_size (in_literal)];
  copy (begin (in_literal), end (in_literal), begin (in));

  static_assert (array_size (in_literal) == 6, "");
  static_assert (array_size (exp) == 11, "");
  static_assert (array_size (in) == 6, "");
  VERIFY (char_traits<char16_t>::length (in_literal) == 5);
  VERIFY (char_traits<char>::length (exp) == 10);
  VERIFY (char_traits<CharT>::length (in) == 5);

  const test_offsets_ok offsets[] = {{0, 0}, {1, 1}, {2, 3}, {3, 6}, {5, 10}};
  for (auto t : offsets)
    {
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<char>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }
}

template <class CharT>
void
utf16_to_utf8_out_partial (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP, 3-byte CP and 4-byte CP
  const char16_t in_literal[] = u"bш\uAAAA\U0010AAAA";
  const char exp[] = "bш\uAAAA\U0010AAAA";
  CharT in[array_size (in_literal)];
  copy (begin (in_literal), end (in_literal), begin (in));

  static_assert (array_size (in_literal) == 6, "");
  static_assert (array_size (exp) == 11, "");
  static_assert (array_size (in) == 6, "");
  VERIFY (char_traits<char16_t>::length (in_literal) == 5);
  VERIFY (char_traits<char>::length (exp) == 10);
  VERIFY (char_traits<CharT>::length (in) == 5);

  const test_offsets_partial offsets[] = {
    {1, 0, 0, 0}, // no space for first CP

    {2, 1, 1, 1}, // no space for second CP
    {2, 2, 1, 1}, // no space for second CP

    {3, 3, 2, 3}, // no space for third CP
    {3, 4, 2, 3}, // no space for third CP
    {3, 5, 2, 3}, // no space for third CP

    {5, 6, 3, 6}, // no space for fourth CP
    {5, 7, 3, 6}, // no space for fourth CP
    {5, 8, 3, 6}, // no space for fourth CP
    {5, 9, 3, 6}, // no space for fourth CP

    {4, 10, 3, 6}, // incomplete fourth CP

    {4, 6, 3, 6}, // incomplete fourth CP, and no space for it
    {4, 7, 3, 6}, // incomplete fourth CP, and no space for it
    {4, 8, 3, 6}, // incomplete fourth CP, and no space for it
    {4, 9, 3, 6}, // incomplete fourth CP, and no space for it
  };
  for (auto t : offsets)
    {
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.partial);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<char>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf16_to_utf8_out_error (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  const char16_t valid_in[] = u"bш\uAAAA\U0010AAAA";
  const char exp[] = "bш\uAAAA\U0010AAAA";

  static_assert (array_size (valid_in) == 6, "");
  static_assert (array_size (exp) == 11, "");
  VERIFY (char_traits<char16_t>::length (valid_in) == 5);
  VERIFY (char_traits<char>::length (exp) == 10);

  test_offsets_error<CharT> offsets[] = {
    {5, 10, 0, 0, 0xD800, 0},
    {5, 10, 0, 0, 0xDBFF, 0},
    {5, 10, 0, 0, 0xDC00, 0},
    {5, 10, 0, 0, 0xDFFF, 0},

    {5, 10, 1, 1, 0xD800, 1},
    {5, 10, 1, 1, 0xDBFF, 1},
    {5, 10, 1, 1, 0xDC00, 1},
    {5, 10, 1, 1, 0xDFFF, 1},

    {5, 10, 2, 3, 0xD800, 2},
    {5, 10, 2, 3, 0xDBFF, 2},
    {5, 10, 2, 3, 0xDC00, 2},
    {5, 10, 2, 3, 0xDFFF, 2},

    // make the leading surrogate a trailing one
    {5, 10, 3, 6, 0xDC00, 3},
    {5, 10, 3, 6, 0xDFFF, 3},

    // make the trailing surrogate a leading one
    {5, 10, 3, 6, 0xD800, 4},
    {5, 10, 3, 6, 0xDBFF, 4},

    // make the trailing surrogate a BMP char
    {5, 10, 3, 6, u'z', 4},
  };

  for (auto t : offsets)
    {
      CharT in[array_size (valid_in)] = {};
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      copy (begin (valid_in), end (valid_in), begin (in));
      in[t.replace_pos] = t.replace_char;

      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.error);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<char>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf16_to_utf8_out (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf16_to_utf8_out_ok (cvt);
  utf16_to_utf8_out_partial (cvt);
  utf16_to_utf8_out_error (cvt);
}

template <class CharT>
void
test_utf8_utf16_cvts (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf8_to_utf16_in (cvt);
  utf16_to_utf8_out (cvt);
}

template <class CharT>
void
utf8_to_ucs2_in_ok (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP and 3-byte CP
  const char in[] = "bш\uAAAA";
  const char16_t exp_literal[] = u"bш\uAAAA";
  CharT exp[array_size (exp_literal)] = {};
  copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (in) == 7, "");
  static_assert (array_size (exp_literal) == 4, "");
  static_assert (array_size (exp) == 4, "");
  VERIFY (char_traits<char>::length (in) == 6);
  VERIFY (char_traits<char16_t>::length (exp_literal) == 3);
  VERIFY (char_traits<CharT>::length (exp) == 3);

  test_offsets_ok offsets[] = {{0, 0}, {1, 1}, {3, 2}, {6, 3}};
  for (auto t : offsets)
    {
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<CharT>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }

  for (auto t : offsets)
    {
      CharT out[array_size (exp)] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res
	= cvt.in (state, in, in + t.in_size, in_next, out, end (out), out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<CharT>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }
}

template <class CharT>
void
utf8_to_ucs2_in_partial (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP and 3-byte CP
  const char in[] = "bш\uAAAA";
  const char16_t exp_literal[] = u"bш\uAAAA";
  CharT exp[array_size (exp_literal)] = {};
  copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (in) == 7, "");
  static_assert (array_size (exp_literal) == 4, "");
  static_assert (array_size (exp) == 4, "");
  VERIFY (char_traits<char>::length (in) == 6);
  VERIFY (char_traits<char16_t>::length (exp_literal) == 3);
  VERIFY (char_traits<CharT>::length (exp) == 3);

  test_offsets_partial offsets[] = {
    {1, 0, 0, 0}, // no space for first CP

    {3, 1, 1, 1}, // no space for second CP
    {2, 2, 1, 1}, // incomplete second CP
    {2, 1, 1, 1}, // incomplete second CP, and no space for it

    {6, 2, 3, 2}, // no space for third CP
    {4, 3, 3, 2}, // incomplete third CP
    {5, 3, 3, 2}, // incomplete third CP
    {4, 2, 3, 2}, // incomplete third CP, and no space for it
    {5, 2, 3, 2}, // incomplete third CP, and no space for it
  };

  for (auto t : offsets)
    {
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.partial);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<CharT>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf8_to_ucs2_in_error (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  const char valid_in[] = "bш\uAAAA\U0010AAAA";
  const char16_t exp_literal[] = u"bш\uAAAA\U0010AAAA";
  CharT exp[array_size (exp_literal)] = {};
  copy (begin (exp_literal), end (exp_literal), begin (exp));

  static_assert (array_size (valid_in) == 11, "");
  static_assert (array_size (exp_literal) == 6, "");
  static_assert (array_size (exp) == 6, "");
  VERIFY (char_traits<char>::length (valid_in) == 10);
  VERIFY (char_traits<char16_t>::length (exp_literal) == 5);
  VERIFY (char_traits<CharT>::length (exp) == 5);

  test_offsets_error<char> offsets[] = {

    // replace leading byte with invalid byte
    {1, 5, 0, 0, '\xFF', 0},
    {3, 5, 1, 1, '\xFF', 1},
    {6, 5, 3, 2, '\xFF', 3},
    {10, 5, 6, 3, '\xFF', 6},

    // replace first trailing byte with ASCII byte
    {3, 5, 1, 1, 'z', 2},
    {6, 5, 3, 2, 'z', 4},
    {10, 5, 6, 3, 'z', 7},

    // replace first trailing byte with invalid byte
    {3, 5, 1, 1, '\xFF', 2},
    {6, 5, 3, 2, '\xFF', 4},
    {10, 5, 6, 3, '\xFF', 7},

    // replace second trailing byte with ASCII byte
    {6, 5, 3, 2, 'z', 5},
    {10, 5, 6, 3, 'z', 8},

    // replace second trailing byte with invalid byte
    {6, 5, 3, 2, '\xFF', 5},
    {10, 5, 6, 3, '\xFF', 8},

    // replace third trailing byte
    {10, 5, 6, 3, 'z', 9},
    {10, 5, 6, 3, '\xFF', 9},

    // When we see a leading byte of 4-byte CP, we should return error, no
    // matter if it is incomplete at the end or has errors in the trailing
    // bytes.

    // Don't replace anything, show full 4-byte CP
    {10, 4, 6, 3, 'b', 0},
    {10, 5, 6, 3, 'b', 0},

    // Don't replace anything, show incomplete 4-byte CP at the end
    {7, 4, 6, 3, 'b', 0}, // incomplete fourth CP
    {8, 4, 6, 3, 'b', 0}, // incomplete fourth CP
    {9, 4, 6, 3, 'b', 0}, // incomplete fourth CP
    {7, 5, 6, 3, 'b', 0}, // incomplete fourth CP
    {8, 5, 6, 3, 'b', 0}, // incomplete fourth CP
    {9, 5, 6, 3, 'b', 0}, // incomplete fourth CP

    // replace first trailing byte with ASCII byte, also incomplete at end
    {5, 5, 3, 2, 'z', 4},

    // replace first trailing byte with invalid byte, also incomplete at end
    {5, 5, 3, 2, '\xFF', 4},

    // replace first trailing byte with ASCII byte, also incomplete at end
    {8, 5, 6, 3, 'z', 7},
    {9, 5, 6, 3, 'z', 7},

    // replace first trailing byte with invalid byte, also incomplete at end
    {8, 5, 6, 3, '\xFF', 7},
    {9, 5, 6, 3, '\xFF', 7},

    // replace second trailing byte with ASCII byte, also incomplete at end
    {9, 5, 6, 3, 'z', 8},

    // replace second trailing byte with invalid byte, also incomplete at end
    {9, 5, 6, 3, '\xFF', 8},
  };
  for (auto t : offsets)
    {
      char in[array_size (valid_in)] = {};
      CharT out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      char_traits<char>::copy (in, valid_in, array_size (valid_in));
      in[t.replace_pos] = t.replace_char;

      auto state = mbstate_t{};
      auto in_next = (const char *) nullptr;
      auto out_next = (CharT *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.in (state, in, in + t.in_size, in_next, out, out + t.out_size,
		    out_next);
      VERIFY (res == cvt.error);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<CharT>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
utf8_to_ucs2_in (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf8_to_ucs2_in_ok (cvt);
  utf8_to_ucs2_in_partial (cvt);
  utf8_to_ucs2_in_error (cvt);
}

template <class CharT>
void
ucs2_to_utf8_out_ok (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP and 3-byte CP
  const char16_t in_literal[] = u"bш\uAAAA";
  const char exp[] = "bш\uAAAA";
  CharT in[array_size (in_literal)] = {};
  copy (begin (in_literal), end (in_literal), begin (in));

  static_assert (array_size (in_literal) == 4, "");
  static_assert (array_size (exp) == 7, "");
  static_assert (array_size (in) == 4, "");
  VERIFY (char_traits<char16_t>::length (in_literal) == 3);
  VERIFY (char_traits<char>::length (exp) == 6);
  VERIFY (char_traits<CharT>::length (in) == 3);

  const test_offsets_ok offsets[] = {{0, 0}, {1, 1}, {2, 3}, {3, 6}};
  for (auto t : offsets)
    {
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.ok);
      VERIFY (in_next == in + t.in_size);
      VERIFY (out_next == out + t.out_size);
      VERIFY (char_traits<char>::compare (out, exp, t.out_size) == 0);
      if (t.out_size < array_size (out))
	VERIFY (out[t.out_size] == 0);
    }
}

template <class CharT>
void
ucs2_to_utf8_out_partial (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  // UTF-8 string of 1-byte CP, 2-byte CP and 3-byte CP
  const char16_t in_literal[] = u"bш\uAAAA";
  const char exp[] = "bш\uAAAA";
  CharT in[array_size (in_literal)] = {};
  copy (begin (in_literal), end (in_literal), begin (in));

  static_assert (array_size (in_literal) == 4, "");
  static_assert (array_size (exp) == 7, "");
  static_assert (array_size (in) == 4, "");
  VERIFY (char_traits<char16_t>::length (in_literal) == 3);
  VERIFY (char_traits<char>::length (exp) == 6);
  VERIFY (char_traits<CharT>::length (in) == 3);

  const test_offsets_partial offsets[] = {
    {1, 0, 0, 0}, // no space for first CP

    {2, 1, 1, 1}, // no space for second CP
    {2, 2, 1, 1}, // no space for second CP

    {3, 3, 2, 3}, // no space for third CP
    {3, 4, 2, 3}, // no space for third CP
    {3, 5, 2, 3}, // no space for third CP
  };
  for (auto t : offsets)
    {
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.partial);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<char>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
ucs2_to_utf8_out_error (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  using namespace std;
  const char16_t valid_in[] = u"bш\uAAAA\U0010AAAA";
  const char exp[] = "bш\uAAAA\U0010AAAA";

  static_assert (array_size (valid_in) == 6, "");
  static_assert (array_size (exp) == 11, "");
  VERIFY (char_traits<char16_t>::length (valid_in) == 5);
  VERIFY (char_traits<char>::length (exp) == 10);

  test_offsets_error<CharT> offsets[] = {
    {5, 10, 0, 0, 0xD800, 0},
    {5, 10, 0, 0, 0xDBFF, 0},
    {5, 10, 0, 0, 0xDC00, 0},
    {5, 10, 0, 0, 0xDFFF, 0},

    {5, 10, 1, 1, 0xD800, 1},
    {5, 10, 1, 1, 0xDBFF, 1},
    {5, 10, 1, 1, 0xDC00, 1},
    {5, 10, 1, 1, 0xDFFF, 1},

    {5, 10, 2, 3, 0xD800, 2},
    {5, 10, 2, 3, 0xDBFF, 2},
    {5, 10, 2, 3, 0xDC00, 2},
    {5, 10, 2, 3, 0xDFFF, 2},

    // dont replace anything, just show the surrogate pair
    {5, 10, 3, 6, u'b', 0},

    // make the leading surrogate a trailing one
    {5, 10, 3, 6, 0xDC00, 3},
    {5, 10, 3, 6, 0xDFFF, 3},

    // make the trailing surrogate a leading one
    {5, 10, 3, 6, 0xD800, 4},
    {5, 10, 3, 6, 0xDBFF, 4},

    // make the trailing surrogate a BMP char
    {5, 10, 3, 6, u'z', 4},

    {5, 7, 3, 6, u'b', 0}, // no space for fourth CP
    {5, 8, 3, 6, u'b', 0}, // no space for fourth CP
    {5, 9, 3, 6, u'b', 0}, // no space for fourth CP

    {4, 10, 3, 6, u'b', 0}, // incomplete fourth CP
    {4, 7, 3, 6, u'b', 0},  // incomplete fourth CP, and no space for it
    {4, 8, 3, 6, u'b', 0},  // incomplete fourth CP, and no space for it
    {4, 9, 3, 6, u'b', 0},  // incomplete fourth CP, and no space for it

  };

  for (auto t : offsets)
    {
      CharT in[array_size (valid_in)] = {};
      char out[array_size (exp) - 1] = {};
      VERIFY (t.in_size <= array_size (in));
      VERIFY (t.out_size <= array_size (out));
      VERIFY (t.expected_in_next <= t.in_size);
      VERIFY (t.expected_out_next <= t.out_size);
      copy (begin (valid_in), end (valid_in), begin (in));
      in[t.replace_pos] = t.replace_char;

      auto state = mbstate_t{};
      auto in_next = (const CharT *) nullptr;
      auto out_next = (char *) nullptr;
      auto res = codecvt_base::result ();

      res = cvt.out (state, in, in + t.in_size, in_next, out, out + t.out_size,
		     out_next);
      VERIFY (res == cvt.error);
      VERIFY (in_next == in + t.expected_in_next);
      VERIFY (out_next == out + t.expected_out_next);
      VERIFY (char_traits<char>::compare (out, exp, t.expected_out_next) == 0);
      if (t.expected_out_next < array_size (out))
	VERIFY (out[t.expected_out_next] == 0);
    }
}

template <class CharT>
void
ucs2_to_utf8_out (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  ucs2_to_utf8_out_ok (cvt);
  ucs2_to_utf8_out_partial (cvt);
  ucs2_to_utf8_out_error (cvt);
}

template <class CharT>
void
test_utf8_ucs2_cvts (const std::codecvt<CharT, char, mbstate_t> &cvt)
{
  utf8_to_ucs2_in (cvt);
  ucs2_to_utf8_out (cvt);
}
