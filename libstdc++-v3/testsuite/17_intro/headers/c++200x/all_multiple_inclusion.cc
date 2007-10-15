// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 17.4.1.2 Headers

// "C" headers
#include <cassert>
#include <ccomplex>
#include <cctype>
#include <cerrno>
#include <cfenv>
#include <cfloat>
#include <cinttypes>
#include <ciso646>
#include <climits>
#include <clocale>
#include <cmath>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstdbool>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctgmath>
#include <ctime>
#include <cwchar>
#include <cwctype>

// "C" compatibility headers
#include <assert.h>
#ifdef _GLIBCXX_HAVE_COMPLEX_H
#include <complex.h>
#endif
#include <ctype.h>
#include <errno.h>
#ifdef _GLIBCXX_HAVE_FENV_H
#include <fenv.h>
#endif
#include <float.h>
#ifdef _GLIBCXX_HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#include <iso646.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#ifdef _GLIBCXX_HAVE_STDBOOL_H
#include <stdbool.h>
#endif
#include <stddef.h>
#ifdef _GLIBCXX_HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _GLIBCXX_HAVE_TGMATH_H
#include <tgmath.h>
#endif
#include <time.h>
#ifdef _GLIBCXX_HAVE_WCHAR_H
#include <wchar.h>
#endif
#ifdef _GLIBCXX_HAVE_WCTYPE_H
#include <wctype.h>
#endif

// "C++" headers
#include <algorithm>
#include <array>
#include <bitset>
#include <complex>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <new>
#include <numeric>
#include <ostream>
#include <queue>
#include <random>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <tuple>
#include <typeinfo>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <valarray>
#include <vector>


// "C" headers
#include <cassert>
#include <ccomplex>
#include <cctype>
#include <cerrno>
#include <cfenv>
#include <cfloat>
#include <cinttypes>
#include <ciso646>
#include <climits>
#include <clocale>
#include <cmath>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstdbool>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctgmath>
#include <ctime>
#include <cwchar>
#include <cwctype>

// "C" compatibility headers
#include <assert.h>
#ifdef _GLIBCXX_HAVE_COMPLEX_H
#include <complex.h>
#endif
#include <ctype.h>
#include <errno.h>
#ifdef _GLIBCXX_HAVE_FENV_H
#include <fenv.h>
#endif
#include <float.h>
#ifdef _GLIBCXX_HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#include <iso646.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#ifdef _GLIBCXX_HAVE_STDBOOL_H
#include <stdbool.h>
#endif
#include <stddef.h>
#ifdef _GLIBCXX_HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _GLIBCXX_HAVE_TGMATH_H
#include <tgmath.h>
#endif
#include <time.h>
#ifdef _GLIBCXX_HAVE_WCHAR_H
#include <wchar.h>
#endif
#ifdef _GLIBCXX_HAVE_WCTYPE_H
#include <wctype.h>
#endif

// "C++" headers
#include <algorithm>
#include <array>
#include <bitset>
#include <complex>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <new>
#include <numeric>
#include <ostream>
#include <queue>
#include <random>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <tuple>
#include <typeinfo>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <valarray>
#include <vector>
