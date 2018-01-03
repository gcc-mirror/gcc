// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

#include <locale>

namespace std {
  //  lib.locale, locale:
  class locale;
  template <class Facet> const Facet& use_facet(const locale&);
  template <class Facet> bool         has_facet(const locale&) throw();

  //  lib.locale.convenience, convenience interfaces:
  template <class charT> bool isspace (charT c, const locale& loc);
  template <class charT> bool isprint (charT c, const locale& loc);
  template <class charT> bool iscntrl (charT c, const locale& loc);
  template <class charT> bool isupper (charT c, const locale& loc);
  template <class charT> bool islower (charT c, const locale& loc);
  template <class charT> bool isalpha (charT c, const locale& loc);
  template <class charT> bool isdigit (charT c, const locale& loc);
  template <class charT> bool ispunct (charT c, const locale& loc);
  template <class charT> bool isxdigit(charT c, const locale& loc);
  template <class charT> bool isalnum (charT c, const locale& loc);
  template <class charT> bool isgraph (charT c, const locale& loc);
  template <class charT> charT toupper(charT c, const locale& loc);
  template <class charT> charT tolower(charT c, const locale& loc);

  //  lib.category.ctype and lib.facet.ctype.special, ctype:
  class ctype_base;
  template <class charT> class ctype;
  template <>            class ctype<char>;             //  specialization
  template <class charT> class ctype_byname;
  template <>            class ctype_byname<char>;      //  specialization
  class codecvt_base;
  template <class internT, class externT, class stateT>
  class codecvt;
  template <class internT, class externT, class stateT>
  class codecvt_byname;

  //  lib.category.numeric and lib.facet.numpunct, numeric:
  template <class charT, class InputIterator>  class num_get;
  template <class charT, class OutputIterator> class num_put;
  template <class charT> class numpunct;
  template <class charT> class numpunct_byname;

  //  lib.category.collate, collation:
  template <class charT> class collate;
  template <class charT> class collate_byname;

  //  lib.category.time, date and time:
  class time_base;
  template <class charT, class InputIterator>  class time_get;
  template <class charT, class InputIterator>  class time_get_byname;
  template <class charT, class OutputIterator> class time_put;
  template <class charT, class OutputIterator> class time_put_byname;

  //  lib.category.monetary, money:
  class money_base;
  template <class charT, class InputIterator>  class money_get;
  template <class charT, class OutputIterator> class money_put;
  template <class charT, bool Intl> class moneypunct;
  template <class charT, bool Intl> class moneypunct_byname;

  //  lib.category.messages, message retrieval:
  class messages_base;
  template <class charT> class messages;
  template <class charT> class messages_byname;
}
