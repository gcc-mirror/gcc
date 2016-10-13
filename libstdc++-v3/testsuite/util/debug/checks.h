// Copyright (C) 2010-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#include <vector>
#include <deque>
#include <list>
#ifndef _GLIBCXX_DEBUG
#  include <debug/vector>
#  include <debug/deque>
#  include <debug/list>
#endif
#include <testsuite_hooks.h>

namespace __gnu_test
{
  template<typename _Tp>
    struct CopyableValueType
    {
      typedef _Tp value_type;
    };

  template<typename _Tp1, typename _Tp2>
    struct CopyableValueType<std::pair<const _Tp1, _Tp2> >
    {
      typedef std::pair<_Tp1, _Tp2> value_type;
    };

  template<typename _Tp>
    struct generate_unique
    {
      typedef _Tp value_type;

      value_type build()
      {
	static value_type _S_;
	++_S_;
	return _S_;
      }
    };

  template<>
    struct generate_unique<bool>
    {
      typedef bool value_type;

      value_type build()
      {
	static value_type _S_;
	_S_ = !_S_;
	return _S_;
      }
    };

  template<typename _Tp1, typename _Tp2>
    struct generate_unique<std::pair<_Tp1, _Tp2> >
    {
      typedef _Tp1 first_type;
      typedef _Tp2 second_type;
      typedef std::pair<_Tp1, _Tp2> pair_type;

      pair_type build()
      {
	static first_type _S_1;
	static second_type _S_2;
	++_S_1;
	++_S_2;
	return pair_type(_S_1, _S_2);
      }
    };

  // Check that invalid range of pointers is detected
  template<typename _Tp>
    void
    check_assign1()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::vector<val_type> vector_type;

      generate_unique<val_type> gu;

      vector_type v;
      for (int i = 0; i != 5; ++i)
        v.push_back(gu.build());
      VERIFY(v.size() == 5);

      const val_type* first = &v.front() + 1;
      const val_type* last = first + 2;

      cont_type c1;
      c1.assign(first, last);
      VERIFY(c1.size() == 2);

      cont_type c2;
      c2.assign(last, first); // Expected failure
    }

  // Check that invalid range of debug random iterators is detected
  template<typename _Tp>
    void
    check_assign2()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::vector<val_type> vector_type;

      generate_unique<val_type> gu;

      vector_type v;
      for (int i = 0; i != 5; ++i)
        v.push_back(gu.build());
      VERIFY(v.size() == 5);

      typename vector_type::iterator first = v.begin() + 1;
      typename vector_type::iterator last = first + 2;
      cont_type c1;
      c1.assign(first, last);
      VERIFY(c1.size() == 2);

      cont_type c2;
      c2.assign(last, first); // Expected failure
    }

  // Check that invalid range of debug not random iterators is detected
  template<typename _Tp>
    void
    check_assign3()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::list<val_type> list_type;

      generate_unique<val_type> gu;

      list_type l;
      for (int i = 0; i != 5; ++i)
        l.push_back(gu.build());
      VERIFY(l.size() == 5);

      typename list_type::iterator first = l.begin(); ++first;
      typename list_type::iterator last = first; ++last; ++last;
      cont_type c1;
      c1.assign(first, last);
      VERIFY(c1.size() == 2);

      cont_type c2;
      c2.assign(last, first); // Expected failure
    }

  // Check that invalid range of pointers is detected
  template<typename _Tp>
    void
    check_construct1()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::vector<val_type> vector_type;

      generate_unique<val_type> gu;

      vector_type v;
      for (int i = 0; i != 5; ++i)
        v.push_back(gu.build());
      VERIFY(v.size() == 5);

      val_type *first = &v.front() + 1;
      val_type *last = first + 2;

      cont_type c(last, first); // Expected failure
    }

  // Check that invalid range of debug random iterators is detected
  template<typename _Tp>
    void
    check_construct2()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::vector<val_type> vector_type;

      generate_unique<val_type> gu;

      vector_type v;
      for (int i = 0; i != 5; ++i)
        v.push_back(gu.build());
      VERIFY(v.size() == 5);

      typename vector_type::iterator first = v.begin() + 1;
      typename vector_type::iterator last = first + 2;

      cont_type c(last, first); // Expected failure
    }

  // Check that invalid range of debug not random iterators is detected
  template<typename _Tp>
    void
    check_construct3()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::list<val_type> list_type;

      generate_unique<val_type> gu;

      list_type l;
      for (int i = 0; i != 5; ++i)
        l.push_back(gu.build());
      VERIFY(l.size() == 5);

      typename list_type::iterator first = l.begin(); ++first;
      typename list_type::iterator last = first; ++last; ++last;

      cont_type c(last, first); // Expected failure
    }

  template <typename _Cont>
    struct InsertRangeHelper
    {
      template <typename _It>
        static void
        Insert(_Cont& cont, _It first, _It last)
	{ cont.insert(first, last); }
    };

  template <typename _Cont>
    struct InsertRangeHelperAux
    {
      template <typename _It>
        static void
        Insert(_Cont& cont, _It first, _It last)
	{ cont.insert(cont.begin(), first, last); }
    };

  template <typename _Tp1, typename _Tp2>
    struct InsertRangeHelper<std::vector<_Tp1, _Tp2> >
    : InsertRangeHelperAux<std::vector<_Tp1, _Tp2> >
    { };

  template <typename _Tp1, typename _Tp2>
    struct InsertRangeHelper<std::deque<_Tp1, _Tp2> >
    : InsertRangeHelperAux<std::deque<_Tp1, _Tp2> >
    { };

  template <typename _Tp1, typename _Tp2>
    struct InsertRangeHelper<std::list<_Tp1, _Tp2> >
    : InsertRangeHelperAux<std::list<_Tp1, _Tp2> >
    { };

#ifndef _GLIBCXX_DEBUG
  template <typename _Tp1, typename _Tp2>
    struct InsertRangeHelper<__gnu_debug::vector<_Tp1, _Tp2> >
    : InsertRangeHelperAux<__gnu_debug::vector<_Tp1, _Tp2> >
    { };

  template <typename _Tp1, typename _Tp2>
    struct InsertRangeHelper<__gnu_debug::deque<_Tp1, _Tp2> >
    : InsertRangeHelperAux<__gnu_debug::deque<_Tp1, _Tp2> >
    { };

  template <typename _Tp1, typename _Tp2>
    struct InsertRangeHelper<__gnu_debug::list<_Tp1, _Tp2> >
    : InsertRangeHelperAux<__gnu_debug::list<_Tp1, _Tp2> >
    { };
#endif

  template<typename _Tp>
    void
    check_insert1()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::vector<val_type> vector_type;

      generate_unique<val_type> gu;

      vector_type v;
      for (int i = 0; i != 5; ++i)
        v.push_back(gu.build());
      VERIFY(v.size() == 5);

      const val_type* first = &v.front() + 1;
      const val_type* last = first + 2;

      cont_type c1;
      InsertRangeHelper<cont_type>::Insert(c1, first, last);
      VERIFY(c1.size() == 2);

      cont_type c2;
      InsertRangeHelper<cont_type>::Insert(c2, last, first); // Expected failure
    }

  template<typename _Tp>
    void
    check_insert2()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::vector<val_type> vector_type;

      generate_unique<val_type> gu;

      vector_type v;
      for (int i = 0; i != 5; ++i)
        v.push_back(gu.build());
      VERIFY(v.size() == 5);

      typename vector_type::iterator first = v.begin() + 1;
      typename vector_type::iterator last = first + 2;

      cont_type c1;
      InsertRangeHelper<cont_type>::Insert(c1, first, last);
      VERIFY(c1.size() == 2);

      cont_type c2;
      InsertRangeHelper<cont_type>::Insert(c2, last, first); // Expected failure
    }

  template<typename _Tp>
    void
    check_insert3()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::list<val_type> list_type;

      generate_unique<val_type> gu;

      list_type l;
      for (int i = 0; i != 5; ++i)
        l.push_back(gu.build());
      VERIFY(l.size() == 5);

      typename list_type::iterator first = l.begin(); ++first;
      typename list_type::iterator last = first; ++last; ++last;

      cont_type c1;
      InsertRangeHelper<cont_type>::Insert(c1, first, last);
      VERIFY(c1.size() == 2);

      cont_type c2;
      InsertRangeHelper<cont_type>::Insert(c2, last, first); // Expected failure
    }

  template<typename _Tp>
    void
    check_insert4()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      typedef std::list<val_type> list_type;

      generate_unique<val_type> gu;

      list_type l;
      for (int i = 0; i != 5; ++i)
        l.push_back(gu.build());
      VERIFY(l.size() == 5);

      typename list_type::iterator first = l.begin(); ++first;
      typename list_type::iterator last = first; ++last; ++last;

      cont_type c1;
      InsertRangeHelper<cont_type>::Insert(c1, l.begin(), l.end());
      VERIFY(c1.size() == 5);

      c1.insert(c1.begin(), c1.begin(), c1.end()); // Expected failure.
    }

  template<typename _Tp>
    void use_invalid_iterator()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      generate_unique<val_type> gu;

      cont_type c;
      for (size_t i = 0; i != 5; ++i)
	c.insert(gu.build());

      typename cont_type::iterator it = c.begin();
      cont_val_type val = *it;
      c.clear();
      VERIFY( *it == val );
    }
}

