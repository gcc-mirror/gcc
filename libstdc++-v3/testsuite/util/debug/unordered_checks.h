// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

  template<typename _Tp>
    struct KeyExtractor
    {
      static _Tp get_key(const _Tp& val)
      { return val; }
    };

  template<typename _Tp1, typename _Tp2>
    struct KeyExtractor<std::pair<const _Tp1, _Tp2>>
    {
      static _Tp1 get_key(const std::pair<const _Tp1, _Tp2>& val)
      { return val.first; }
    };

  template<typename _Tp>
    void use_erased_local_iterator()
    {
      bool test __attribute__((unused)) = true;

      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      generate_unique<val_type> gu;

      cont_type c;
      for (size_t i = 0; i != 5; ++i)
	c.insert(gu.build());

      typename cont_type::local_iterator it, end;
      for (size_t i = 0; i != c.bucket_count(); ++i)
      {
	it = c.begin(i);
	end = c.end(i);
	if (it != end)
	  break;
      }
      typename cont_type::key_type key = KeyExtractor<cont_val_type>::get_key(*it);
      c.erase(key);
      VERIFY( it != end );
  }

  template<typename _Tp>
    void use_invalid_local_iterator()
    {
      bool test __attribute__((unused)) = true;

      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      generate_unique<val_type> gu;

      cont_type c;
      for (size_t i = 0; i != 5; ++i)
	c.insert(gu.build());

      typename cont_type::local_iterator it;
      for (size_t i = 0; i != c.bucket_count(); ++i)
      {
	it = c.begin(i);
	if (it != c.end(i))
	  break;
      }
      cont_val_type val = *it;
      c.clear();
      VERIFY( *it == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_compare()
    {
      bool test __attribute__((unused)) = true;

      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      generate_unique<val_type> gu;

      cont_type c;
      for (size_t i = 0; i != 5; ++i)
	c.insert(gu.build());

      typename cont_type::local_iterator it1, it2;
      size_t i;
      for (i = 0; i != c.bucket_count(); ++i)
      {
	it1 = c.begin(i);
	if (it1 != c.end(i))
	  break;
      }
      VERIFY( i != c.bucket_count() );
      for (++i; i != c.bucket_count(); ++i)
      {
	it2 = c.begin(i);
	if (it2 != c.end(i))
	  break;
      }

      VERIFY( it1 != it2 );
    }

  template<typename _Tp>
    void invalid_local_iterator_range()
    {
      bool test __attribute__((unused)) = true;

      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      generate_unique<val_type> gu;

      cont_type c;
      for (size_t i = 0; i != 5; ++i)
	c.insert(gu.build());

      typename cont_type::local_iterator it, end;
      for (size_t i = 0; i != c.bucket_count(); ++i)
      {
	it = c.begin(i);
	end = c.end(i);
	if (it != end)
	  break;
      }
      c.insert(end, it);
    }
}

