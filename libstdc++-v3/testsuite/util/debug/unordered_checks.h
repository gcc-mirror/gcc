// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
      static const _Tp& get_key(const _Tp& val)
      { return val; }
    };

  template<typename _Tp1, typename _Tp2>
    struct KeyExtractor<std::pair<_Tp1, _Tp2>>
    {
      static const _Tp1& get_key(const std::pair<_Tp1, _Tp2>& val)
      { return val.first; }
    };

  template<typename _Tp>
    void fill_container(_Tp& c)
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      typedef typename CopyableValueType<cont_val_type>::value_type val_type;
      generate_unique<val_type> gu;

      for (size_t i = 0; i != 5; ++i)
	c.insert(gu.build());
    }

  template<typename _Tp>
    void use_erased_local_iterator()
    {
      typedef _Tp cont_type;
      typedef typename cont_type::value_type cont_val_type;
      cont_type c;
      fill_container(c);

      typename cont_type::local_iterator it, end;
      for (size_t i = 0; i != c.bucket_count(); ++i)
      {
	it = c.begin(i);
	end = c.end(i);
	if (it != end)
	  break;
      }

      const auto& key = KeyExtractor<cont_val_type>::get_key(*it);
      c.erase(key);
      VERIFY( it != end );
  }

  template<typename _Tp>
    typename _Tp::local_iterator
    fill_and_get_local_iterator(_Tp& c)
    {
      typedef _Tp cont_type;
      fill_container(c);

      typename cont_type::local_iterator it;
      for (size_t i = 0; i != c.bucket_count(); ++i)
      {
	it = c.begin(i);
	if (it != c.end(i))
	  break;
      }

      return it;
    }

  template<typename _Tp>
    void use_invalid_local_iterator()
    {
      typedef _Tp cont_type;
      cont_type c;
      auto it = fill_and_get_local_iterator(c);

      const auto& val = *it;
      c.clear();
      VERIFY( *it == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_arrow_operator()
    {
      typedef _Tp cont_type;
      cont_type c;
      auto it = fill_and_get_local_iterator(c);

      const auto& val = *it;
      c.clear();
      VERIFY( *it.operator->() == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_copy_construction()
    {
      typedef _Tp cont_type;
      cont_type c;
      auto it = fill_and_get_local_iterator(c);

      const auto& val = *it;
      c.clear();
      typename cont_type::local_iterator lit(it);
      VERIFY( *lit == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_move_construction()
    {
      typedef _Tp cont_type;
      cont_type c;
      auto it = fill_and_get_local_iterator(c);

      const auto& val = *it;
      c.clear();
      typename cont_type::local_iterator lit(std::move(it));
      VERIFY( *lit == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_copy_assignment()
    {
      typedef _Tp cont_type;
      cont_type c;
      auto it = fill_and_get_local_iterator(c);

      const auto& val = *it;
      c.clear();
      typename cont_type::local_iterator lit;
      lit = it;
      VERIFY( *lit == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_move_assignment()
    {
      typedef _Tp cont_type;
      cont_type c;
      auto it = fill_and_get_local_iterator(c);

      const auto& val = *it;
      c.clear();
      typename cont_type::local_iterator lit;
      lit = std::move(it);
      VERIFY( *lit == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_const_conversion()
    {
      typedef _Tp cont_type;
      cont_type c;
      auto it = fill_and_get_local_iterator(c);

      const auto& val = *it;
      c.clear();
      typename cont_type::const_local_iterator clit(it);
      VERIFY( *clit == val );
    }

  template<typename _Tp>
    void invalid_local_iterator_pre_increment()
    {
      typedef _Tp cont_type;
      cont_type c;
      fill_container(c);

      auto lit = c.begin(0);
      for (size_t i = 0; i != 6; ++i)
	++lit;
    }

  template<typename _Tp>
    void invalid_local_iterator_post_increment()
    {
      typedef _Tp cont_type;
      cont_type c;
      fill_container(c);

      auto lit = c.begin(0);
      for (size_t i = 0; i != 6; ++i)
	lit++;
    }

  template<typename _Tp>
    void invalid_local_iterator_compare()
    {
      typedef _Tp cont_type;
      cont_type c;
      fill_container(c);

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
      typedef _Tp cont_type;
      cont_type c;
      fill_container(c);

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

