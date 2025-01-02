// { dg-do compile { target c++17 } }
// { dg-require-normal-namespace "" }

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

#include <iterator>

namespace std {

  // C++17 24.4.3, iterator operations:
  template <class InputIterator, class Distance>
    constexpr void
    advance(InputIterator& i, Distance n);

  template <class InputIterator>
    constexpr typename iterator_traits<InputIterator>::difference_type
    distance(InputIterator first, InputIterator last);

  template <class ForwardIterator>
    constexpr ForwardIterator
    next(ForwardIterator x,
	 typename iterator_traits<ForwardIterator>::difference_type);

  template <class BidirectionalIterator>
    constexpr BidirectionalIterator
    prev(BidirectionalIterator x,
	 typename iterator_traits<BidirectionalIterator>::difference_type);

  // C++17 24.5, iterator adaptors:
  template <class Iterator> class reverse_iterator;

#if __cplusplus == 201703L
  // These operators are constrained in C++20 mode and so don't match
  // these signatures.

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator==(const reverse_iterator<Iterator1>& x,
		  const reverse_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator<(const reverse_iterator<Iterator1>& x,
		 const reverse_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator!=(const reverse_iterator<Iterator1>& x,
		  const reverse_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator>(const reverse_iterator<Iterator1>& x,
		 const reverse_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator>=(const reverse_iterator<Iterator1>& x,
		  const reverse_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator<=(const reverse_iterator<Iterator1>& x,
		  const reverse_iterator<Iterator2>& y);
#endif // C++17

  template <class Iterator1, class Iterator2>
  constexpr auto
  operator-(const reverse_iterator<Iterator1>& x,
	    const reverse_iterator<Iterator2>& y)
  -> decltype(x.base() - y.base());

  template <class Iterator>
  constexpr reverse_iterator<Iterator>
  operator+(typename reverse_iterator<Iterator>::difference_type n,
	    const reverse_iterator<Iterator>& x);

  template <class Iterator>
  constexpr reverse_iterator<Iterator> make_reverse_iterator(const Iterator&);

  template <class Container> class back_insert_iterator;

  template <class Container>
#if __cplusplus > 201703L
  constexpr
#endif
  back_insert_iterator<Container> back_inserter(Container& x);

  template <class Container> class front_insert_iterator;

  template <class Container>
#if __cplusplus > 201703L
  constexpr
#endif
  front_insert_iterator<Container> front_inserter(Container& x);

  template <class Container> class insert_iterator;

#if __cplusplus == 201703L
  // This function template is defined differently  in C++20 mode and so
  // doesn't match this signature.
  template <class Container, class Iterator>
  insert_iterator<Container> inserter(Container& x, Iterator i);

  // These operators are constrained in C++20 mode and so don't match
  // these signatures.

  template <class Iterator> class move_iterator;

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator==(const move_iterator<Iterator1>& x,
		  const move_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator!=(const move_iterator<Iterator1>& x,
		  const move_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator<(const move_iterator<Iterator1>& x,
		 const move_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator<=(const move_iterator<Iterator1>& x,
		  const move_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator>(const move_iterator<Iterator1>& x,
		 const move_iterator<Iterator2>& y);

  template <class Iterator1, class Iterator2>
  constexpr
  bool operator>=(const move_iterator<Iterator1>& x,
		  const move_iterator<Iterator2>& y);
#endif // C++17

  template <class Iterator1, class Iterator2>
  constexpr
  auto operator-(const move_iterator<Iterator1>& x,
		 const move_iterator<Iterator2>& y)
  -> decltype(x.base() - y.base());

  template <class Iterator>
  constexpr move_iterator<Iterator>
  operator+(typename move_iterator<Iterator>::difference_type,
	    const move_iterator<Iterator>&);

  template <class Iterator>
  constexpr move_iterator<Iterator> make_move_iterator(const Iterator&);

  // 24.6, stream iterators:
  template <class T, class charT, class traits, class Distance>
  class istream_iterator;

  template <class T, class charT, class traits, class Distance>
  bool operator==(const istream_iterator<T,charT,traits,Distance>& x,
		  const istream_iterator<T,charT,traits,Distance>& y);

  template <class T, class charT, class traits, class Distance>
  bool operator!=(const istream_iterator<T,charT,traits,Distance>& x,
		  const istream_iterator<T,charT,traits,Distance>& y);

  template <class T, class charT, class traits>
  class ostream_iterator;

  template<class charT, class traits>
  class istreambuf_iterator;

  template <class charT, class traits>
  bool
  operator==(const istreambuf_iterator<charT,traits>&,
	     const istreambuf_iterator<charT,traits>&);

  template <class charT, class traits>
    bool operator!=(const istreambuf_iterator<charT,traits>&,
                    const istreambuf_iterator<charT,traits>&);

  template <class charT, class traits>
    class ostreambuf_iterator;
}
