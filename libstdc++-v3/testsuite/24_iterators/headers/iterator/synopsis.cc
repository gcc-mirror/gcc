// { dg-options "-std=gnu++98" }
// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

  // 24.3, primitives:
  template<class Iterator> struct iterator_traits;
  template<class T> struct iterator_traits<T*>;

  template<class Category, class T, class Distance,
	   class Pointer, class Reference> struct iterator;

  struct input_iterator_tag;
  struct output_iterator_tag;
  struct forward_iterator_tag;
  struct bidirectional_iterator_tag;
  struct random_access_iterator_tag;

  // 24.3.4, iterator operations:
  template <class InputIterator, class Distance>
  void 
  advance(InputIterator& i, Distance n);

  template <class InputIterator>
  typename iterator_traits<InputIterator>::difference_type
  distance(InputIterator first, InputIterator last);

  // 24.4, predefined iterators:
  template <class Iterator> class reverse_iterator;

  template <class Iterator>
  bool operator==(const reverse_iterator<Iterator>& x,
		  const reverse_iterator<Iterator>& y);

  template <class Iterator>
  bool operator<(const reverse_iterator<Iterator>& x,
		 const reverse_iterator<Iterator>& y);

  template <class Iterator>
  bool operator!=(const reverse_iterator<Iterator>& x,
		  const reverse_iterator<Iterator>& y);

  template <class Iterator>
  bool operator>(const reverse_iterator<Iterator>& x,
		 const reverse_iterator<Iterator>& y);

  template <class Iterator>
  bool operator>=(const reverse_iterator<Iterator>& x,
		  const reverse_iterator<Iterator>& y);

  template <class Iterator>
  bool operator<=(const reverse_iterator<Iterator>& x,
		  const reverse_iterator<Iterator>& y);

  template <class Iterator>
  typename reverse_iterator<Iterator>::difference_type 
  operator-(const reverse_iterator<Iterator>& x, 
	    const reverse_iterator<Iterator>& y);

  template <class Iterator>
  reverse_iterator<Iterator>
  operator+(typename reverse_iterator<Iterator>::difference_type n,
	    const reverse_iterator<Iterator>& x);

  template <class Container> class back_insert_iterator;
 
  template <class Container>
  back_insert_iterator<Container> back_inserter(Container& x);

  template <class Container> class front_insert_iterator;

  template <class Container>
  front_insert_iterator<Container> front_inserter(Container& x);

  template <class Container> class insert_iterator;

  template <class Container, class Iterator>
  insert_iterator<Container> inserter(Container& x, Iterator i);

  // 24.5, stream iterators:
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
