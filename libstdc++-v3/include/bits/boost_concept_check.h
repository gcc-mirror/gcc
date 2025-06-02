// -*- C++ -*-

// Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// (C) Copyright Jeremy Siek 2000. Permission to copy, use, modify,
// sell and distribute this software is granted provided this
// copyright notice appears in all copies. This software is provided
// "as is" without express or implied warranty, and with no claim as
// to its suitability for any purpose.
//

/** @file bits/boost_concept_check.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iterator}
 */

// GCC Note:  based on version 1.12.0 of the Boost library.

#ifndef _BOOST_CONCEPT_CHECK_H
#define _BOOST_CONCEPT_CHECK_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/c++config.h>
#include <bits/stl_iterator_base_types.h>    // for traits and tags

namespace std  _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_BEGIN_NAMESPACE_CONTAINER
  struct _Bit_iterator;
  struct _Bit_const_iterator;
_GLIBCXX_END_NAMESPACE_CONTAINER
_GLIBCXX_END_NAMESPACE_VERSION
}

namespace __gnu_debug
{
  template<typename _Iterator, typename _Sequence, typename _Category>
    class _Safe_iterator;
}

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#pragma GCC diagnostic ignored "-Wlong-long"

#define _IsUnused __attribute__ ((__unused__))

// When the C-C code is in use, we would like this function to do as little
// as possible at runtime, use as few resources as possible, and hopefully
// be elided out of existence... hmmm.
template <class _Concept>
_GLIBCXX14_CONSTEXPR inline void __function_requires()
{
  void (_Concept::*__x)() _IsUnused = &_Concept::__constraints;
}

// No definition: if this is referenced, there's a problem with
// the instantiating type not being one of the required integer types.
// Unfortunately, this results in a link-time error, not a compile-time error.
void __error_type_must_be_an_integer_type();
void __error_type_must_be_an_unsigned_integer_type();
void __error_type_must_be_a_signed_integer_type();

// ??? Should the "concept_checking*" structs begin with more than _ ?
#define _GLIBCXX_CLASS_REQUIRES(_type_var, _ns, _concept) \
  typedef void (_ns::_concept <_type_var>::* _func##_type_var##_concept)(); \
  template <_func##_type_var##_concept _Tp1> \
  struct _concept_checking##_type_var##_concept { }; \
  typedef _concept_checking##_type_var##_concept< \
    &_ns::_concept <_type_var>::__constraints> \
    _concept_checking_typedef##_type_var##_concept

#define _GLIBCXX_CLASS_REQUIRES2(_type_var1, _type_var2, _ns, _concept) \
  typedef void (_ns::_concept <_type_var1,_type_var2>::* _func##_type_var1##_type_var2##_concept)(); \
  template <_func##_type_var1##_type_var2##_concept _Tp1> \
  struct _concept_checking##_type_var1##_type_var2##_concept { }; \
  typedef _concept_checking##_type_var1##_type_var2##_concept< \
    &_ns::_concept <_type_var1,_type_var2>::__constraints> \
    _concept_checking_typedef##_type_var1##_type_var2##_concept

#define _GLIBCXX_CLASS_REQUIRES3(_type_var1, _type_var2, _type_var3, _ns, _concept) \
  typedef void (_ns::_concept <_type_var1,_type_var2,_type_var3>::* _func##_type_var1##_type_var2##_type_var3##_concept)(); \
  template <_func##_type_var1##_type_var2##_type_var3##_concept _Tp1> \
  struct _concept_checking##_type_var1##_type_var2##_type_var3##_concept { }; \
  typedef _concept_checking##_type_var1##_type_var2##_type_var3##_concept< \
    &_ns::_concept <_type_var1,_type_var2,_type_var3>::__constraints>  \
  _concept_checking_typedef##_type_var1##_type_var2##_type_var3##_concept

#define _GLIBCXX_CLASS_REQUIRES4(_type_var1, _type_var2, _type_var3, _type_var4, _ns, _concept) \
  typedef void (_ns::_concept <_type_var1,_type_var2,_type_var3,_type_var4>::* _func##_type_var1##_type_var2##_type_var3##_type_var4##_concept)(); \
  template <_func##_type_var1##_type_var2##_type_var3##_type_var4##_concept _Tp1> \
  struct _concept_checking##_type_var1##_type_var2##_type_var3##_type_var4##_concept { }; \
  typedef _concept_checking##_type_var1##_type_var2##_type_var3##_type_var4##_concept< \
  &_ns::_concept <_type_var1,_type_var2,_type_var3,_type_var4>::__constraints> \
    _concept_checking_typedef##_type_var1##_type_var2##_type_var3##_type_var4##_concept


template <class _Tp1, class _Tp2>
struct _Aux_require_same { };

template <class _Tp>
struct _Aux_require_same<_Tp,_Tp> { typedef _Tp _Type; };

  template <class _Tp1, class _Tp2>
  struct _SameTypeConcept
  {
    void __constraints() {
      typedef typename _Aux_require_same<_Tp1, _Tp2>::_Type _Required;
    }
  };

  template <class _Tp>
  struct _IntegerConcept {
    void __constraints() {
      __error_type_must_be_an_integer_type();
    }
  };
  template <> struct _IntegerConcept<short> { void __constraints() {} };
  template <> struct _IntegerConcept<unsigned short> { void __constraints(){} };
  template <> struct _IntegerConcept<int> { void __constraints() {} };
  template <> struct _IntegerConcept<unsigned int> { void __constraints() {} };
  template <> struct _IntegerConcept<long> { void __constraints() {} };
  template <> struct _IntegerConcept<unsigned long> { void __constraints() {} };
  template <> struct _IntegerConcept<long long> { void __constraints() {} };
  template <> struct _IntegerConcept<unsigned long long>
                                                { void __constraints() {} };

  template <class _Tp>
  struct _SignedIntegerConcept {
    void __constraints() {
      __error_type_must_be_a_signed_integer_type();
    }
  };
  template <> struct _SignedIntegerConcept<short> { void __constraints() {} };
  template <> struct _SignedIntegerConcept<int> { void __constraints() {} };
  template <> struct _SignedIntegerConcept<long> { void __constraints() {} };
  template <> struct _SignedIntegerConcept<long long> { void __constraints(){}};

  template <class _Tp>
  struct _UnsignedIntegerConcept {
    void __constraints() {
      __error_type_must_be_an_unsigned_integer_type();
    }
  };
  template <> struct _UnsignedIntegerConcept<unsigned short>
    { void __constraints() {} };
  template <> struct _UnsignedIntegerConcept<unsigned int>
    { void __constraints() {} };
  template <> struct _UnsignedIntegerConcept<unsigned long>
    { void __constraints() {} };
  template <> struct _UnsignedIntegerConcept<unsigned long long>
    { void __constraints() {} };

  //===========================================================================
  // Basic Concepts

  template <class _Tp>
  struct _DefaultConstructibleConcept
  {
    void __constraints() {
      _Tp __a _IsUnused;                // require default constructor
    }
  };

  template <class _Tp>
  struct _AssignableConcept
  {
    void __constraints() {
      __a = __a;                        // require assignment operator
      __const_constraints(__a);
    }
    void __const_constraints(const _Tp& __b) {
      __a = __b;                   // const required for argument to assignment
    }
    _Tp __a;
    // possibly should be "Tp* a;" and then dereference "a" in constraint
    // functions?  present way would require a default ctor, i think...
  };

  template <class _Tp>
  struct _CopyConstructibleConcept
  {
    void __constraints() {
      _Tp __a(__b);                     // require copy constructor
      _Tp* __ptr _IsUnused = &__a;      // require address of operator
      __const_constraints(__a);
    }
    void __const_constraints(const _Tp& __a) {
      _Tp __c _IsUnused(__a);           // require const copy constructor
      const _Tp* __ptr _IsUnused = &__a; // require const address of operator
    }
    _Tp __b;
  };

  // The SGI STL version of Assignable requires copy constructor and operator=
  template <class _Tp>
  struct _SGIAssignableConcept
  {
    void __constraints() {
      _Tp __b _IsUnused(__a);
      __a = __a;                        // require assignment operator
      __const_constraints(__a);
    }
    void __const_constraints(const _Tp& __b) {
      _Tp __c _IsUnused(__b);
      __a = __b;              // const required for argument to assignment
    }
    _Tp __a;
  };

  template <class _From, class _To>
  struct _ConvertibleConcept
  {
    void __constraints() {
      _To __y _IsUnused = __x;
    }
    _From __x;
  };

  // The C++ standard requirements for many concepts talk about return
  // types that must be "convertible to bool".  The problem with this
  // requirement is that it leaves the door open for evil proxies that
  // define things like operator|| with strange return types.  Two
  // possible solutions are:
  // 1) require the return type to be exactly bool
  // 2) stay with convertible to bool, and also
  //    specify stuff about all the logical operators.
  // For now we just test for convertible to bool.
  template <class _Tp>
  void __aux_require_boolean_expr(const _Tp& __t) {
    bool __x _IsUnused = __t;
  }

// FIXME
  template <class _Tp>
  struct _EqualityComparableConcept
  {
    void __constraints() {
      __aux_require_boolean_expr(__a == __b);
    }
    _Tp __a, __b;
  };

  template <class _Tp>
  struct _LessThanComparableConcept
  {
    void __constraints() {
      __aux_require_boolean_expr(__a < __b);
    }
    _Tp __a, __b;
  };

  // This is equivalent to SGI STL's LessThanComparable.
  template <class _Tp>
  struct _ComparableConcept
  {
    void __constraints() {
      __aux_require_boolean_expr(__a < __b);
      __aux_require_boolean_expr(__a > __b);
      __aux_require_boolean_expr(__a <= __b);
      __aux_require_boolean_expr(__a >= __b);
    }
    _Tp __a, __b;
  };

#define _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT(_OP,_NAME) \
  template <class _First, class _Second> \
  struct _NAME { \
    void __constraints() { (void)__constraints_(); } \
    bool __constraints_() {  \
      return  __a _OP __b; \
    } \
    _First __a; \
    _Second __b; \
  }

#define _GLIBCXX_DEFINE_BINARY_OPERATOR_CONSTRAINT(_OP,_NAME) \
  template <class _Ret, class _First, class _Second> \
  struct _NAME { \
    void __constraints() { (void)__constraints_(); } \
    _Ret __constraints_() {  \
      return __a _OP __b; \
    } \
    _First __a; \
    _Second __b; \
  }

  _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT(==, _EqualOpConcept);
  _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT(!=, _NotEqualOpConcept);
  _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT(<, _LessThanOpConcept);
  _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT(<=, _LessEqualOpConcept);
  _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT(>, _GreaterThanOpConcept);
  _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT(>=, _GreaterEqualOpConcept);

  _GLIBCXX_DEFINE_BINARY_OPERATOR_CONSTRAINT(+, _PlusOpConcept);
  _GLIBCXX_DEFINE_BINARY_OPERATOR_CONSTRAINT(*, _TimesOpConcept);
  _GLIBCXX_DEFINE_BINARY_OPERATOR_CONSTRAINT(/, _DivideOpConcept);
  _GLIBCXX_DEFINE_BINARY_OPERATOR_CONSTRAINT(-, _SubtractOpConcept);
  _GLIBCXX_DEFINE_BINARY_OPERATOR_CONSTRAINT(%, _ModOpConcept);

#undef _GLIBCXX_DEFINE_BINARY_PREDICATE_OP_CONSTRAINT
#undef _GLIBCXX_DEFINE_BINARY_OPERATOR_CONSTRAINT

  //===========================================================================
  // Function Object Concepts

  template <class _Func, class _Return>
  struct _GeneratorConcept
  {
    void __constraints() {
      const _Return& __r _IsUnused = __f();// require operator() member function
    }
    _Func __f;
  };


  template <class _Func>
  struct _GeneratorConcept<_Func,void>
  {
    void __constraints() {
      __f();                            // require operator() member function
    }
    _Func __f;
  };

  template <class _Func, class _Return, class _Arg>
  struct _UnaryFunctionConcept
  {
    void __constraints() {
      __r = __f(__arg);                  // require operator()
    }
    _Func __f;
    _Arg __arg;
    _Return __r;
  };

  template <class _Func, class _Arg>
  struct _UnaryFunctionConcept<_Func, void, _Arg> {
    void __constraints() {
      __f(__arg);                       // require operator()
    }
    _Func __f;
    _Arg __arg;
  };

  template <class _Func, class _Return, class _First, class _Second>
  struct _BinaryFunctionConcept
  {
    void __constraints() {
      __r = __f(__first, __second);     // require operator()
    }
    _Func __f;
    _First __first;
    _Second __second;
    _Return __r;
  };

  template <class _Func, class _First, class _Second>
  struct _BinaryFunctionConcept<_Func, void, _First, _Second>
  {
    void __constraints() {
      __f(__first, __second);           // require operator()
    }
    _Func __f;
    _First __first;
    _Second __second;
  };

  template <class _Func, class _Arg>
  struct _UnaryPredicateConcept
  {
    void __constraints() {
      __aux_require_boolean_expr(__f(__arg)); // require op() returning bool
    }
    _Func __f;
    _Arg __arg;
  };

  template <class _Func, class _First, class _Second>
  struct _BinaryPredicateConcept
  {
    void __constraints() {
      __aux_require_boolean_expr(__f(__a, __b)); // require op() returning bool
    }
    _Func __f;
    _First __a;
    _Second __b;
  };

  // use this when functor is used inside a container class like std::set
  template <class _Func, class _First, class _Second>
  struct _Const_BinaryPredicateConcept {
    void __constraints() {
      __const_constraints(__f);
    }
    void __const_constraints(const _Func& __fun) {
      __function_requires<_BinaryPredicateConcept<_Func, _First, _Second> >();
      // operator() must be a const member function
      __aux_require_boolean_expr(__fun(__a, __b));
    }
    _Func __f;
    _First __a;
    _Second __b;
  };

  //===========================================================================
  // Iterator Concepts

  template <class _Tp>
  struct _TrivialIteratorConcept
  {
    void __constraints() {
//    __function_requires< _DefaultConstructibleConcept<_Tp> >();
      __function_requires< _AssignableConcept<_Tp> >();
      __function_requires< _EqualityComparableConcept<_Tp> >();
//      typedef typename std::iterator_traits<_Tp>::value_type _V;
      (void)*__i;                       // require dereference operator
    }
    _Tp __i;
  };

  template <class _Tp>
  struct _Mutable_TrivialIteratorConcept
  {
    void __constraints() {
      __function_requires< _TrivialIteratorConcept<_Tp> >();
      *__i = *__j;                      // require dereference and assignment
    }
    _Tp __i, __j;
  };

  template <class _Tp>
  struct _InputIteratorConcept
  {
    void __constraints() {
      __function_requires< _TrivialIteratorConcept<_Tp> >();
      // require iterator_traits typedef's
      typedef typename std::iterator_traits<_Tp>::difference_type _Diff;
//      __function_requires< _SignedIntegerConcept<_Diff> >();
      typedef typename std::iterator_traits<_Tp>::reference _Ref;
      typedef typename std::iterator_traits<_Tp>::pointer _Pt;
      typedef typename std::iterator_traits<_Tp>::iterator_category _Cat;
      __function_requires< _ConvertibleConcept<
        typename std::iterator_traits<_Tp>::iterator_category,
        std::input_iterator_tag> >();
      ++__i;                            // require preincrement operator
      __i++;                            // require postincrement operator
    }
    _Tp __i;
  };

  template <class _Tp, class _ValueT>
  struct _OutputIteratorConcept
  {
    void __constraints() {
      __function_requires< _AssignableConcept<_Tp> >();
      ++__i;                            // require preincrement operator
      __i++;                            // require postincrement operator
      *__i++ = __val();                 // require postincrement and assignment
    }
    _Tp __i;
    // Use a function pointer here so no definition of the function needed.
    // Just need something that returns a _ValueT (which might be a reference).
    _ValueT (*__val)();
  };

  template<typename _Tp>
  struct _Is_vector_bool_iterator
  { static const bool __value = false; };

#ifdef _GLIBCXX_DEBUG
  namespace __cont = ::std::_GLIBCXX_STD_C;
#else
  namespace __cont = ::std;
#endif

  // Trait to identify vector<bool>::iterator
  template <>
  struct _Is_vector_bool_iterator<__cont::_Bit_iterator>
  { static const bool __value = true; };

  // And for vector<bool>::const_iterator.
  template <>
  struct _Is_vector_bool_iterator<__cont::_Bit_const_iterator>
  { static const bool __value = true; };

  // And for __gnu_debug::vector<bool> iterators too.
  template <typename _It, typename _Seq, typename _Tag>
  struct _Is_vector_bool_iterator<__gnu_debug::_Safe_iterator<_It, _Seq, _Tag> >
  : _Is_vector_bool_iterator<_It> { };

  template <class _Tp, bool = _Is_vector_bool_iterator<_Tp>::__value>
  struct _ForwardIteratorReferenceConcept
  {
    void __constraints() {
#if __cplusplus >= 201103L
      typedef typename std::iterator_traits<_Tp>::reference _Ref;
      static_assert(std::is_reference<_Ref>::value,
	  "reference type of a forward iterator must be a real reference");
#endif
    }
  };

  template <class _Tp, bool = _Is_vector_bool_iterator<_Tp>::__value>
  struct _Mutable_ForwardIteratorReferenceConcept
  {
    void __constraints() {
      typedef typename std::iterator_traits<_Tp>::reference _Ref;
      typedef typename std::iterator_traits<_Tp>::value_type _Val;
      __function_requires< _SameTypeConcept<_Ref, _Val&> >();
    }
  };

  // vector<bool> iterators are not real forward iterators, but we ignore that.
  template <class _Tp>
  struct _ForwardIteratorReferenceConcept<_Tp, true>
  {
    void __constraints() { }
  };

  // vector<bool> iterators are not real forward iterators, but we ignore that.
  template <class _Tp>
  struct _Mutable_ForwardIteratorReferenceConcept<_Tp, true>
  {
    void __constraints() { }
  };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"

  template <class _Tp>
  struct _ForwardIteratorConcept
  {
    void __constraints() {
      __function_requires< _InputIteratorConcept<_Tp> >();
      __function_requires< _DefaultConstructibleConcept<_Tp> >();
      __function_requires< _ConvertibleConcept<
        typename std::iterator_traits<_Tp>::iterator_category,
        std::forward_iterator_tag> >();
      __function_requires< _ForwardIteratorReferenceConcept<_Tp> >();
      _Tp& __j = ++__i;
      const _Tp& __k = __i++;
      typedef typename std::iterator_traits<_Tp>::reference _Ref;
      _Ref __r = *__k;
      _Ref __r2 = *__i++;
    }
    _Tp __i;
  };

  template <class _Tp>
  struct _Mutable_ForwardIteratorConcept
  {
    void __constraints() {
      __function_requires< _ForwardIteratorConcept<_Tp> >();
      typedef typename std::iterator_traits<_Tp>::reference _Ref;
      typedef typename std::iterator_traits<_Tp>::value_type _Val;
      __function_requires< _Mutable_ForwardIteratorReferenceConcept<_Tp> >();
    }
    _Tp __i;
  };

  template <class _Tp>
  struct _BidirectionalIteratorConcept
  {
    void __constraints() {
      __function_requires< _ForwardIteratorConcept<_Tp> >();
      __function_requires< _ConvertibleConcept<
        typename std::iterator_traits<_Tp>::iterator_category,
        std::bidirectional_iterator_tag> >();
      _Tp& __j = --__i;                 // require predecrement operator
      const _Tp& __k = __i--;           // require postdecrement operator
      typedef typename std::iterator_traits<_Tp>::reference _Ref;
      _Ref __r = *__j--;
    }
    _Tp __i;
  };

  template <class _Tp>
  struct _Mutable_BidirectionalIteratorConcept
  {
    void __constraints() {
      __function_requires< _BidirectionalIteratorConcept<_Tp> >();
      __function_requires< _Mutable_ForwardIteratorConcept<_Tp> >();
    }
    _Tp __i;
  };


  template <class _Tp>
  struct _RandomAccessIteratorConcept
  {
    void __constraints() {
      __function_requires< _BidirectionalIteratorConcept<_Tp> >();
      __function_requires< _ComparableConcept<_Tp> >();
      __function_requires< _ConvertibleConcept<
        typename std::iterator_traits<_Tp>::iterator_category,
        std::random_access_iterator_tag> >();
      typedef typename std::iterator_traits<_Tp>::reference _Ref;

      _Tp& __j = __i += __n;            // require assignment addition operator
      __i = __i + __n; __i = __n + __i; // require addition with difference type
      _Tp& __k = __i -= __n;            // require assignment subtraction op
      __i = __i - __n;                  // require subtraction with
                                        //            difference type
      __n = __i - __j;                  // require difference operator
      _Ref __r = __i[__n];              // require element access operator
    }
    _Tp __a, __b;
    _Tp __i, __j;
    typename std::iterator_traits<_Tp>::difference_type __n;
  };

  template <class _Tp>
  struct _Mutable_RandomAccessIteratorConcept
  {
    void __constraints() {
      __function_requires< _RandomAccessIteratorConcept<_Tp> >();
      __function_requires< _Mutable_BidirectionalIteratorConcept<_Tp> >();
    }
    _Tp __i;
    typename std::iterator_traits<_Tp>::difference_type __n;
  };

#pragma GCC diagnostic pop

  //===========================================================================
  // Container Concepts

  template <class _Container>
  struct _ContainerConcept
  {
    typedef typename _Container::value_type _Value_type;
    typedef typename _Container::difference_type _Difference_type;
    typedef typename _Container::size_type _Size_type;
    typedef typename _Container::const_reference _Const_reference;
    typedef typename _Container::const_pointer _Const_pointer;
    typedef typename _Container::const_iterator _Const_iterator;

    void __constraints() {
      __function_requires< _InputIteratorConcept<_Const_iterator> >();
      __function_requires< _AssignableConcept<_Container> >();
      const _Container __c;
      __i = __c.begin();
      __i = __c.end();
      __n = __c.size();
      __n = __c.max_size();
      __b = __c.empty();
    }
    bool __b;
    _Const_iterator __i;
    _Size_type __n;
  };

  template <class _Container>
  struct _Mutable_ContainerConcept
  {
    typedef typename _Container::value_type _Value_type;
    typedef typename _Container::reference _Reference;
    typedef typename _Container::iterator _Iterator;
    typedef typename _Container::pointer _Pointer;

    void __constraints() {
      __function_requires< _ContainerConcept<_Container> >();
      __function_requires< _AssignableConcept<_Value_type> >();
      __function_requires< _InputIteratorConcept<_Iterator> >();

      __i = __c.begin();
      __i = __c.end();
      __c.swap(__c2);
    }
    _Iterator __i;
    _Container __c, __c2;
  };

  template <class _ForwardContainer>
  struct _ForwardContainerConcept
  {
    void __constraints() {
      __function_requires< _ContainerConcept<_ForwardContainer> >();
      typedef typename _ForwardContainer::const_iterator _Const_iterator;
      __function_requires< _ForwardIteratorConcept<_Const_iterator> >();
    }
  };

  template <class _ForwardContainer>
  struct _Mutable_ForwardContainerConcept
  {
    void __constraints() {
      __function_requires< _ForwardContainerConcept<_ForwardContainer> >();
      __function_requires< _Mutable_ContainerConcept<_ForwardContainer> >();
      typedef typename _ForwardContainer::iterator _Iterator;
      __function_requires< _Mutable_ForwardIteratorConcept<_Iterator> >();
    }
  };

  template <class _ReversibleContainer>
  struct _ReversibleContainerConcept
  {
    typedef typename _ReversibleContainer::const_iterator _Const_iterator;
    typedef typename _ReversibleContainer::const_reverse_iterator
      _Const_reverse_iterator;

    void __constraints() {
      __function_requires< _ForwardContainerConcept<_ReversibleContainer> >();
      __function_requires< _BidirectionalIteratorConcept<_Const_iterator> >();
      __function_requires<
        _BidirectionalIteratorConcept<_Const_reverse_iterator> >();

      const _ReversibleContainer __c;
      _Const_reverse_iterator __i = __c.rbegin();
      __i = __c.rend();
    }
  };

  template <class _ReversibleContainer>
  struct _Mutable_ReversibleContainerConcept
  {
    typedef typename _ReversibleContainer::iterator _Iterator;
    typedef typename _ReversibleContainer::reverse_iterator _Reverse_iterator;

    void __constraints() {
      __function_requires<_ReversibleContainerConcept<_ReversibleContainer> >();
      __function_requires<
        _Mutable_ForwardContainerConcept<_ReversibleContainer> >();
      __function_requires<_Mutable_BidirectionalIteratorConcept<_Iterator> >();
      __function_requires<
        _Mutable_BidirectionalIteratorConcept<_Reverse_iterator> >();

      _Reverse_iterator __i = __c.rbegin();
      __i = __c.rend();
    }
    _ReversibleContainer __c;
  };

  template <class _RandomAccessContainer>
  struct _RandomAccessContainerConcept
  {
    typedef typename _RandomAccessContainer::size_type _Size_type;
    typedef typename _RandomAccessContainer::const_reference _Const_reference;
    typedef typename _RandomAccessContainer::const_iterator _Const_iterator;
    typedef typename _RandomAccessContainer::const_reverse_iterator
      _Const_reverse_iterator;

    void __constraints() {
      __function_requires<
        _ReversibleContainerConcept<_RandomAccessContainer> >();
      __function_requires< _RandomAccessIteratorConcept<_Const_iterator> >();
      __function_requires<
        _RandomAccessIteratorConcept<_Const_reverse_iterator> >();

      const _RandomAccessContainer __c;
      _Const_reference __r _IsUnused = __c[__n];
    }
    _Size_type __n;
  };

  template <class _RandomAccessContainer>
  struct _Mutable_RandomAccessContainerConcept
  {
    typedef typename _RandomAccessContainer::size_type _Size_type;
    typedef typename _RandomAccessContainer::reference _Reference;
    typedef typename _RandomAccessContainer::iterator _Iterator;
    typedef typename _RandomAccessContainer::reverse_iterator _Reverse_iterator;

    void __constraints() {
      __function_requires<
        _RandomAccessContainerConcept<_RandomAccessContainer> >();
      __function_requires<
        _Mutable_ReversibleContainerConcept<_RandomAccessContainer> >();
      __function_requires< _Mutable_RandomAccessIteratorConcept<_Iterator> >();
      __function_requires<
        _Mutable_RandomAccessIteratorConcept<_Reverse_iterator> >();

      _Reference __r _IsUnused = __c[__i];
    }
    _Size_type __i;
    _RandomAccessContainer __c;
  };

  // A Sequence is inherently mutable
  template <class _Sequence>
  struct _SequenceConcept
  {
    typedef typename _Sequence::reference _Reference;
    typedef typename _Sequence::const_reference _Const_reference;

    void __constraints() {
      // Matt Austern's book puts DefaultConstructible here, the C++
      // standard places it in Container
      //    function_requires< DefaultConstructible<Sequence> >();
      __function_requires< _Mutable_ForwardContainerConcept<_Sequence> >();
      __function_requires< _DefaultConstructibleConcept<_Sequence> >();

      _Sequence
	__c _IsUnused(__n, __t),
        __c2 _IsUnused(__first, __last);

      __c.insert(__p, __t);
      __c.insert(__p, __n, __t);
      __c.insert(__p, __first, __last);

      __c.erase(__p);
      __c.erase(__p, __q);

      _Reference __r _IsUnused = __c.front();

      __const_constraints(__c);
    }
    void __const_constraints(const _Sequence& __c) {
      _Const_reference __r _IsUnused = __c.front();
    }
    typename _Sequence::value_type __t;
    typename _Sequence::size_type __n;
    typename _Sequence::value_type *__first, *__last;
    typename _Sequence::iterator __p, __q;
  };

  template <class _FrontInsertionSequence>
  struct _FrontInsertionSequenceConcept
  {
    void __constraints() {
      __function_requires< _SequenceConcept<_FrontInsertionSequence> >();

      __c.push_front(__t);
      __c.pop_front();
    }
    _FrontInsertionSequence __c;
    typename _FrontInsertionSequence::value_type __t;
  };

  template <class _BackInsertionSequence>
  struct _BackInsertionSequenceConcept
  {
    typedef typename _BackInsertionSequence::reference _Reference;
    typedef typename _BackInsertionSequence::const_reference _Const_reference;

    void __constraints() {
      __function_requires< _SequenceConcept<_BackInsertionSequence> >();

      __c.push_back(__t);
      __c.pop_back();
      _Reference __r _IsUnused = __c.back();
    }
    void __const_constraints(const _BackInsertionSequence& __c) {
      _Const_reference __r _IsUnused = __c.back();
    };
    _BackInsertionSequence __c;
    typename _BackInsertionSequence::value_type __t;
  };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#pragma GCC diagnostic pop
#undef _IsUnused

#endif // _GLIBCXX_BOOST_CONCEPT_CHECK


