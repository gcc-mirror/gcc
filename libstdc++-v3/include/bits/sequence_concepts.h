/*
 * Copyright (c) 1999
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

#ifndef _STL_SEQUENCE_CONCEPTS_H
#define _STL_SEQUENCE_CONCEPTS_H 1

#pragma GCC system_header

#include <bits/container_concepts.h>

#ifdef __STL_USE_CONCEPT_CHECKS

// This file covers the following concepts:
//       _Sequence
//       _FrontInsertionSequence
//       _BackInsertionSequence

struct _ERROR_IN_STL_SEQ {

  template <class _XX>
  static void
  __fill_constructor_requirement_violation(_XX& __s) {
    typename _XX::value_type __t = typename _XX::value_type();
    typename _XX::difference_type __n = typename _XX::difference_type();
    _XX __x(__n, __t);
    __sink_unused_warning(__x);
  }
  template <class _XX>
  static void
  __fill_default_constructor_requirement_violation(_XX& __s) {
    _STL_ERROR::__default_constructor_requirement_violation(*__s.begin());
    typename _XX::difference_type __n = typename _XX::difference_type();
    _XX __x(__n);
    __sink_unused_warning(__x);
  }  
  template <class _XX>
  static void
  __range_constructor_requirement_violation(_XX& __s) {
    _XX __x(__s.begin(), __s.end());
    __sink_unused_warning(__x);
  }
  template <class _XX>
  static void
  __insert_function_requirement_violation(_XX& __s) {
    typename _XX::value_type __t = typename _XX::value_type();
    typename _XX::iterator __p = typename _XX::iterator();
    __p = __s.insert(__p, __t);
  }
  template <class _XX>
  static void
  __fill_insert_function_requirement_violation(_XX& __s) {
    typename _XX::value_type __t = typename _XX::value_type();
    typename _XX::iterator __p = typename _XX::iterator();
    typename _XX::difference_type __n = typename _XX::difference_type();
    __s.insert(__p, __n, __t);
  }
  template <class _XX>
  static void
  __range_insert_function_requirement_violation(_XX& __s) {
    typename _XX::iterator __p = typename _XX::iterator();
    typename _XX::iterator __i = typename _XX::iterator();
    typename _XX::iterator __j = typename _XX::iterator();
    __s.insert(__p, __i, __j);
  }
  template <class _XX>
  static void
  __insert_element_function_requirement_violation(_XX& __s) {
    typename _XX::value_type __t = typename _XX::value_type();
    std::pair<typename _XX::iterator, bool> __r;
    __r = __s.insert(__t);
    __sink_unused_warning(__r);
  }
  template <class _XX>
  static void
  __unconditional_insert_element_function_requirement_violation(_XX& __s) {
    typename _XX::value_type __t = typename _XX::value_type();
    typename _XX::iterator __p;
    __p = __s.insert(__t);
    __sink_unused_warning(__p);
  }
  template <class _XX>
  static void
  __erase_function_requirement_violation(_XX& __s) {
    typename _XX::iterator __p = typename _XX::iterator();
    __p = __s.erase(__p);
  }
  template <class _XX>
  static void
  __range_erase_function_requirement_violation(_XX& __s) {
    typename _XX::iterator __p = typename _XX::iterator();
    typename _XX::iterator __q = typename _XX::iterator();
    __p = __s.erase(__p, __q);
  }
  template <class _XX>
  static void
  __const_front_function_requirement_violation(const _XX& __s) {
    typename _XX::const_reference __t = __s.front();
    __sink_unused_warning(__t);
  }
  template <class _XX>
  static void
  __front_function_requirement_violation(_XX& __s) {
    typename _XX::reference __t = __s.front();
    __const_front_function_requirement_violation(__s);
    __sink_unused_warning(__t);
  }
  template <class _XX>
  static void
  __const_back_function_requirement_violation(const _XX& __s) {
    typename _XX::const_reference __t = __s.back();
    __sink_unused_warning(__t);
  }
  template <class _XX>
  static void
  __back_function_requirement_violation(_XX& __s) {
    typename _XX::reference __t = __s.back();
    __const_back_function_requirement_violation(__s);
    __sink_unused_warning(__t);
  }
  template <class _XX>
  static void
  __push_front_function_requirement_violation(_XX& __s) {
    typename _XX::value_type __t = typename _XX::value_type();
    __s.push_front(__t);
  }
  template <class _XX>
  static void
  __pop_front_function_requirement_violation(_XX& __s) {
    __s.pop_front();
  }
  template <class _XX>
  static void
  __push_back_function_requirement_violation(_XX& __s) {
    typename _XX::value_type __t = typename _XX::value_type();
    __s.push_back(__t);
  }
  template <class _XX>
  static void
  __pop_back_function_requirement_violation(_XX& __s) {
    __s.pop_back();
  }

};

/* Sequence Containers */

template <class _Sequence>
struct _Sequence_concept_specification {
static void
_Sequence_requirement_violation(_Sequence __s) {
  // Refinement of ForwardContainer
  _ForwardContainer_concept_specification<_Sequence>::_ForwardContainer_requirement_violation(__s);
  // Refinement of DefaultConstructible
  _DefaultConstructible_concept_specification<_Sequence>::_DefaultConstructible_requirement_violation(__s);
  // Valid Expressions
  _ERROR_IN_STL_SEQ::__fill_constructor_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__fill_default_constructor_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__range_constructor_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__insert_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__fill_insert_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__range_insert_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__erase_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__range_erase_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__front_function_requirement_violation(__s);
}
};

template <class _FrontInsertionSequence>
struct _FrontInsertionSequence_concept_specification {
static void
_FrontInsertionSequence_requirement_violation(_FrontInsertionSequence __s) {
  // Refinement of Sequence
  _Sequence_concept_specification<_FrontInsertionSequence>::_Sequence_requirement_violation(__s);
  // Valid Expressions
  _ERROR_IN_STL_SEQ::__push_front_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__pop_front_function_requirement_violation(__s);
}
};

template <class _BackInsertionSequence>
struct _BackInsertionSequence_concept_specification {
static void
_BackInsertionSequence_requirement_violation(_BackInsertionSequence __s) {
  // Refinement of Sequence
  _Sequence_concept_specification<_BackInsertionSequence>::_Sequence_requirement_violation(__s);
  // Valid Expressions
  _ERROR_IN_STL_SEQ::__back_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__push_back_function_requirement_violation(__s);
  _ERROR_IN_STL_SEQ::__pop_back_function_requirement_violation(__s);
}
};

#endif /* if __STL_USE_CONCEPT_CHECKS */


#endif /* _STL_SEQUENCE_CONCEPTS_H */
