/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996,1997
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

/* NOTE: This is an internal header file, included by other STL headers.
 *   You should not attempt to use it directly.
 */

#ifndef __SGI_STL_INTERNAL_QUEUE_H
#define __SGI_STL_INTERNAL_QUEUE_H

__STL_BEGIN_NAMESPACE

#ifndef __STL_LIMITED_DEFAULT_TEMPLATES
template <class _Tp, class _Sequence = deque<_Tp> >
#else
template <class _Tp, class _Sequence>
#endif
class queue {
  friend bool operator== __STL_NULL_TMPL_ARGS (const queue&, const queue&);
  friend bool operator< __STL_NULL_TMPL_ARGS (const queue&, const queue&);
public:
  typedef typename _Sequence::value_type      value_type;
  typedef typename _Sequence::size_type       size_type;
  typedef          _Sequence                  container_type;

  typedef typename _Sequence::reference       reference;
  typedef typename _Sequence::const_reference const_reference;
protected:
  _Sequence _M_c;
public:
  queue() : _M_c() {}
  explicit queue(const _Sequence& __c) : _M_c(__c) {}

  bool empty() const { return _M_c.empty(); }
  size_type size() const { return _M_c.size(); }
  reference front() { return _M_c.front(); }
  const_reference front() const { return _M_c.front(); }
  reference back() { return _M_c.back(); }
  const_reference back() const { return _M_c.back(); }
  void push(const value_type& __x) { _M_c.push_back(__x); }
  void pop() { _M_c.pop_front(); }
};

template <class _Tp, class _Sequence>
bool 
operator==(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return __x._M_c == __y._M_c;
}

template <class _Tp, class _Sequence>
bool
operator<(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return __x._M_c < __y._M_c;
}

#ifdef __STL_FUNCTION_TMPL_PARTIAL_ORDER

template <class _Tp, class _Sequence>
bool
operator!=(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return !(__x == __y);
}

template <class _Tp, class _Sequence>
bool 
operator>(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return __y < __x;
}

template <class _Tp, class _Sequence>
bool 
operator<=(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return !(__y < __x);
}

template <class _Tp, class _Sequence>
bool 
operator>=(const queue<_Tp, _Sequence>& __x, const queue<_Tp, _Sequence>& __y)
{
  return !(__x < __y);
}

#endif /* __STL_FUNCTION_TMPL_PARTIAL_ORDER */

#ifndef __STL_LIMITED_DEFAULT_TEMPLATES
template <class _Tp, class _Sequence = vector<_Tp>, 
          class _Compare = less<typename _Sequence::value_type> >
#else
template <class _Tp, class _Sequence, class _Compare>
#endif
class  priority_queue {
public:
  typedef typename _Sequence::value_type      value_type;
  typedef typename _Sequence::size_type       size_type;
  typedef          _Sequence                  container_type;

  typedef typename _Sequence::reference       reference;
  typedef typename _Sequence::const_reference const_reference;
protected:
  _Sequence _M_c;
  _Compare _M_comp;
public:
  priority_queue() : _M_c() {}
  explicit priority_queue(const _Compare& __x) :  _M_c(), _M_comp(__x) {}
  priority_queue(const _Compare& __x, const _Sequence& __s) 
    : _M_c(__s), _M_comp(__x) 
    { make_heap(_M_c.begin(), _M_c.end(), _M_comp); }

#ifdef __STL_MEMBER_TEMPLATES
  template <class _InputIterator>
  priority_queue(_InputIterator __first, _InputIterator __last) 
    : _M_c(__first, __last) { make_heap(_M_c.begin(), _M_c.end(), _M_comp); }

  template <class _InputIterator>
  priority_queue(_InputIterator __first, 
                 _InputIterator __last, const _Compare& __x)
    : _M_c(__first, __last), _M_comp(__x) 
    { make_heap(_M_c.begin(), _M_c.end(), _M_comp); }

  template <class _InputIterator>
  priority_queue(_InputIterator __first, _InputIterator __last,
                 const _Compare& __x, const _Sequence& __s)
  : _M_c(__s), _M_comp(__x)
  { 
    _M_c.insert(_M_c.end(), __first, __last);
    make_heap(_M_c.begin(), _M_c.end(), _M_comp);
  }

#else /* __STL_MEMBER_TEMPLATES */
  priority_queue(const value_type* __first, const value_type* __last) 
    : _M_c(__first, __last) { make_heap(_M_c.begin(), _M_c.end(), _M_comp); }

  priority_queue(const value_type* __first, const value_type* __last, 
                 const _Compare& __x) 
    : _M_c(__first, __last), _M_comp(__x)
    { make_heap(_M_c.begin(), _M_c.end(), _M_comp); }

  priority_queue(const value_type* __first, const value_type* __last, 
                 const _Compare& __x, const _Sequence& __c)
    : _M_c(__c), _M_comp(__x) 
  { 
    _M_c.insert(_M_c.end(), __first, __last);
    make_heap(_M_c.begin(), _M_c.end(), _M_comp);
  }
#endif /* __STL_MEMBER_TEMPLATES */

  bool empty() const { return _M_c.empty(); }
  size_type size() const { return _M_c.size(); }
  const_reference top() const { return _M_c.front(); }
  void push(const value_type& __x) {
    __STL_TRY {
      _M_c.push_back(__x); 
      push_heap(_M_c.begin(), _M_c.end(), _M_comp);
    }
    __STL_UNWIND(_M_c.clear());
  }
  void pop() {
    __STL_TRY {
      pop_heap(_M_c.begin(), _M_c.end(), _M_comp);
      _M_c.pop_back();
    }
    __STL_UNWIND(_M_c.clear());
  }
};

// no equality is provided

__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_QUEUE_H */

// Local Variables:
// mode:C++
// End:
