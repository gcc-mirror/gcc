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
 * Copyright (c) 1996
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

#ifndef STACK_H
#define STACK_H

#include <function.h>
#include <heap.h>
#include <vector.h>
#include <deque.h>

#ifndef __STL_LIMITED_DEFAULT_TEMPLATES
template <class T, class Sequence = deque<T> >
#else
template <class T, class Sequence>
#endif
class stack {
  friend bool operator==(const stack<T, Sequence>& x,
                         const stack<T, Sequence>& y);
  friend bool operator<(const stack<T, Sequence>& x,
                        const stack<T, Sequence>& y);
public:
    typedef typename Sequence::value_type value_type;
    typedef typename Sequence::size_type size_type;
protected:
    Sequence c;
public:
    bool empty() const { return c.empty(); }
    size_type size() const { return c.size(); }
    value_type& top() { return c.back(); }
    const value_type& top() const { return c.back(); }
    void push(const value_type& x) { c.push_back(x); }
    void pop() { c.pop_back(); }
};

template <class T, class Sequence>
bool operator==(const stack<T, Sequence>& x, const stack<T, Sequence>& y) {
    return x.c == y.c;
}

template <class T, class Sequence>
bool operator<(const stack<T, Sequence>& x, const stack<T, Sequence>& y) {
    return x.c < y.c;
}

#ifndef __STL_LIMITED_DEFAULT_TEMPLATES
template <class T, class Sequence = deque<T> >
#else
template <class T, class Sequence>
#endif
class queue {
friend bool operator==(const queue<T, Sequence>& x, const queue<T, Sequence>& y);
friend bool operator<(const queue<T, Sequence>& x, const queue<T, Sequence>& y);
public:
    typedef typename Sequence::value_type value_type;
    typedef typename Sequence::size_type size_type;
protected:
    Sequence c;
public:
    bool empty() const { return c.empty(); }
    size_type size() const { return c.size(); }
    value_type& front() { return c.front(); }
    const value_type& front() const { return c.front(); }
    value_type& back() { return c.back(); }
    const value_type& back() const { return c.back(); }
    void push(const value_type& x) { c.push_back(x); }
    void pop() { c.pop_front(); }
};

template <class T, class Sequence>
bool operator==(const queue<T, Sequence>& x, const queue<T, Sequence>& y) {
    return x.c == y.c;
}

template <class T, class Sequence>
bool operator<(const queue<T, Sequence>& x, const queue<T, Sequence>& y) {
    return x.c < y.c;
}

#ifndef __STL_LIMITED_DEFAULT_TEMPLATES
template <class T, class Sequence = vector<T>, 
          class Compare = less<typename Sequence::value_type> >
#else
template <class T, class Sequence, class Compare>
#endif
class  priority_queue {
public:
    typedef typename Sequence::value_type value_type;
    typedef typename Sequence::size_type size_type;
protected:
    Sequence c;
    Compare comp;
public:
    priority_queue() : c() {}
    explicit priority_queue(const Compare& x) :  c(), comp(x) {}

#ifdef __STL_MEMBER_TEMPLATES
    template <class InputIterator>
    priority_queue(InputIterator first, InputIterator last, const Compare& x)
      : c(first, last), comp(x) { make_heap(c.begin(), c.end(), comp); }
    template <class InputIterator>
    priority_queue(InputIterator first, InputIterator last) 
      : c(first, last) { make_heap(c.begin(), c.end(), comp); }
#else /* __STL_MEMBER_TEMPLATES */
    priority_queue(const value_type* first, const value_type* last, 
                   const Compare& x) : c(first, last), comp(x) {
        make_heap(c.begin(), c.end(), comp);
    }
    priority_queue(const value_type* first, const value_type* last) 
      : c(first, last) { make_heap(c.begin(), c.end(), comp); }
#endif /* __STL_MEMBER_TEMPLATES */

    bool empty() const { return c.empty(); }
    size_type size() const { return c.size(); }
    const value_type& top() const { return c.front(); }
    void push(const value_type& x) {
#         ifdef __STL_USE_EXCEPTIONS
      try {
#         endif /* __STL_USE_EXCEPTIONS */
        c.push_back(x); 
        push_heap(c.begin(), c.end(), comp);
#         ifdef __STL_USE_EXCEPTIONS
      }
      catch(...) {
        c.clear();
        throw;
      }
#         endif /* __STL_USE_EXCEPTIONS */
    }
    void pop() {
#         ifdef __STL_USE_EXCEPTIONS
      try {
#         endif /* __STL_USE_EXCEPTIONS */
        pop_heap(c.begin(), c.end(), comp);
        c.pop_back();
#         ifdef __STL_USE_EXCEPTIONS
      }
      catch(...) {
        c.clear();
        throw;
      }
#         endif /* __STL_USE_EXCEPTIONS */
    }
};

// no equality is provided

#endif
