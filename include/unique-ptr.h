/* gnu::unique_ptr, a simple std::unique_ptr replacement for C++03.

   Copyright (C) 2007-2020 Free Software Foundation, Inc.

   This file is part of GCC.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* gnu::unique_ptr defines a C++ owning smart pointer that exposes a
   subset of the std::unique_ptr API.

   In fact, when compiled with a C++11 compiler, gnu::unique_ptr
   actually _is_ std::unique_ptr.  When compiled with a C++03 compiler
   OTOH, it's an hand coded std::unique_ptr emulation that assumes
   code is correct and doesn't try to be too smart.

   This supports custom deleters, but not _stateful_ deleters, so you
   can't use those in C++11 mode either.  Only the managed pointer is
   stored in the smart pointer.  That could be changed; it simply
   wasn't found necessary.

   At the end of the file you'll find a gnu::unique_ptr partial
   specialization that uses a custom (stateless) deleter:
   gnu::unique_xmalloc_ptr.  That is used to manage pointers to
   objects allocated with xmalloc.

   The C++03 version was originally based on GCC 7.0's std::auto_ptr
   and then heavily customized to behave more like C++11's
   std::unique_ptr, but at this point, it no longer shares much at all
   with the original file.  But, that's the history and the reason for
   the copyright's starting year.

   The C++03 version lets you shoot yourself in the foot, since
   similarly to std::auto_ptr, the copy constructor and assignment
   operators actually move.  Also, in the name of simplicity, no
   effort is spent on using SFINAE to prevent invalid conversions,
   etc.  This is not really a problem, because the goal here is to
   allow code that would be correct using std::unique_ptr to be
   equally correct in C++03 mode, and, just as efficient.  If client
   code compiles correctly with a C++11 (or newer) compiler, we know
   we're not doing anything invalid by mistake.

   Usage notes:

   - Putting gnu::unique_ptr in standard containers is not supported,
     since C++03 containers are not move-aware (and our emulation
     relies on copy actually moving).

   - Since there's no nullptr in C++03, gnu::unique_ptr allows
     implicit initialization and assignment from NULL instead.

   - To check whether there's an associated managed object, all these
     work as expected:

      if (ptr)
      if (!ptr)
      if (ptr != NULL)
      if (ptr == NULL)
      if (NULL != ptr)
      if (NULL == ptr)
*/

#ifndef GNU_UNIQUE_PTR_H
#define GNU_UNIQUE_PTR_H 1

#if __cplusplus >= 201103
# include <memory>
#endif

namespace gnu
{

#if __cplusplus >= 201103

/* In C++11 mode, all we need is import the standard
   std::unique_ptr.  */
template<typename T> using unique_ptr = std::unique_ptr<T>;

/* Pull in move as well.  */
using std::move;

#else /* C++11 */

/* Default destruction policy used by gnu::unique_ptr when no deleter
   is specified.  Uses delete.  */

template<typename T>
struct default_delete
{
  void operator () (T *ptr) const { delete ptr; }
};

/* Specialization for arrays.  Uses delete[].  */

template<typename T>
struct default_delete<T[]>
{
  void operator () (T *ptr) const { delete [] ptr; }
};

namespace detail
{
/* Type used to support implicit construction from NULL:

     gnu::unique_ptr<foo> func (....)
     {
     return NULL;
     }

   and assignment from NULL:

     gnu::unique_ptr<foo> ptr (....);
     ...
     ptr = NULL;

  It is intentionally not defined anywhere.  */
struct nullptr_t;

/* Base class of our unique_ptr emulation.  Contains code common to
   both unique_ptr<T, D> and unique_ptr<T[], D>.  */

template<typename T, typename D>
class unique_ptr_base
{
public:
  typedef T *pointer;
  typedef T element_type;
  typedef D deleter_type;

  /* Takes ownership of a pointer.  P is a pointer to an object of
     element_type type.  Defaults to NULL.  */
  explicit unique_ptr_base (element_type *p = NULL) throw () : m_ptr (p) {}

  /* The "move" constructor.  Really a copy constructor that actually
     moves.  Even though std::unique_ptr is not copyable, our little
     simpler emulation allows it, because:

       - There are no rvalue references in C++03.  Our move emulation
       instead relies on copy/assignment moving, like std::auto_ptr.
       - RVO/NRVO requires an accessible copy constructor
  */
  unique_ptr_base (const unique_ptr_base &other) throw ()
    : m_ptr (const_cast<unique_ptr_base &> (other).release ()) {}

  /* Converting "move" constructor.  Really an lvalue ref converting
     constructor that actually moves.  This allows constructs such as:

      unique_ptr<Derived> func_returning_unique_ptr (.....);
      ...
      unique_ptr<Base> ptr = func_returning_unique_ptr (.....);
  */
  template<typename T1, typename D1>
  unique_ptr_base (const unique_ptr_base<T1, D1> &other) throw ()
    : m_ptr (const_cast<unique_ptr_base<T1, D1> &> (other).release ()) {}

  /* The "move" assignment operator.  Really an lvalue ref copy
     assignment operator that actually moves.  See comments above.  */
  unique_ptr_base &operator= (const unique_ptr_base &other) throw ()
  {
    reset (const_cast<unique_ptr_base &> (other).release ());
    return *this;
  }

  /* Converting "move" assignment.  Really an lvalue ref converting
     copy assignment operator that moves.  See comments above.  */
  template<typename T1, typename D1>
  unique_ptr_base &operator= (const unique_ptr_base<T1, D1> &other) throw ()
  {
    reset (const_cast<unique_ptr_base<T1, D1> &> (other).release ());
    return *this;
  }

  /* std::unique_ptr does not allow assignment, except from nullptr.
     nullptr doesn't exist in C++03, so we allow assignment from NULL
     instead [ptr = NULL;].
  */
  unique_ptr_base &operator= (detail::nullptr_t *) throw ()
  {
    reset ();
    return *this;
  }

  ~unique_ptr_base () { call_deleter (); }

  /* "explicit operator bool ()" emulation using the safe bool
     idiom.  */
private:
  typedef void (unique_ptr_base::*explicit_operator_bool) () const;
  void this_type_does_not_support_comparisons () const {}

public:
  operator explicit_operator_bool () const
  {
    return (m_ptr != NULL
	    ? &unique_ptr_base::this_type_does_not_support_comparisons
	    : 0);
  }

  element_type *get () const throw () { return m_ptr; }

  element_type *release () throw ()
  {
    pointer tmp = m_ptr;
    m_ptr = NULL;
    return tmp;
  }

  void reset (element_type *p = NULL) throw ()
  {
    if (p != m_ptr)
      {
	call_deleter ();
	m_ptr = p;
      }
  }

private:

  /* Call the deleter.  Note we assume the deleter is "stateless".  */
  void call_deleter ()
  {
    D d;

    d (m_ptr);
  }

  element_type *m_ptr;
};

} /* namespace detail */

/* Macro used to create a unique_ptr_base "partial specialization" --
   a subclass that uses a specific deleter.  Basically this re-defines
   the necessary constructors.  This is necessary because C++03
   doesn't support inheriting constructors with "using".  While at it,
   we inherit the assignment operator.  TYPE is the name of the type
   being defined.  Assumes that 'base_type' is a typedef of the
   baseclass TYPE is inheriting from.  */
#define DEFINE_GNU_UNIQUE_PTR(TYPE)						\
public:									\
  explicit TYPE (T *p = NULL) throw ()					\
    : base_type (p) {}							\
									\
  TYPE (const TYPE &other) throw () : base_type (other) {}		\
									\
  TYPE (detail::nullptr_t *) throw () : base_type (NULL) {}		\
									\
  template<typename T1, typename D1>					\
  TYPE (const detail::unique_ptr_base<T1, D1> &other) throw ()		\
    : base_type (other) {}						\
									\
  using base_type::operator=;

/* Define single-object gnu::unique_ptr.  */

template <typename T, typename D = default_delete<T> >
class unique_ptr : public detail::unique_ptr_base<T, D>
{
  typedef detail::unique_ptr_base<T, D> base_type;

  DEFINE_GNU_UNIQUE_PTR (unique_ptr)

public:
  /* Dereferencing.  */
  T &operator* () const throw () { return *this->get (); }
  T *operator-> () const throw () { return this->get (); }
};

/* Define gnu::unique_ptr specialization for T[].  */

template <typename T, typename D>
class unique_ptr<T[], D> : public detail::unique_ptr_base<T, D>
{
  typedef detail::unique_ptr_base<T, D> base_type;

  DEFINE_GNU_UNIQUE_PTR (unique_ptr)

public:
  /* Indexing operator.  */
  T &operator[] (size_t i) const { return this->get ()[i]; }
};

/* Comparison operators.  */

template <typename T, typename D,
	  typename U, typename E>
inline bool
operator== (const detail::unique_ptr_base<T, D> &x,
	    const detail::unique_ptr_base<U, E> &y)
{ return x.get() == y.get(); }

template <typename T, typename D,
	  typename U, typename E>
inline bool
operator!= (const detail::unique_ptr_base<T, D> &x,
	    const detail::unique_ptr_base<U, E> &y)
{ return x.get() != y.get(); }

template<typename T, typename D,
	 typename U, typename E>
inline bool
operator< (const detail::unique_ptr_base<T, D> &x,
	   const detail::unique_ptr_base<U, E> &y)
{ return x.get() < y.get (); }

template<typename T, typename D,
	 typename U, typename E>
inline bool
operator<= (const detail::unique_ptr_base<T, D> &x,
	    const detail::unique_ptr_base<U, E> &y)
{ return !(y < x); }

template<typename T, typename D,
	 typename U, typename E>
inline bool
operator> (const detail::unique_ptr_base<T, D> &x,
	   const detail::unique_ptr_base<U, E> &y)
{ return y < x; }

template<typename T, typename D,
	 typename U, typename E>
inline bool
operator>= (const detail::unique_ptr_base<T, D> &x,
	    const detail::unique_ptr_base<U, E> &y)
{ return !(x < y); }

/* std::move "emulation".  This is as simple as it can be -- no
   attempt is made to emulate rvalue references.  This relies on T
   having move semantics like std::auto_ptr.
   I.e., copy/assignment actually moves.  */

template<typename T>
const T&
move (T& v)
{
  return v;
}

#endif /* C++11 */

/* Define gnu::unique_xmalloc_ptr, a gnu::unique_ptr that manages
   xmalloc'ed memory.  */

/* The deleter for gnu::unique_xmalloc_ptr.  Uses free.  */
template <typename T>
struct xmalloc_deleter
{
  void operator() (T *ptr) const { free (ptr); }
};

/* Same, for arrays.  */
template <typename T>
struct xmalloc_deleter<T[]>
{
  void operator() (T *ptr) const { free (ptr); }
};

#if __cplusplus >= 201103

/* In C++11, we just import the standard unique_ptr to our namespace
   with a custom deleter.  */

template<typename T> using unique_xmalloc_ptr
  = std::unique_ptr<T, xmalloc_deleter<T>>;

#else /* C++11 */

/* In C++03, we don't have template aliases, so we need to define a
   subclass instead, and re-define the constructors, because C++03
   doesn't support inheriting constructors either.  */

template <typename T>
class unique_xmalloc_ptr : public unique_ptr<T, xmalloc_deleter<T> >
{
  typedef unique_ptr<T, xmalloc_deleter<T> > base_type;

  DEFINE_GNU_UNIQUE_PTR (unique_xmalloc_ptr)
};

/* Define gnu::unique_xmalloc_ptr specialization for T[].  */

template <typename T>
class unique_xmalloc_ptr<T[]> : public unique_ptr<T[], xmalloc_deleter<T[]> >
{
  typedef unique_ptr<T[], xmalloc_deleter<T[]> > base_type;

  DEFINE_GNU_UNIQUE_PTR (unique_xmalloc_ptr)
};

#endif /* C++11 */

} /* namespace gnu */

#endif /* GNU_UNIQUE_PTR_H */
