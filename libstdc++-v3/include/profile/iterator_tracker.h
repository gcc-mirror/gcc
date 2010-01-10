#ifndef _GLIBCXX_PROFILE_ITERATOR_TRACKER
#define _GLIBCXX_PROFILE_ITERATOR_TRACKER 1

#include <ext/type_traits.h>

namespace std
{
namespace __profile
{

template<typename _Iterator, typename _Sequence>
class __iterator_tracker 
{
  typedef __iterator_tracker _Self;
  // The underlying iterator
  _Iterator _M_current;
  // The underlying data structure
  const _Sequence* _M_ds;
  typedef std::iterator_traits<_Iterator> _Traits;

 public:
  typedef _Iterator		              _Base_iterator;
  typedef typename _Traits::iterator_category iterator_category; 
  typedef typename _Traits::value_type        value_type;
  typedef typename _Traits::difference_type   difference_type;
  typedef typename _Traits::reference         reference;
  typedef typename _Traits::pointer           pointer;

  __iterator_tracker() : _M_current(), _M_ds(0) { }
  __iterator_tracker(const _Iterator& __i, const _Sequence* seq) 
      : _M_current(__i), _M_ds(seq) { }
  __iterator_tracker(const __iterator_tracker& __x) 
      : _M_current(__x._M_current), _M_ds(__x._M_ds) { }
  template<typename _MutableIterator>
  __iterator_tracker(const __iterator_tracker<_MutableIterator, typename __gnu_cxx::__enable_if<(std::__are_same<_MutableIterator, typename _Sequence::iterator::_Base_iterator>::__value), _Sequence>::__type>& __x)
      :  _M_current(__x.base()), _M_ds(__x._M_get_sequence()) { }

  _Iterator
  base() const { return _M_current; }
  /**
   * @brief Conversion to underlying non-debug iterator to allow
   * better interaction with non-profile containers.
   */
  operator _Iterator() const { return _M_current; }

  pointer
  operator->() const { return &*_M_current; }

  __iterator_tracker&
  operator++()
  {
    _M_ds->_M_profile_iterate();
    ++_M_current;
    return *this;
  }

  __iterator_tracker&
  operator++(int)
  {
    _M_ds->_M_profile_iterate();
    __iterator_tracker __tmp(*this);
    ++_M_current;
    return __tmp;
  }

  __iterator_tracker&
  operator--()
  {
    _M_ds->_M_profile_iterate(1);
    --_M_current;
    return *this;
  }

  __iterator_tracker&
  operator--(int)
  {
    _M_ds->_M_profile_iterate(1);
    __iterator_tracker __tmp(*this);
    --_M_current;
    return __tmp;
  }

  __iterator_tracker&
  operator=(const __iterator_tracker& __x)
  {
    _M_current = __x._M_current;
    return *this;
  }

  reference
  operator*() const
  {
    return *_M_current;
  }

 // ------ Random access iterator requirements ------
  reference
  operator[](const difference_type& __n) const 
  {
    return _M_current[__n];
  }

  __iterator_tracker&
  operator+=(const difference_type& __n)
  {
    _M_current += __n;
    return *this;
  }

  __iterator_tracker
  operator+(const difference_type& __n) const
  {
    __iterator_tracker __tmp(*this);
    __tmp += __n;
    return __tmp;
  }

  __iterator_tracker&
  operator-=(const difference_type& __n)
  {
    _M_current += -__n;
    return *this;
  }

  __iterator_tracker
  operator-(const difference_type& __n) const
  {
    __iterator_tracker __tmp(*this);
    __tmp -= __n;
    return __tmp;
  }

  void
  _M_find()
  {
    _M_ds->_M_profile_find();
  }

  const _Sequence*
  _M_get_sequence() const
  {
    return static_cast<const _Sequence*>(_M_ds);
  }
};

template<typename _IteratorL, typename _IteratorR, typename _Sequence>
inline bool
operator==(const __iterator_tracker<_IteratorL, _Sequence>& __lhs,
           const __iterator_tracker<_IteratorR, _Sequence>& __rhs)
{
  return __lhs.base() == __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline bool
operator==(const __iterator_tracker<_Iterator, _Sequence>& __lhs,
           const __iterator_tracker<_Iterator, _Sequence>& __rhs)
{
  return __lhs.base() == __rhs.base();
}

template<typename _IteratorL, typename _IteratorR, typename _Sequence>
inline bool
operator!=(const __iterator_tracker<_IteratorL, _Sequence>& __lhs,
           const __iterator_tracker<_IteratorR, _Sequence>& __rhs)
{
  return __lhs.base() != __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline bool
operator!=(const __iterator_tracker<_Iterator, _Sequence>& __lhs,
               const __iterator_tracker<_Iterator, _Sequence>& __rhs)
{
  return __lhs.base() != __rhs.base();
}

template<typename _IteratorL, typename _IteratorR, typename _Sequence>
inline bool
operator<(const __iterator_tracker<_IteratorL, _Sequence>& __lhs,
          const __iterator_tracker<_IteratorR, _Sequence>& __rhs)
{
  return __lhs.base() < __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline bool
operator<(const __iterator_tracker<_Iterator, _Sequence>& __lhs,
          const __iterator_tracker<_Iterator, _Sequence>& __rhs)
{
  return __lhs.base() < __rhs.base();
}

template<typename _IteratorL, typename _IteratorR, typename _Sequence>
inline bool
operator<=(const __iterator_tracker<_IteratorL, _Sequence>& __lhs,
           const __iterator_tracker<_IteratorR, _Sequence>& __rhs)
{
  return __lhs.base() <= __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline bool
operator<=(const __iterator_tracker<_Iterator, _Sequence>& __lhs,
           const __iterator_tracker<_Iterator, _Sequence>& __rhs)
{
  return __lhs.base() <= __rhs.base();
}

template<typename _IteratorL, typename _IteratorR, typename _Sequence>
inline bool
operator>(const __iterator_tracker<_IteratorL, _Sequence>& __lhs,
          const __iterator_tracker<_IteratorR, _Sequence>& __rhs)
{
  return __lhs.base() > __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline bool
operator>(const __iterator_tracker<_Iterator, _Sequence>& __lhs,
          const __iterator_tracker<_Iterator, _Sequence>& __rhs)
{
  return __lhs.base() > __rhs.base();
}

template<typename _IteratorL, typename _IteratorR, typename _Sequence>
inline bool
operator>=(const __iterator_tracker<_IteratorL, _Sequence>& __lhs,
           const __iterator_tracker<_IteratorR, _Sequence>& __rhs)
{
  return __lhs.base() >= __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline bool
operator>=(const __iterator_tracker<_Iterator, _Sequence>& __lhs,
           const __iterator_tracker<_Iterator, _Sequence>& __rhs)
{
  return __lhs.base() >= __rhs.base();
}

// _GLIBCXX_RESOLVE_LIB_DEFECTS
// According to the resolution of DR179 not only the various comparison
// operators but also operator- must accept mixed iterator/const_iterator
// parameters.
  template<typename _IteratorL, typename _IteratorR, typename _Sequence>
  inline typename __iterator_tracker<_IteratorL, _Sequence>::difference_type
  operator-(const __iterator_tracker<_IteratorL, _Sequence>& __lhs,
            const __iterator_tracker<_IteratorR, _Sequence>& __rhs)
{
  return __lhs.base() - __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline typename __iterator_tracker<_Iterator, _Sequence>::difference_type
operator-(const __iterator_tracker<_Iterator, _Sequence>& __lhs,
          const __iterator_tracker<_Iterator, _Sequence>& __rhs)
{
  return __lhs.base() - __rhs.base();
}

template<typename _Iterator, typename _Sequence>
inline __iterator_tracker<_Iterator, _Sequence>
operator+(typename __iterator_tracker<_Iterator,_Sequence>::difference_type
          __n,
          const __iterator_tracker<_Iterator, _Sequence>& __i)
{
  return __i + __n;
}
		
}  // namespace __profile
}  // namespace std
#endif
