#include <std/std_valarray.h>

template void
   __valarray_fill(size_t* __restrict__, size_t, const size_t&);

template void
   __valarray_copy(const size_t* __restrict__, size_t, size_t* __restrict__);

template valarray<size_t>::valarray(size_t);
template valarray<size_t>::~valarray();
template valarray<size_t>::valarray(const valarray<size_t>&);
template size_t valarray<size_t>::size() const;
template size_t& valarray<size_t>::operator[](size_t);


inline size_t
__valarray_product(const valarray<size_t>& __a)
{
  // XXX: This ugly cast is necessary because
  //      valarray::operator[]() const returns a VALUE!
  //      Try to get the committee to correct that gross error.
  typedef const size_t* __restrict__ _Tp;
  const size_t __n = __a.size();
  valarray<size_t>& __t = const_cast<valarray<size_t>&>(__a);
  return __valarray_product(&__t[0], &__t[0] + __n);
}


void __gslice_to_index(size_t __o, const valarray<size_t>& __l,
                       const valarray<size_t>& __s,
                       valarray<size_t>& __i)
{
    const size_t __n = __l.size();
    size_t* const __t = static_cast<size_t*>(alloca(__n*sizeof(size_t)));
    __valarray_fill(__t, __n, size_t(0));
    const size_t __z = __i.size();
    __valarray_fill(&__i[0], __z, __o);
    for (size_t __j=0; __j<__z; ++__j) {
        for (size_t __k=0; __k<__n; ++__k)
            __i[__j] += __s[__k]*__t[__k];
        ++__t[__n-1];
        for (size_t __k=__n-1; __k; --__k) {
            if (__t[__k] >= __l[__k]) {
                __t[__k] = 0;
                ++__t[__k-1];
            }
        }
    }
}

_Indexer::_Indexer(size_t __o, const valarray<size_t>& __l,
                   const valarray<size_t>& __s)
        : _M_count(1), _M_start(__o), _M_size(__l), _M_stride(__s),
          _M_index(__l.size() ? __valarray_product(__l) : 0)
{ __gslice_to_index(__o, __l, __s, _M_index); }



