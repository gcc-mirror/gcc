#include <bits/std_valarray.h>

namespace std
{
  // Some explicit instanciations.
  template void
     __valarray_fill(size_t* __restrict__, size_t, const size_t&);
  
  template void
     __valarray_copy(const size_t* __restrict__, size_t, size_t* __restrict__);
  
  template valarray<size_t>::valarray(size_t);
  template valarray<size_t>::valarray(const valarray<size_t>&);
  template valarray<size_t>::~valarray();
  template size_t valarray<size_t>::size() const;
  template size_t& valarray<size_t>::operator[](size_t);


  inline size_t
  __valarray_product(const valarray<size_t>& __a)
  {
    typedef const size_t* __restrict__ _Tp;
    const size_t __n = __a.size();
    // XXX: This ugly cast is necessary because
    //      valarray::operator[]() const return a VALUE!
    //      Try to get the committee to correct that gross error.
    valarray<size_t>& __t = const_cast<valarray<size_t>&>(__a);
    return __valarray_product(&__t[0], &__t[0] + __n);
  }
  
  // Map a gslice, described by its multidimensional LENGTHS
  // and corresponding STRIDES, to a linear array of INDEXES
  // for the purpose of indexing a flat, one-dimensional array
  // representation of a gslice_array.
  void
  __gslice_to_index(size_t __o, const valarray<size_t>& __l,
                    const valarray<size_t>& __s, valarray<size_t>& __i)
  {
    // There are as much as dimensions as there are strides.
    size_t __n = __l.size();

    // Get a buffer to hold current multi-index as we go through
    // the gslice for the purpose of computing its linear-image.
    size_t* const __t = static_cast<size_t*>
      (__builtin_alloca(__n * sizeof (size_t)));
    __valarray_fill(__t, __n, size_t(0));

    // Note that this should match the product of all numbers appearing
    // in __l which describes the multidimensional sizes of the
    // the generalized slice.
    const size_t __z = __i.size();
    
    for (size_t __j = 0; __j < __z; ++__j)
      {
        // Compute the linear-index image of (t_0, ... t_{n-1}).
        // Normaly, we should use inner_product<>(), but we do it the
        // the hard way here to avoid link-time can of worms.
        size_t __a = __o;
        for (size_t __k = 0; __k < __n; ++__k)
          __a += __s[__k] * __t[__k];

        __i[__j] = __a;

        // Process the next multi-index.  The loop ought to be
        // backward since we're making a lexicagraphical visit.
        ++__t[__n-1];
        for (size_t __k=__n-1; __k; --__k)
          {
            if (__t[__k] >= __l[__k])
              {
                __t[__k] = 0;
                ++__t[__k-1];
              }
          }
      }
  }
  
  gslice::_Indexer::_Indexer(size_t __o, const valarray<size_t>& __l,
                             const valarray<size_t>& __s)
      : _M_count(1), _M_start(__o), _M_size(__l), _M_stride(__s),
        _M_index(__l.size() == 0 ? 0 : __valarray_product(__l))
  { __gslice_to_index(__o, __l, __s, _M_index); }
  
}
