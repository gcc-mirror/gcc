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
 */

// Inclusion of this file is DEPRECATED.  This is the original HP
// default allocator.  It is provided only for backward compatibility.
// This file WILL BE REMOVED in a future release.
//
// DO NOT USE THIS FILE unless you have an old container implementation
// that requires an allocator with the HP-style interface.  
//
// Standard-conforming allocators have a very different interface.  The
// standard default allocator is declared in the header <memory>.

#ifndef _CPP_BACKWARD_DEFALLOC_H
#define _CPP_BACKWARD_DEFALLOC_H 1

#include "new.h"
#include <stddef.h>
#include <stdlib.h>
#include <limits.h> 
#include "iostream.h" 
#include "algobase.h"


template <class _Tp>
inline _Tp* allocate(ptrdiff_t __size, _Tp*) {
    set_new_handler(0);
    _Tp* __tmp = (_Tp*)(::operator new((size_t)(__size * sizeof(_Tp))));
    if (__tmp == 0) {
	cerr << "out of memory" << endl; 
	exit(1);
    }
    return __tmp;
}


template <class _Tp>
inline void deallocate(_Tp* __buffer) {
    ::operator delete(__buffer);
}

template <class _Tp>
class allocator {
public:
    typedef _Tp value_type;
    typedef _Tp* pointer;
    typedef const _Tp* const_pointer;
    typedef _Tp& reference;
    typedef const _Tp& const_reference;
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    pointer allocate(size_type __n) { 
	return ::allocate((difference_type)__n, (pointer)0);
    }
    void deallocate(pointer __p) { ::deallocate(__p); }
    pointer address(reference __x) { return (pointer)&__x; }
    const_pointer const_address(const_reference __x) { 
	return (const_pointer)&__x; 
    }
    size_type init_page_size() { 
	return max(size_type(1), size_type(4096/sizeof(_Tp))); 
    }
    size_type max_size() const { 
	return max(size_type(1), size_type(UINT_MAX/sizeof(_Tp))); 
    }
};

class allocator<void> {
public:
    typedef void* pointer;
};



#endif /* _CPP_BACKWARD_DEFALLOC_H */
