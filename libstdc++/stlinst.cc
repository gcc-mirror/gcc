// Instantiation file for the -*- C++ -*- Standard Library allocator templates
// This file is part of the GNU ANSI C++ Library.

#include <alloc.h>

#ifndef __USE_MALLOC
template class __default_alloc_template<__NODE_ALLOCATOR_THREADS, 0>;
#endif

template class  __malloc_alloc_template<0>;
