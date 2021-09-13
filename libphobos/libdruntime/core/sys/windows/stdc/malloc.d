/**
  * D header file for Windows malloc.h.
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Iain Buclaw
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/stdc/_malloc.d)
 */
module core.sys.windows.stdc.malloc;
version (CRuntime_Microsoft):
extern (C):
@system:
nothrow:
@nogc:

export void* _recalloc(void*, size_t, size_t);

export void _aligned_free(void*);
export void* _aligned_malloc(size_t, size_t);

export void* _aligned_offset_malloc(size_t, size_t, size_t);
export void* _aligned_realloc(void*, size_t, size_t);
export void* _aligned_recalloc(void*, size_t, size_t, size_t);
export void* _aligned_offset_realloc(void*, size_t, size_t, size_t);
export void* _aligned_offset_recalloc(void*, size_t, size_t, size_t, size_t);
