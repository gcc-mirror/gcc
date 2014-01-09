/* Ensure we don't pollute application namespace.  */
#define backtrace_alloc __asan_backtrace_alloc
#define backtrace_close __asan_backtrace_close
#define backtrace_create_state __asan_backtrace_create_state
#define backtrace_dwarf_add __asan_backtrace_dwarf_add
#define backtrace_free __asan_backtrace_free
#define backtrace_get_view __asan_backtrace_get_view
#define backtrace_initialize __asan_backtrace_initialize
#define backtrace_open __asan_backtrace_open
#define backtrace_pcinfo __asan_backtrace_pcinfo
#define backtrace_release_view __asan_backtrace_release_view
#define backtrace_syminfo __asan_backtrace_syminfo
#define backtrace_vector_finish __asan_backtrace_vector_finish
#define backtrace_vector_grow __asan_backtrace_vector_grow
#define backtrace_vector_release __asan_backtrace_vector_release

#ifndef __cplusplus

#include <string.h>

extern void *__asan_internal_memcpy (void *, const void *, size_t);
extern void *__asan_internal_memset (void *, int, size_t);
extern int __asan_internal_strcmp (const char *, const char *);
extern size_t __asan_internal_strlen (const char *);
extern size_t __asan_internal_strnlen (const char *, size_t);

#undef memcpy
#undef memset
#undef strcmp
#undef strlen
#undef strnlen

#define memcpy(x,y,z) __asan_internal_memcpy (x, y, z)
#define memset(x,y,z) __asan_internal_memset (x, y, z)
#define strcmp(x,y) __asan_internal_strcmp (x, y)
#define strlen(x) __asan_internal_strlen (x)
#ifdef HAVE_DECL_STRNLEN
#define strnlen(x,y) __asan_internal_strnlen (x, y)
#endif

#endif
