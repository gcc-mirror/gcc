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
#define backtrace_qsort __asan_backtrace_qsort
#define backtrace_release_view __asan_backtrace_release_view
#define backtrace_syminfo __asan_backtrace_syminfo
#define backtrace_uncompress_zdebug __asan_backtrace_uncompress_zdebug
#define backtrace_vector_finish __asan_backtrace_vector_finish
#define backtrace_vector_grow __asan_backtrace_vector_grow
#define backtrace_vector_release __asan_backtrace_vector_release

#define cplus_demangle_builtin_types __asan_cplus_demangle_builtin_types
#define cplus_demangle_fill_ctor __asan_cplus_demangle_fill_ctor
#define cplus_demangle_fill_dtor __asan_cplus_demangle_fill_dtor
#define cplus_demangle_fill_extended_operator __asan_cplus_demangle_fill_extended_operator
#define cplus_demangle_fill_name __asan_cplus_demangle_fill_name
#define cplus_demangle_init_info __asan_cplus_demangle_init_info
#define cplus_demangle_mangled_name __asan_cplus_demangle_mangled_name
#define cplus_demangle_operators __asan_cplus_demangle_operators
#define cplus_demangle_print __asan_cplus_demangle_print
#define cplus_demangle_print_callback __asan_cplus_demangle_print_callback
#define cplus_demangle_type __asan_cplus_demangle_type
#define cplus_demangle_v3 __asan_cplus_demangle_v3
#define cplus_demangle_v3_callback __asan_cplus_demangle_v3_callback
#define is_gnu_v3_mangled_ctor __asan_is_gnu_v3_mangled_ctor
#define is_gnu_v3_mangled_dtor __asan_is_gnu_v3_mangled_dtor
#define java_demangle_v3 __asan_java_demangle_v3
#define java_demangle_v3_callback __asan_java_demangle_v3_callback

#ifndef __cplusplus

#include <string.h>

extern void *__asan_internal_memcpy (void *, const void *, size_t);
extern void *__asan_internal_memset (void *, int, size_t);
extern int __asan_internal_memcmp (const void *, const void *, size_t);
extern int __asan_internal_strcmp (const char *, const char *);
extern int __asan_internal_strncmp (const char *, const char *, size_t);
extern size_t __asan_internal_strlen (const char *);
extern size_t __asan_internal_strnlen (const char *, size_t);

#undef memcpy
#undef memset
#undef memcmp
#undef strcmp
#undef strncmp
#undef strlen
#undef strnlen

#define memcpy(x,y,z) __asan_internal_memcpy (x, y, z)
#define memset(x,y,z) __asan_internal_memset (x, y, z)
#define memcmp(x,y,z) __asan_internal_memcmp (x, y, z)
#define strcmp(x,y) __asan_internal_strcmp (x, y)
#define strncmp(x,y,z) __asan_internal_strncmp (x, y, z)
#define strlen(x) __asan_internal_strlen (x)
#ifdef HAVE_DECL_STRNLEN
#define strnlen(x,y) __asan_internal_strnlen (x, y)
#endif

#endif
