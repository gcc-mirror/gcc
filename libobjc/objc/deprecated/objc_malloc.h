/*
** Hook functions for memory allocation and disposal.
** This makes it easy to substitute garbage collection systems
** such as Boehm's GC by assigning these function pointers
** to the GC's allocation routines.  By default these point
** to the ANSI standard malloc, realloc, free, etc.
**
** Users should call the normal objc routines above for
** memory allocation and disposal within their programs.
*/
objc_EXPORT void *(*_objc_malloc)(size_t);
objc_EXPORT void *(*_objc_atomic_malloc)(size_t);
objc_EXPORT void *(*_objc_valloc)(size_t);
objc_EXPORT void *(*_objc_realloc)(void *, size_t);
objc_EXPORT void *(*_objc_calloc)(size_t, size_t);
objc_EXPORT void (*_objc_free)(void *);

