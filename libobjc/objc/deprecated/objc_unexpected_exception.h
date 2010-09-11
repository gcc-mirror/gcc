/*
** Hook for uncaught exceptions.  This hook is called when an
** exception is thrown and no valid exception handler is in place.
** The function is expected never to return.  If the function returns
** the result is currently undefined.  This is deprecated.  Please use
** objc_set_uncaught_exception_handler() from objc/objc-exception.h
** instead.
*/
objc_EXPORT void (*_objc_unexpected_exception)(id) __attribute__ ((deprecated));
