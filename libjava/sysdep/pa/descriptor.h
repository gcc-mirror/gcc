// Given a function pointer, return the code address.
// If the plabel bit is set, mask it off and return the code from the
// first word of the function descriptor.  Otherwise, the function
// pointer is the code address.

#define UNWRAP_FUNCTION_DESCRIPTOR(X) \
  (((unsigned int)(X)) & 2 ? *(void **)(((unsigned int)(X)) & ~3) : (X))
