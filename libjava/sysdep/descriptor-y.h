// Given a function pointer, return the code address.

// The function descriptor is actually multiple words,
// but we don't care about anything except the first.
#define UNWRAP_FUNCTION_DESCRIPTOR(X)  (*(void **)(X))
