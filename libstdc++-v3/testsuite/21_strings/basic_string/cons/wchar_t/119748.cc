// { dg-do compile }

// Bug 119748
// string(InputIterator, InputIterator) rejects volatile charT* as iterator

#define TEST_CHAR_TYPE wchar_t
#include "../char/119748.cc"
