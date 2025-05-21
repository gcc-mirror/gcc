// { dg-options "-D_GLIBCXX_CONCEPT_CHECKS" }
// { dg-do compile }

// PR 120384 _BinaryPredicateConcept checks in std::unique_copy are wrong

#include <algorithm>

void
test_pr120384(const int* first, const int* last, int* out)
{
  std::unique_copy(first, last, out);
}
