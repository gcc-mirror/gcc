// { dg-do compile }

// C++17 [list.overview]
// An incomplete type T may be used when instantiating list if the allocator
// satisfies the allocator completeness requirements (20.5.3.5.1).
// T shall be complete before any member of the resulting specialization
// of list is referenced.

#include <list>

struct Incomplete;

// This instantiates std::list, but none of its members.
const int sz = sizeof(std::list<Incomplete>);

// Technically the following references a member of std::list, but because
// our iterators are SCARY it doesn't instantiate any members of std::list.
// GCC's own source code expects this to work.
std::list<Incomplete>::iterator i = std::list<Incomplete>::iterator();
