// { dg-do compile {target c++11 } }

// C++17 [forwardlist.overview]
// An incomplete type T may be used when instantiating forward_list if the
// allocator satisfies the allocator completeness requirements (20.5.3.5.1).
// T shall be complete before any member of the resulting specialization
// of forward_list is referenced.

#include <forward_list>

struct Incomplete;

// This instantiates std::forward_list, but none of its members.
const int sz = sizeof(std::forward_list<Incomplete>);

// Technically the following references a member of std::forward_list,
// but because our iterators are SCARY it doesn't instantiate any members
// of std::forward_list.
std::forward_list<Incomplete>::iterator i{};
