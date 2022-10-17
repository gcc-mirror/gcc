// { dg-do compile { target c++11 } }

#include <queue>

struct C : std::vector<int>
{
  C(int*, int*) { }
};

int i;

// LWG 3529. priority_queue(first, last) should construct c with (first, last)
std::priority_queue<int, C> q(&i, &i);
