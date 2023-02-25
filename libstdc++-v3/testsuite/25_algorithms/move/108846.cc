// { dg-do run { target c++11 } }

#include <algorithm>
#include <testsuite_hooks.h>

// PR libstdc++/108846 std::copy, std::copy_n and std::copy_backward
// on potentially overlapping subobjects

struct B {
    B(int i, short j) : i(i), j(j) {}
    int i;
    short j;
};
struct D : B {
    D(int i, short j, short x) : B(i, j), x(x) {}
    short x; // Stored in tail padding of B
};

void
test_pr108846()
{
    D ddst(1, 2, 3);
    D dsrc(4, 5, 6);
    B *dst = &ddst;
    B *src = &dsrc;
    // If this is optimized to memmove it will overwrite tail padding.
    std::move(src, src+1, dst);
    VERIFY(ddst.x == 3);
}

struct B3 {
    B3(int i, short j) : i(i), j(j) {}
    B3& operator=(B3&&) = default;
    int i;
    short j;
};
struct D3 : B3 {
    D3(int i, short j, short x) : B3(i, j), x(x) {}
    short x; // Stored in tail padding of B3
};

void
test_move_only()
{
    D3 ddst(1, 2, 3);
    D3 dsrc(4, 5, 6);
    B3 *dst = &ddst;
    B3 *src = &dsrc;
    // Ensure the not-taken trivial copy path works for this type.
    std::move(src, src+1, dst);
    VERIFY(ddst.x == 3);
}

int main()
{
  test_pr108846();
  test_move_only();
}
