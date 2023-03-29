// { dg-do run }

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
    std::copy_backward(src, src+1, dst+1);
    VERIFY(ddst.x == 3);
}

struct B2 {
    B2(int i, short j) : i(i), j(j) {}
    B2& operator=(B2& b) { i = b.i; j = b.j; return *this; }
    int i;
    short j;
};
struct D2 : B2 {
    D2(int i, short j, short x) : B2(i, j), x(x) {}
    short x; // Stored in tail padding of B2
};

void
test_non_const_copy_assign()
{
    D2 ddst(1, 2, 3);
    D2 dsrc(4, 5, 6);
    B2 *dst = &ddst;
    B2 *src = &dsrc;
    // Ensure the not-taken trivial copy path works for this type.
    std::copy_backward(src, src+1, dst+1);
    VERIFY(ddst.x == 3);
}

int main()
{
  test_pr108846();
  test_non_const_copy_assign();
}
