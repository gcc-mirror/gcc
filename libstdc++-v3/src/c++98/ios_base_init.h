// This is only in a header so we can use the system_header pragma,
// to suppress the warning caused by using a reserved init_priority.
#pragma GCC system_header

// If the target supports init priorities, set up a static object in the
// compiled library to perform the <iostream> initialization once and
// sufficiently early (so that it happens before any other global
// constructor when statically linking with libstdc++.a), instead of
// doing so in (each TU that includes) <iostream>.
#if __has_attribute(init_priority)
static ios_base::Init __ioinit __attribute__((init_priority(90)));
#endif
