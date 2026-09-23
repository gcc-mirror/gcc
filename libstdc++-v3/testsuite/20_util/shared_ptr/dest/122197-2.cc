// { dg-do compile { target c++11 } }
// { dg-additional-options "-O2 -Warray-bounds -Wfree-nonheap-object -U_GLIBCXX_ASSERTIONS" }

// Bug 122197 predictive devirtualization vs middle-end warnings since r16-4000

// Bug 2507952 - Bogus array bounds warning in shared_ptr_base.h
// https://bugzilla.redhat.com/show_bug.cgi?id=2507952

#undef _GLIBCXX_ASSERTIONS

#include <memory>

struct value {
    int a;
};

value *value_new( void );
void value_free( value * );

inline std::shared_ptr<value> foo( void )
{
    // Commenting out the custom free in this line "fixes" the error
    return { value_new(), value_free };
}

struct baz {
    int x;
};

extern void bar( void );

void bar( void )
{
    auto x = std::shared_ptr<baz>( new baz{ .x = 1 } );
}
