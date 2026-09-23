// { dg-do compile { target c++11 } }
// { dg-additional-options "-O2 -Warray-bounds -Wfree-nonheap-object" }

// Bug 122197 predictive devirtualization vs middle-end warnings since r16-4000

#undef _GLIBCXX_ASSERTIONS

#include <memory>
#include <condition_variable>

// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=122197#c16

extern std::shared_ptr<int> output_;
void f();

void f()
{
    std::shared_ptr<int> output = std::make_shared<int>();
    output_ = output;
}
