// { dg-do run { target c++17 } }
// { dg-require-gthreads "" }
// Bug 118681 - unsynchronized_pool_resource may fail to respect alignment

#define RESOURCE std::pmr::synchronized_pool_resource
#include "../unsynchronized_pool_resource/118681.cc"
