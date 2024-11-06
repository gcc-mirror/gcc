// { dg-options "-D_GLIBCXX_USE_POSIX_SEMAPHORE" }
// { dg-do run { target c++20 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }

#include "try_acquire_for.cc"

// { dg-prune-output "ignoring _GLIBCXX_USE_POSIX_SEMAPHORE" }
