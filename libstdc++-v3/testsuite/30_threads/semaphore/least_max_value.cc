// { dg-do compile { target c++20 } }
// { dg-require-effective-target gthreads { target { ! *-*-linux* } } }
// { dg-require-effective-target hosted }

#include <semaphore>

std::counting_semaphore<> sem(0);
std::counting_semaphore<> sem2(2);
std::counting_semaphore sem3(3);
