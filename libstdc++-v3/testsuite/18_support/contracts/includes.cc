// { dg-options "-fcontracts" }
// { dg-do compile { target c++26 } }

// We should not get errors from including this before <contracts>:
#include <source_location>
#include <contracts>
