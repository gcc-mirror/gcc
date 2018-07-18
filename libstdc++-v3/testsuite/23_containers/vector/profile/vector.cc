// Test vector: performance difference 25% (0.444s vs 0.539s)
// Advice: set tmp as 10000

// { dg-options "-DITERATIONS=20" { target simulator } }

#ifndef ITERATIONS
#define ITERATIONS 2000
#endif

#include <vector>

using std::vector;

int main()
{
  vector <int> tmp;

  for (int j=0; j<ITERATIONS; j++)
    // Insert more than default item
    for (int i=0; i<10000; i++) {
      tmp.push_back(i);
    }
}

