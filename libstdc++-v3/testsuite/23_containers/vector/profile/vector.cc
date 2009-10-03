// Test vector: performance difference 25% (0.444s vs 0.539s)
// Advice: set tmp as 10000

#include <vector>

using std::vector;

int main()
{
  vector <int> tmp;

  for (int j=0; j<2000; j++)
    // Insert more than default item
    for (int i=0; i<10000; i++) {
      tmp.push_back(i);
    }
}

