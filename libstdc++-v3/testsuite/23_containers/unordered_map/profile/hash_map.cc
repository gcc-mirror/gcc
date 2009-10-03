// { dg-options "-Wno-deprecated" }
/* testing the gcc instrumented */

#include <ext/hash_map>
using namespace std;
using __gnu_cxx::hash_map;

int main()
{
  hash_map <int, int> *tmp;

  for (int j=1; j<=10; j++)
  {
    tmp = new hash_map<int, int>;
    // Insert more than default item
    for (int i=0; i<10000*j; i++) {
      (*tmp)[i]= i;
    }
    delete tmp;
  }
}

