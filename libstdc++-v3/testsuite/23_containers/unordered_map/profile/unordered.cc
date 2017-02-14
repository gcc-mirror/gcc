// { dg-do run { target c++11 } }
/* testing the gcc instrumented */

#include <unordered_map>
#include <unordered_set>
using std::unordered_map;
using std::unordered_set;

void test_unordered_set()
{
  // Test for unordered set
  unordered_set <int> *tmp2;
  tmp2 = new unordered_set<int>;
  tmp2->insert(1);
  delete tmp2;
}
void test_unordered_map()
{
  unordered_map <int, int> *tmp;
  for (int i=0; i<20; i++) 
 {
  tmp = new unordered_map<int, int>(i+2);
  // Insert more than default item
  for (int j=0; j<10000; j++) {
      (*tmp)[j]= j;
  }
  
  delete tmp;
  }

  tmp = new unordered_map<int, int>;

  // Insert more than default item
  for (int i=0; i<150000; i++) {
//      (*tmp)[i] = i;
      (*tmp).insert(unordered_map<int, int>::value_type(i, i));
  }

  (*tmp).erase(1);
  delete tmp;
}
int main()
{
  test_unordered_set();
  test_unordered_map();
}

