#include <map.h>
#include <algo.h>
#include <iostream.h>
#include <function.h>

#define int_less less<int>
struct str_less {
  bool operator() (char* x, char* y) const { return strcmp(x,y) < 0; }
};

#if 0
int SIZE;

void add(int x[], int y[], map<int,int, int_less>& a)
{
  for (int i = 0; i < SIZE; ++i) a[x[i]] = y[i];
}
#endif

int
main(int argv, char** argc)
{
#if 0
  if (argv > 1)
  {
    SIZE = abs(atoi(argc[1]));
    SIZE &= ~1;
  }
  else
    SIZE = 100;
  nums = new int[SIZE];
  odds = new int[SIZE];
  perm = new int[SIZE];
#endif

  map<int, int, int_less> my_map;

  map<char*, int, str_less> phones;

  my_map[4] = 40;
  my_map[2] = 20;

  // The (char*) is needed because g++ doesn't
  // convert char[] to char* in this context.
  phones[(char*)"tom"] = 2345;
  phones[(char*)"dick"] = 5678;
  phones[(char*)"harry"] = 7654;

  cout << "2 -> " << my_map[2] << endl;
  cout << "4 -> " << my_map[4] << endl;

  map<int, int, int_less>::iterator it = my_map.begin();
  for ( ; it != my_map.end(); it++)
    cout << "my_map[" << (*it).first << "] = " << (*it).second << endl; 

  map<char*, int, str_less>::iterator pit = phones.begin();
  for ( ; pit != phones.end(); pit++)
    cout << "phones[" << (*pit).first << "] = " << (*pit).second << endl; 
}
