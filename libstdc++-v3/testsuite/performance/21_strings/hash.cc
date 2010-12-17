#include <string>
#include <vector>
#include <unordered_set>
#include <cstdlib>
#include <random>
#include <testsuite_performance.h>

using namespace std;

vector<string>
random_strings(int n, int len)
{
  string s(len, '\0');
  unordered_set<string> result_set;
  random_device rd;
  while (result_set.size() < n)
    {
      result_set.insert(s);
      unsigned int tmp = rd();
      tmp %= len * 256;
      s[tmp / 256] = tmp % 256;
    }
  return vector<string>(result_set.begin(), result_set.end());
}

int
main(int argc, char **argv)
{
  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;

  int string_size = 71;
  int num_strings = 6000000;
  if (argc > 1)
    {
      string_size = atoi(argv[1]);
      if (argc > 2) 
	num_strings = atoi(argv[2]);
    }

  // Construct random strings.
  vector<string> v = random_strings(num_strings, string_size);

  // Time hashing.
  size_t tmp = 0;  // prevent compiler from optimizing away all the work
  start_counters(time, resource);
  for (int i = 0; i < num_strings; i++)
    tmp += hash<string>()(v[i]);
  stop_counters(time, resource);

  if (tmp != 0 || argc < 9) // use tmp to prevent compiler optimization
    report_performance(__FILE__, "", time, resource);

  clear_counters(time, resource);

  return 0;
}
