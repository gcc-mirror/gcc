#include <testsuite_performance.h>

#include <algorithm>
#include <deque>
#include <list>
#include <vector>

const std::size_t size = 8192;

template <typename Container>
void bench_seq(const char* label, __gnu_test::time_counter& time,
               __gnu_test::resource_counter& resource) {
  using T = typename Container::value_type;
  Container c(size, 1);
  start_counters(time, resource);
  for (int i = 0; i < 20000; ++i)
    std::for_each(c.begin(), c.end(),
                  [](T& x) { x = std::min<T>(100, std::max<T>(x, 10)); });
  stop_counters(time, resource);
  report_performance(__FILE__, label, time, resource);
  clear_counters(time, resource);
}

int main() {
  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;

  bench_seq<std::vector<int>>("std::for_each vector<int>", time, resource);
  bench_seq<std::deque<int>>("std::for_each deque<int>", time, resource);
  bench_seq<std::list<int>>("std::for_each list<int>", time, resource);
}
