// { dg-do run }

// CVE-2026-201020 use-after-free in binary heap erase_if

#include <ext/pb_ds/priority_queue.hpp>

bool is_odd(int v) { return v & 1; }

int main()
{
  using __gnu_pbds::priority_queue;
  using __gnu_pbds::binary_heap_tag;
  priority_queue<int, std::less<int>, binary_heap_tag> q;
  q.push(1);
  q.push(2);
  q.push(3);
  q.erase_if(&is_odd);
  q.clear();
}
