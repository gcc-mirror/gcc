// { dg-options "-O3 -Werror=stringop-overflow" }
// { dg-do compile }

// PR libstdc++/117983
// -Wstringop-overflow false positive for __builtin_memmove from vector::insert

#include <vector>

typedef std::vector<unsigned char> bytes;

void push(bytes chunk, bytes& data) {
  if (data.empty()) {
    data.swap(chunk);
  } else {
    data.insert(data.end(), chunk.begin(), chunk.end());
  }
}
