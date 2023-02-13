// { dg-do compile { target c++17 } }
// { dg-options "-Wnull-dereference -O2" }

// PR libstdc++/108554
// Warning from -Wnull-dereference when extracting a unique_ptr from a map.

// { dg-bogus "null pointer dereference" "PR 108554" { target *-*-* } 0 }

#include <map>
#include <memory>
#include <string>

int pop(std::map<std::string, std::unique_ptr<int>>& m)
{
  if (auto it = m.find("key"); it != m.end())
  {
    auto item = std::move(m.extract(it).mapped());
    return *item;
  }
  return 0;
}
