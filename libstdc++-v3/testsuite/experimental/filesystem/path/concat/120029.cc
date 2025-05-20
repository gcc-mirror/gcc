// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Bug libstdc++/120029
// Dangling iterator usage in path::operator+=(const path& p) when this == p

#include <experimental/filesystem>
#include <testsuite_hooks.h>

namespace fs = std::experimental::filesystem;

void
test_root_dir()
{
  fs::path p = "/";
  p += p;
  p += p;
  VERIFY( p == "////" );
  p += p.filename();
  VERIFY( p == "////////" );
  p += *std::prev(p.end());
  VERIFY( p == "////////////////" );
}

void
test_root_name()
{
  fs::path p = "C:/";
  p += p;
  p += p;
  VERIFY( p == "C:/C:/C:/C:/" );
  p += p.filename(); // For Filesystem TS the filename is "."
  VERIFY( p == "C:/C:/C:/C:/." );
  p += *std::prev(p.end());
  VERIFY( p == "C:/C:/C:/C:/.." );
}

void
test_filename()
{
  fs::path p = "file";
  p += p;
  p += p;
  VERIFY( p == "filefilefilefile" );
  p += p.filename();
  VERIFY( p == "filefilefilefilefilefilefilefile" );
  p += *std::prev(p.end());
  VERIFY( p == "filefilefilefilefilefilefilefilefilefilefilefilefilefilefilefile" );
}

void
test_multi()
{
  fs::path p = "/home/username/Documents/mu";
  p += p;
  p += p;
  VERIFY( p == "/home/username/Documents/mu/home/username/Documents/mu/home/username/Documents/mu/home/username/Documents/mu" );
  p += p.filename();
  VERIFY( p == "/home/username/Documents/mu/home/username/Documents/mu/home/username/Documents/mu/home/username/Documents/mumu" );
  p += *std::prev(p.end());
  VERIFY( p == "/home/username/Documents/mu/home/username/Documents/mu/home/username/Documents/mu/home/username/Documents/mumumumu" );
  auto n = std::distance(p.begin(), p.end());
  for (int i = 0; i < n; ++i)
    p += *std::next(p.begin(), i);
}

int main()
{
  test_root_dir();
  test_root_name();
  test_filename();
  test_multi();
}
