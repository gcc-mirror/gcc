// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// C++17 30.10.15.4 Copy [fs.op.copy_file]

#include <filesystem>
#include <fstream>
#include <unistd.h> // getpid
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

namespace fs = std::filesystem;

void
test_procfs() // PR libstdc++/108178
{
  auto pid = ::getpid();
  std::string from = "/proc/" + std::to_string(pid) + "/status";
  if (fs::exists(from))
  {
    auto to = __gnu_test::nonexistent_path();
    fs::copy_file(from, to);
    std::ifstream f(to);
    VERIFY(f.is_open());
    VERIFY(f.peek() != std::char_traits<char>::eof());
    fs::remove(to);
  }
}

int main()
{
  test_procfs();
}
