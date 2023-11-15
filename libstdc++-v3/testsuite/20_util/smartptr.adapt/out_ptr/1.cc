// { dg-do run { target c++23 } }
// { dg-require-fileio "" }
// { dg-additional-files "thirty_years_among_the_dead_preproc.txt" }

#include <memory>
#include <cstdio>
#include <testsuite_hooks.h>

// C++23 [out.ptr.t] Class template out_ptr_t

int fopen_s(std::FILE** f, const char* name, const char* mode);

struct fclose_deleter {
  void operator()(std::FILE* f) const noexcept {
    std::fclose(f);
  }
};

// Example 1 from [out.ptr.t]
void
test_example_1()
{
  constexpr const char* file_name = "thirty_years_among_the_dead_preproc.txt";
  std::unique_ptr<std::FILE, fclose_deleter> file_ptr;
  int err = fopen_s(std::out_ptr<std::FILE*>(file_ptr), file_name, "r+b");
  if (err != 0)
    VERIFY(false);
  // *file_ptr is valid
  VERIFY(file_ptr != nullptr);
}

// Same again without explicit template argument list.
void
test_example_1_2()
{
  constexpr const char* file_name = "thirty_years_among_the_dead_preproc.txt";
  std::unique_ptr<std::FILE, fclose_deleter> file_ptr;
  int err = fopen_s(std::out_ptr(file_ptr), file_name, "r+b");
  if (err != 0)
    VERIFY(false);
  // *file_ptr is valid
  VERIFY(file_ptr != nullptr);
}

// And again with a deleter argument.
void
test_example_1_3()
{
  constexpr const char* file_name = "thirty_years_among_the_dead_preproc.txt";
  std::unique_ptr<std::FILE, fclose_deleter> file_ptr;
  int err = fopen_s(std::out_ptr(file_ptr, fclose_deleter()), file_name, "r+b");
  if (err != 0)
    VERIFY(false);
  // *file_ptr is valid
  VERIFY(file_ptr != nullptr);
}

// Same again using std::shared_ptr
void
test_example_1_sp()
{
  constexpr const char* file_name = "thirty_years_among_the_dead_preproc.txt";
  std::shared_ptr<std::FILE> file_ptr;
  int err = fopen_s(std::out_ptr<std::FILE*>(file_ptr, fclose_deleter()),
		    file_name, "r+b");
  if (err != 0)
    VERIFY(false);
  // *file_ptr is valid
  VERIFY(file_ptr != nullptr);
}

// And again without explicit template argument list.
void
test_example_1_sp_2()
{
  constexpr const char* file_name = "thirty_years_among_the_dead_preproc.txt";
  std::shared_ptr<std::FILE> file_ptr;
  int err = fopen_s(std::out_ptr(file_ptr, fclose_deleter()), file_name, "r+b");
  if (err != 0)
    VERIFY(false);
  // *file_ptr is valid
  VERIFY(file_ptr != nullptr);
}

// And again using a raw pointer.
void
test_example_1_raw()
{
  constexpr const char* file_name = "thirty_years_among_the_dead_preproc.txt";
  std::FILE* file_ptr;
  int err = fopen_s(std::out_ptr(file_ptr), file_name, "r+b");
  if (err != 0)
    VERIFY(false);
  // *file_ptr is valid
  VERIFY(file_ptr != nullptr);
  std::fclose(file_ptr);
}

int main()
{
  test_example_1();
  test_example_1_2();
  test_example_1_3();
  test_example_1_sp();
  test_example_1_sp_2();
  test_example_1_raw();
}

#include <cerrno>

int fopen_s(std::FILE** f, const char* name, const char* mode)
{
  if ((*f = std::fopen(name, mode)))
    return 0;
  return errno;
}
