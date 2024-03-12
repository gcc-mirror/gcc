// { dg-options "-fno-inline" }
// { dg-do run { target c++26 } }
// { dg-additional-files "filebuf_members-1.txt" }

#include <fstream>

using type = std::basic_filebuf<wchar_t>::native_handle_type;

#include <cstdio> // std::fclose(FILE*)
#if __has_include(<unistd.h>)
# include <unistd.h> // close(int)
#endif
#if __has_include(<handleapi.h>)
# include <handleapi.h> // CloseHandle(HANDLE)
#endif

#include <testsuite_hooks.h>

void
test01()
{
  std::wfilebuf f;
  f.open("filebuf_members-1.txt", std::wios::in);
  type handle = f.native_handle();

  auto native_close = []<typename HandleT>(HandleT handle) {
    if constexpr (std::is_same_v<HandleT, std::FILE*>)
      std::fclose(handle); // --enable-cstdio=stdio_pure
#if __has_include(<unistd.h>)
    else if constexpr (std::is_same_v<HandleT, int>)
      ::close(handle); // POSIX
#endif
#if __has_include(<handleapi.h>)
    else if constexpr (std::is_same_v<HandleT, void*>)
      ::CloseHandle(handle); // Windows
#endif
    else
      VERIFY( false );
  };
  native_close(handle);

  try
  {
    f.sgetc();
    VERIFY( false );
  }
  catch (const std::ios::failure&)
  {
  }
}

int main()
{
  test01();
}
