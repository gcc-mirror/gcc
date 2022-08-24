// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <experimental/scope>
#include <testsuite_hooks.h>

int da_funk = 0;
void funk() { ++da_funk; }

struct ThrowingCopy
{
  ThrowingCopy() = default;
  ThrowingCopy(ThrowingCopy&&) noexcept(false) { VERIFY(false); }
  ThrowingCopy(const ThrowingCopy&) { if (nocopy) throw 1; }

  void operator()() const noexcept { ++counter; }

  static ThrowingCopy create() noexcept { nocopy = false; return {}; }

  static bool nocopy;
  static int counter;
};

bool ThrowingCopy::nocopy = false;
int ThrowingCopy::counter = 0;

void
test_exit()
{
  using std::experimental::scope_exit;

  int counter = 0;
  auto d = [&counter] () { ++counter; };

  {
    scope_exit e(d);
  }
  VERIFY( counter == 1 );

  try
  {
    scope_exit e(d);
    throw 1;
  }
  catch (int)
  {
  }
  VERIFY( counter == 2 );

  {
    scope_exit e(d);
    scope_exit e2(std::move(e));
  }
  VERIFY( counter == 3 );

  {
    scope_exit e(d);
    e.release();
  }
  VERIFY( counter == 3 );

  try
  {
    scope_exit e(d);
    e.release();
    throw 1;
  }
  catch (int)
  {
  }
  VERIFY( counter == 3 );

  {
    da_funk = 0;
    scope_exit<void(&)()> e(funk);
  }
  VERIFY( da_funk == 1 );

  static_assert(!std::is_move_assignable_v<scope_exit<void(*)()>>);
  static_assert(!std::is_move_assignable_v<scope_exit<void(&)()>>);
  static_assert(!std::is_move_assignable_v<scope_exit<ThrowingCopy>>);
  static_assert(!std::is_move_assignable_v<scope_exit<decltype(d)>>);

  {
    ThrowingCopy::counter = 0;
    try
    {
      scope_exit<ThrowingCopy> e(ThrowingCopy::create());
      ThrowingCopy::nocopy = true;
      scope_exit<ThrowingCopy> e2(std::move(e));
      VERIFY(false);
    }
    catch (int)
    {
    }
    VERIFY( ThrowingCopy::counter == 1 );

    scope_exit<ThrowingCopy> e(ThrowingCopy::create());
    try
    {
      ThrowingCopy::nocopy = true;
      scope_exit<ThrowingCopy> e2(std::move(e));
      VERIFY(false);
    }
    catch (int)
    {
    }
    VERIFY( ThrowingCopy::counter == 1 );
  }
  VERIFY( ThrowingCopy::counter == 2 );
}

void
test_fail()
{
  using std::experimental::scope_fail;
  
  int counter = 0;
  auto d = [&counter] () { ++counter; };

  {
    scope_fail f(d);
  }
  VERIFY( counter == 0 );

  try
  {
    scope_fail f(d);
    throw 1;
  }
  catch (int)
  {
  }
  VERIFY( counter == 1 );

  {
    scope_fail f(d);
    f.release();
  }
  VERIFY( counter == 1 );

  try
  {
    scope_fail f(d);
    scope_fail f2(std::move(f));
    throw 1;
  }
  catch(int)
  {
  }
  VERIFY( counter == 2 );

  try
  {
    scope_fail f(d);
    f.release();
    throw 1;
  }
  catch (int)
  {
  }
  VERIFY( counter == 2 );

  try
  {
    da_funk = 0;
    scope_fail<void(&)()> e(funk);
    throw 1;
  }
  catch (int)
  {
  }
  VERIFY( da_funk == 1 );

  static_assert(!std::is_move_assignable_v<scope_fail<void(*)()>>);
  static_assert(!std::is_move_assignable_v<scope_fail<void(&)()>>);
  static_assert(!std::is_move_assignable_v<scope_fail<ThrowingCopy>>);
  static_assert(!std::is_move_assignable_v<scope_fail<decltype(d)>>);

  {
    ThrowingCopy::counter = 0;
    try
    {
      scope_fail<ThrowingCopy> f(ThrowingCopy::create());
      ThrowingCopy::nocopy = true;
      scope_fail<ThrowingCopy> f2(std::move(f));
      VERIFY(false);
    }
    catch (int)
    {
    }
    VERIFY( ThrowingCopy::counter == 1 );

    scope_fail<ThrowingCopy> f(ThrowingCopy::create());
    try
    {
      ThrowingCopy::nocopy = true;
      scope_fail<ThrowingCopy> f2(std::move(f));
      VERIFY(false);
    }
    catch (int)
    {
    }
    VERIFY( ThrowingCopy::counter == 1 );
  }
  VERIFY( ThrowingCopy::counter == 1 );
}

void
test_success()
{
  using std::experimental::scope_success;

  int counter = 0;
  auto d = [&counter] () { ++counter; };

  {
    scope_success s(d);
  }
  VERIFY( counter == 1 );

  try
  {
    scope_success s(d);
    throw 1;
  }
  catch (int)
  {
  }
  VERIFY( counter == 1 );

  {
    scope_success s(d);
    scope_success s2(std::move(s));
  }
  VERIFY( counter == 2 );

  {
    scope_success s(d);
    s.release();
  }
  VERIFY( counter == 2 );

  try
  {
    scope_success s(d);
    s.release();
    throw 1;
  }
  catch (int)
  {
  }
  VERIFY( counter == 2 );

  {
    da_funk = 0;
    scope_success<void(&)()> e(funk);
  }
  VERIFY( da_funk == 1 );

  static_assert(!std::is_move_assignable_v<scope_success<void(*)()>>);
  static_assert(!std::is_move_assignable_v<scope_success<void(&)()>>);
  static_assert(!std::is_move_assignable_v<scope_success<ThrowingCopy>>);
  static_assert(!std::is_move_assignable_v<scope_success<decltype(d)>>);

  {
    ThrowingCopy::counter = 0;
    try
    {
      scope_success<ThrowingCopy> s(ThrowingCopy::create());
      ThrowingCopy::nocopy = true;
      scope_success<ThrowingCopy> s2(std::move(s));
      VERIFY(false);
    }
    catch (int)
    {
    }
    VERIFY( ThrowingCopy::counter == 0 );

    scope_success<ThrowingCopy> s(ThrowingCopy::create());
    try
    {
      ThrowingCopy::nocopy = true;
      scope_success<ThrowingCopy> s2(std::move(s));
      VERIFY(false);
    }
    catch (int)
    {
    }
    VERIFY( ThrowingCopy::counter == 0 );
  }
  VERIFY( ThrowingCopy::counter == 1 );
}

int main()
{
  test_exit();
  test_fail();
  test_success();
}
