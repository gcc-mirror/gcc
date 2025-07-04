#ifndef TEST_MDSPAN_INT_LIKE_H
#define TEST_MDSPAN_INT_LIKE_H

class IntLike
{
public:
  explicit
  IntLike(int i)
  : _M_i(i)
  { }

  IntLike() = delete;
  IntLike(const IntLike&) = delete;
  IntLike(IntLike&&) = delete;

  const IntLike&
  operator=(const IntLike&) = delete;

  const IntLike&
  operator=(IntLike&&) = delete;

  constexpr
  operator int() const noexcept
  { return _M_i; }

private:
  int _M_i;
};

#endif // TEST_MDSPAN_INT_LIKE_H
