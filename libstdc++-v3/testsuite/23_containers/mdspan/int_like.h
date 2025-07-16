#ifndef TEST_MDSPAN_INT_LIKE_H
#define TEST_MDSPAN_INT_LIKE_H

enum class CustomIndexKind
{
  Const,
  Throwing,
  Mutating,
};

template<CustomIndexKind Kind>
  class CustomIndexType
  {
  public:
    explicit
    CustomIndexType(int i)
    : _M_i(i)
    { }

    CustomIndexType() = delete;
    CustomIndexType(const CustomIndexType&) = delete;
    CustomIndexType(CustomIndexType&&) = delete;

    const CustomIndexType&
    operator=(const CustomIndexType&) = delete;

    const CustomIndexType&
    operator=(CustomIndexType&&) = delete;

    constexpr
    operator int() const noexcept
    requires (Kind == CustomIndexKind::Const)
    { return _M_i; }

    constexpr
    operator int() const
    requires (Kind == CustomIndexKind::Throwing)
    { return _M_i; }

    constexpr
    operator int() noexcept
    requires (Kind == CustomIndexKind::Mutating)
    { return _M_i; }

  private:
    int _M_i;
  };

using IntLike = CustomIndexType<CustomIndexKind::Const>;
using ThrowingInt = CustomIndexType<CustomIndexKind::Throwing>;
using MutatingInt = CustomIndexType<CustomIndexKind::Mutating>;

struct NotIntLike
{ };

#endif // TEST_MDSPAN_INT_LIKE_H
