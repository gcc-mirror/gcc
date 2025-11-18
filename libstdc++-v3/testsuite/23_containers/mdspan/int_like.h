#ifndef TEST_MDSPAN_INT_LIKE_H
#define TEST_MDSPAN_INT_LIKE_H

enum class CustomIndexKind
{
  Const,
  Throwing,
  Mutating,
  RValue,
};

template<CustomIndexKind Kind, bool Copyable = false>
  class CustomIndexType
  {
  public:
    explicit
    CustomIndexType(int i)
    : value(i)
    { }

    CustomIndexType() requires(Copyable) = default;
    CustomIndexType() requires(!Copyable) = delete;

    CustomIndexType(const CustomIndexType&) requires(Copyable) = default;
    CustomIndexType(const CustomIndexType&) requires(!Copyable) = delete;

    CustomIndexType(CustomIndexType&&) requires(Copyable) = default;
    CustomIndexType(CustomIndexType&&) requires(!Copyable) = delete;

    CustomIndexType&
    operator=(const CustomIndexType&) requires(Copyable) = default;
    CustomIndexType&
    operator=(const CustomIndexType&) requires(!Copyable) = delete;

    CustomIndexType&
    operator=(CustomIndexType&&) requires(Copyable) = default;
    CustomIndexType&
    operator=(CustomIndexType&&) requires(!Copyable) = delete;

    constexpr
    operator int() const noexcept
    requires (Kind == CustomIndexKind::Const)
    { return value; }

    constexpr
    operator int() const
    requires (Kind == CustomIndexKind::Throwing)
    { return value; }

    constexpr
    operator int() noexcept
    requires (Kind == CustomIndexKind::Mutating)
    { return value; }

    constexpr
    operator int() && noexcept
    requires (Kind == CustomIndexKind::RValue)
    { return value; }

  private:
    int value;
  };

using IntLike = CustomIndexType<CustomIndexKind::Const>;
using ThrowingInt = CustomIndexType<CustomIndexKind::Throwing>;
using MutatingInt = CustomIndexType<CustomIndexKind::Mutating>;
using RValueInt = CustomIndexType<CustomIndexKind::RValue>;

struct NotIntLike
{ };

#endif // TEST_MDSPAN_INT_LIKE_H
