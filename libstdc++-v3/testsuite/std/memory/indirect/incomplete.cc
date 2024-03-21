// { dg-do compile { target c++26 } }

#include <memory>

struct Incomplete;
bool operator==(const Incomplete&, const Incomplete&);
std::strong_ordering operator<=>(const Incomplete&, const Incomplete&);

template<>
struct std::hash<Incomplete>
{
  static std::size_t operator()(const Incomplete& c);
};

std::indirect<Incomplete>*
test_move(std::indirect<Incomplete>& i1, std::indirect<Incomplete>& i2)
{
  i2.swap(i2);
  return new std::indirect<Incomplete>(std::move(i1));
}

void
test_relops(std::indirect<Incomplete> const& i1, Incomplete const& o)
{
  void(i1 == i1);
  void(i1 < i1);
  void(i1 >= i1);

  void(i1 != o);
  void(i1 < o);
}

void
test_hash(std::indirect<Incomplete> const& i1)
{
  std::hash<std::indirect<Incomplete>> h;
  h(i1);
}
