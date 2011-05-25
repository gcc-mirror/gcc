// { dg-options "-Wno-deprecated" }

#include <backward/hashtable.h>
#include <utility>

struct modulo2_hash
{
  size_t operator()(int const key) const
  {
    return key % 2;
  }
};

struct modulo2_eq
{
  bool operator()(int const left, int const right) const
  {
    return left % 2 == right % 2;
  }
};

int main()
{
  typedef std::_Select1st<std::pair<int const, int> > extract_type;
  typedef
    __gnu_cxx::hashtable<std::pair<int const, int>, int, modulo2_hash,
			 extract_type, modulo2_eq, std::allocator<int> >
      table_type;
  table_type table(4, modulo2_hash(), modulo2_eq(), extract_type(),
		   std::allocator<int>());

  table.insert_equal(std::make_pair(2, 1));
  table_type::iterator it(table.insert_equal(std::make_pair(4, 2)));
  table.erase(it->first);
}
