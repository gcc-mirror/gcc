// { dg-do run }

#include <vector>
#include <testsuite_hooks.h>

struct NoAssign
{
  NoAssign(int p) : val(p) {}
  const int val;
};

struct PrivateAssign
{
  PrivateAssign(int p) : val(p) {}
  PrivateAssign(const PrivateAssign& rhs) : val(rhs.val) {}

  int val;

private:
  PrivateAssign& operator=(const PrivateAssign&);
};

#if __cplusplus >= 201102L
struct DeletedAssign
{
  DeletedAssign(int p) : val(p) {}
  DeletedAssign(const DeletedAssign& rhs) : val(rhs.val) {}

  DeletedAssign& operator=(const DeletedAssign&) = delete;

  int val;
};
#endif

template<typename T>
void
testPR90129()
{
  std::vector<T> v;
  v.resize(5, T(5));
  VERIFY( v.size() == 5 );
  VERIFY( v.front().val == 5 );
  VERIFY( v.back().val == 5 );

  v.resize(10, T(10));
  VERIFY( v.size() == 10 );
  VERIFY( v.front().val == 5 );
  VERIFY( v.back().val == 10 );

  v.resize(7, T(7));
  VERIFY( v.size() == 7 );
  VERIFY( v.front().val == 5 );
  VERIFY( v.back().val == 10 );

  v.resize(3, T(3));
  VERIFY( v.size() == 3 );
  VERIFY( v.front().val == 5 );
  VERIFY( v.back().val == 5 );
}

int main()
{
  testPR90129<NoAssign>();
  testPR90129<PrivateAssign>();
#if __cplusplus >= 201102L
  testPR90129<DeletedAssign>();
#endif
  return 0;
}
