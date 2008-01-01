// PR 26442

struct A
{
  A();
};

int main()
{
  if (0)
    A();
  return 0;
}
