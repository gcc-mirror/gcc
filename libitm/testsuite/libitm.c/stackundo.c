int __attribute__((noinline)) test2(int x[1000])
{
  int i;
  return x[12];
}

int __attribute__((noinline)) test1()
{
  int x[1000], i;

  for (i = 0; i < 1000; i++)
    x[i] = i;
  return test2(x);
}

int main()
{
  __transaction_atomic {
    if (test1() !=0)
      __transaction_cancel;
  }
  return 0;
}
