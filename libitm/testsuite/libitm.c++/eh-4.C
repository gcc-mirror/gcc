// Test that throwing out of an atomic_commit block commits the transaction.

// { dg-do run }
// { dg-options "-fgnu-tm" }

int main()
{
  static int i;
  bool caught = false;
  try {
    atomic_commit {
      i = 12;
      throw 42;
      i = 24;
    }
  } catch (int x) {
    caught = (x == 42);
  }
  if (!caught || i != 12)
    __builtin_abort();
}
