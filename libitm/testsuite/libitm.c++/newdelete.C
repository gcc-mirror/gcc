// { dg-do run }

int main ()
{
  atomic_commit {
    int* data = new int;
    delete data;
    data = new int[10];
    delete[] data;
  }
  return 0;
}
