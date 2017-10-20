char __attribute__ ((noipa))
toup (char X)
{
  if (X >= 97 && X <= 122)
    return X - 32;
  else
    return X;
}

char __attribute__ ((noipa))
target_toup (char X)
{
  char r;
#pragma omp target map(to:X) map(from:r)
  {
    if (X >= 97 && X <= 122)
      r = X - 32;
    else
      r = X;
  }
  return r;
}

int main (int argc, char **argv)
{
  char a = 'a';
  if (toup (a) != target_toup (a))
    __builtin_abort ();
  a = 'Z';
  if (toup (a) != target_toup (a))
    __builtin_abort ();
  a = 5;
  if (toup (a) != target_toup (a))
    __builtin_abort ();

  return 0;
}
