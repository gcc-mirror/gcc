int
main ()
{
  int i;
  static int temp;

#pragma acc parallel reduction(+:temp)
  {
    temp++;
  }

  return 0;
}
