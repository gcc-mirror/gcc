/* PR c/106981 */

int
main ()
{
  int a[0x101];
  unsigned int b = 0x100;
  if ((unsigned char) b || (unsigned short) b != 0x100)
    return 0;
  a[0] = 0;
  a[0x100] = 42;
  #pragma omp atomic update
  a[(unsigned char) b] = a[(unsigned short) b] + a[(unsigned char) b];
  #pragma omp atomic update
  a[(unsigned char) b] = a[(unsigned char) b] + a[(unsigned short) b];
  if (a[0] != 84 || a[0x100] != 42)
    __builtin_abort ();
  return 0;
}
