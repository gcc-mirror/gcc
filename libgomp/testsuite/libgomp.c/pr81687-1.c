/* PR c/81687 */
/* { dg-do link } */
/* { dg-additional-options "-O2" } */

extern int printf (const char *, ...);

int
main ()
{
  #pragma omp parallel
  {
   lab1:
    printf ("lab1=%p\n", (void *)(&&lab1));
  }
 lab2:
  #pragma omp parallel
  {
   lab3:
    printf ("lab2=%p\n", (void *)(&&lab2));
  }
  printf ("lab3=%p\n", (void *)(&&lab3));
  return 0;
}
