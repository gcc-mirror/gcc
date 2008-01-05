/* PR tree-optimization/34618 */
/* { dg-do compile } */
/* { dg-options "-O3 -fmudflap" } */

int a[16];

void
foo ()
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = i;
}
