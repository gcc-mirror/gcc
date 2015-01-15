#include <stdio.h>
#include <stdlib.h>

int
main (void)
{
  int s1 = 2, s2 = 5, v1 = 2, v2 = 5;
  int n = 100;
  int i;

#pragma acc parallel vector_length (1000)
#pragma acc loop reduction (+:s1, s2)
  for (i = 0; i < n; i++)
    {
      s1 = s1 + 3;
      s2 = s2 + 2;
    }

  for (i = 0; i < n; i++)
    {
      v1 = v1 + 3;
      v2 = v2 + 2;
    }
  
  if (s1 != v1)
    abort ();
  
  if (s2 != v2)
    abort ();
    
  return 0;
}