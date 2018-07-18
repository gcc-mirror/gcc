/* { dg-do run } */

#include <stdlib.h>

int
main (int argc, char *argv[])
{
#define N 10
    char a[N];
    int i;

    for (i = 0; i < N; ++i)
       a[i] = 0;

#pragma acc data copy (a)
    {
#pragma acc parallel present (a)
        {
            int j;

            for (j = 0; j < N; ++j)
                a[j] = j;
        }
    }

    for (i = 0; i < N; ++i)
    {
        if (a[i] != i)
            abort ();
    }

    for (i = 0; i < N; ++i)
       a[i] = 0;

#pragma acc data copy (a)
    {
#pragma acc kernels present (a)
        {
            int j;

            for (j = 0; j < N; ++j)
                a[j] = j;
        }
    }

    for (i = 0; i < N; ++i)
    {
        if (a[i] != i)
            abort ();
    }

    for (i = 0; i < N; ++i)
        a[i] = 0;

#pragma acc data copy (a)
    {
#pragma acc data present (a)
        {
#pragma acc parallel present (a)
            {
                int j;

                for (j = 0; j < N; ++j)
                    a[j] = j;
            }
        }
    }

    for (i = 0; i < N; ++i)
    {
        if (a[i] != i)
            abort ();
    }

#pragma acc data copy (a)
    {
#pragma acc data present (a)
        {
#pragma acc kernels present (a)
            {
                int j;

                for (j = 0; j < N; ++j)
                    a[j] = j;
            }
        }
    }

    for (i = 0; i < N; ++i)
    {
        if (a[i] != i)
            abort ();
    }

    for (i = 0; i < N; ++i)
        a[i] = 0;

#pragma acc enter data copyin (a)

#pragma acc data present (a)
    {
#pragma acc parallel present (a)
        {
            int j;

            for (j = 0; j < N; ++j)
                a[j] = j;
        }
    }

#pragma acc exit data copyout (a)

    for (i = 0; i < N; ++i)
    {
        if (a[i] != i)
            abort ();
    }

#pragma acc enter data copyin (a)

#pragma acc data present (a)
    {
#pragma acc kernels present (a)
        {
            int j;

            for (j = 0; j < N; ++j)
                a[j] = j;
        }
    }

#pragma acc exit data copyout (a)

    for (i = 0; i < N; ++i)
    {
        if (a[i] != i)
            abort ();
    }


#pragma acc enter data create (a)

#pragma acc parallel default (present)
    {
      for (int j = 0; j < N; ++j)
	a[j] = j + 1;
    }

#pragma acc update host (a)

    for (i = 0; i < N; ++i)
      {
        if (a[i] != i + 1)
	  abort ();
      }

#pragma acc kernels default (present)
    {
      for (int j = 0; j < N; ++j)
	a[j] = j + 2;
    }

#pragma acc exit data copyout (a)

    for (i = 0; i < N; ++i)
      {
        if (a[i] != i + 2)
	  abort ();
      }

    return 0;
}
