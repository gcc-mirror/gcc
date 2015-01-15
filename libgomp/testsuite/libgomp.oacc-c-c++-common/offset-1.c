/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <openacc.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int
main(int argc, char **argv)
{
    int N = 8;
    float *a, *b;
    int i;

    a = (float *) malloc(N * sizeof (float));
    b = (float *) malloc(N * sizeof (float));

    for (i = 0; i < N; i++)
    {
        a[i] = 2.0;
        b[i] = 5.0;
    }

#pragma acc parallel copyin(a[2:4]) copyout(b[2:4])
    {
        b[2] = a[2];
        b[3] = a[3];
    }

    for (i = 2; i < 4; i++)
    {
        if (a[i] != 2.0)
            abort();

        if (b[i] != 2.0)
            abort();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 1.0;
    }

#pragma acc parallel copyin(a[0:4]) copyout(b[0:4])
    {
        b[0] = a[0];
        b[1] = a[1];
        b[2] = a[2];
        b[3] = a[3];
    }

    for (i = 0; i < 4; i++)
    {
        if (a[i] != 3.0)
            abort();

        if (b[i] != 3.0)
            abort();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 9.0;
        b[i] = 6.0;
    }

#pragma acc parallel copyin(a[0:4]) copyout(b[4:4])
    {
        b[4] = a[0];
        b[5] = a[1];
        b[6] = a[2];
        b[7] = a[3];
    }

    for (i = 0; i < 4; i++)
    {
        if (a[i] != 9.0)
            abort();
    }

    for (i = 4; i < 8; i++)
    {
        if (b[i] != 9.0)
            abort();
    }

    if (acc_is_present (a, (N * sizeof (float))))
      abort();

    if (acc_is_present (b, (N * sizeof (float))))
      abort();

    return 0;
}
