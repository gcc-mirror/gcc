/* Copy of update-1.c with self exchanged with host for #pragma acc update.  */

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <openacc.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int
main (int argc, char **argv)
{
    int N = 8;
    int NDIV2 = N / 2;
    float *a, *b, *c;
    float *d_a, *d_b, *d_c;
    int i;

    a = (float *) malloc (N * sizeof (float));
    b = (float *) malloc (N * sizeof (float));
    c = (float *) malloc (N * sizeof (float));

    d_a = (float *) acc_malloc (N * sizeof (float));
    d_b = (float *) acc_malloc (N * sizeof (float));
    d_c = (float *) acc_malloc (N * sizeof (float));

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
    }

    acc_map_data (a, d_a, N * sizeof (float));
    acc_map_data (b, d_b, N * sizeof (float));
    acc_map_data (c, d_c, N * sizeof (float));

#pragma acc update device (a[0:N], b[0:N])

#pragma acc parallel present (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc update self (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 3.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 1.0;
    }

#pragma acc update device (a[0:N], b[0:N])

#pragma acc parallel present (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc update self (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 5.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 1.0;
    }

#pragma acc update device (a[0:N], b[0:N])

#pragma acc parallel present (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc update host (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 5.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
        b[i] = 0.0;
    }

#pragma acc update device (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        a[i] = 9.0;
    }

#pragma acc parallel present (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc update self (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        if (a[i] != 6.0)
            abort ();

        if (b[i] != 6.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 7.0;
        b[i] = 2.0;
    }

#pragma acc update device (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        a[i] = 9.0;
    }

#pragma acc parallel present (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc update self (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        if (a[i] != 7.0)
            abort ();

        if (b[i] != 7.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 9.0;
    }

#pragma acc update device (a[0:N])

#pragma acc parallel present (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc update self (a[0:N], b[0:N])

    for (i = 0; i < N; i++)
    {
        if (a[i] != 9.0)
            abort ();

        if (b[i] != 9.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
    }

#pragma acc update device (a[0:N])

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
    }

#pragma acc update device (a[0:NDIV2])

#pragma acc parallel present (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc update self (a[0:N], b[0:N])

    for (i = 0; i < NDIV2; i++)
    {
        if (a[i] != 6.0)
            abort ();

        if (b[i] != 6.0)
            abort ();
    }

    for (i = NDIV2; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 5.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 0.0;
    }

#pragma acc update device (a[0:4])

#pragma acc parallel present (a[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            a[ii] = a[ii] + 1.0;
    }

#pragma acc update self (a[4:4])

    for (i = 0; i < NDIV2; i++)
    {
        if (a[i] != 0.0)
            abort ();
    }

    for (i = NDIV2; i < N; i++)
    {
        if (a[i] != 6.0)
            abort ();
    }

#pragma acc update self (a[0:4])

    for (i = 0; i < NDIV2; i++)
    {
        if (a[i] != 1.0)
            abort ();
    }

    for (i = NDIV2; i < N; i++)
    {
        if (a[i] != 6.0)
            abort ();
    }

    a[2] = 9;
    a[3] = 9;
    a[4] = 9;
    a[5] = 9;

#pragma acc update device (a[2:4])

#pragma acc parallel present (a[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            a[ii] = a[ii] + 1.0;
    }

#pragma acc update self (a[2:4])

    for (i = 0; i < 2; i++)
    {
      if (a[i] != 1.0)
	abort ();
    }

    for (i = 2; i < 6; i++)
    {
      if (a[i] != 10.0)
	abort ();
    }

    for (i = 6; i < N; i++)
    {
        if (a[i] != 6.0)
            abort ();
    }

    return 0;
}
