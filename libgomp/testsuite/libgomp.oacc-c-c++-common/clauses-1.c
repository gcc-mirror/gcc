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
    float *a, *b, *c, *d;
    int i;

    a = (float *) malloc (N * sizeof (float));
    b = (float *) malloc (N * sizeof (float));
    c = (float *) malloc (N * sizeof (float));

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
    }

#pragma acc parallel copyin (a[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 3.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 1.0;
    }

#pragma acc parallel copyin (a[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 5.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
        b[i] = 0.0;
    }

    d = (float *) acc_copyin (&a[0], N * sizeof (float));

    for (i = 0; i < N; i++)
    {
        a[i] = 9.0;
    }

#pragma acc parallel present_or_copyin (a[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 6.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    acc_delete (&a[0], N * sizeof (float));

    if (acc_is_present (&a[0], N * sizeof (float)))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
        b[i] = 0.0;
    }

#pragma acc parallel copyin (a[0:N]) present_or_copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 6.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 2.0;
    }

    d = (float *) acc_copyin (&b[0], N * sizeof (float));

#pragma acc parallel copyin (a[0:N]) present_or_copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 2.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    acc_delete (&b[0], N * sizeof (float));

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 4.0;
    }

#pragma acc parallel copy (a[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            a[ii] = a[ii] + 1;
            b[ii] = a[ii] + 2;
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 4.0)
            abort ();

        if (b[i] != 6.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 4.0;
        b[i] = 7.0;
    }

#pragma acc parallel present_or_copy (a[0:N]) present_or_copy (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            a[ii] = a[ii] + 1;
            b[ii] = b[ii] + 2;
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 9.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 7.0;
    }

    d = (float *) acc_copyin (&a[0], N * sizeof (float));
    d = (float *) acc_copyin (&b[0], N * sizeof (float));

#pragma acc parallel present_or_copy (a[0:N]) present_or_copy (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            a[ii] = a[ii] + 1;
            b[ii] = b[ii] + 2;
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 7.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (!acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    acc_delete (&a[0], N * sizeof (float));

    if (acc_is_present (&a[0], N * sizeof (float)))
      abort ();

    acc_delete (&b[0], N * sizeof (float));

    if (acc_is_present (&b[0], N * sizeof (float)))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 7.0;
    }

#pragma acc parallel copyin (a[0:N]) create (c[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            c[ii] = a[ii];
            b[ii] = c[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 3.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&c[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 4.0;
        b[i] = 8.0;
    }

#pragma acc parallel copyin (a[0:N]) present_or_create (c[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            c[ii] = a[ii];
            b[ii] = c[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 4.0)
            abort ();

        if (b[i] != 4.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&c[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 2.0;
        b[i] = 5.0;
    }

    d = (float *) acc_malloc (N * sizeof (float));
    acc_map_data (c, d, N * sizeof (float));

#pragma acc parallel copyin (a[0:N]) present_or_create (c[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            c[ii] = a[ii];
            b[ii] = c[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 2.0)
            abort ();

        if (b[i] != 2.0)
            abort ();
    }

    if (acc_is_present (a, (N * sizeof (float))))
      abort ();

    if (acc_is_present (b, (N * sizeof (float))))
      abort ();

    if (!acc_is_present (c, (N * sizeof (float))))
      abort ();

    d = (float *) acc_deviceptr (c);

    acc_unmap_data (c);

    acc_free (d);

    for (i = 0; i < N; i++)
    {
        a[i] = 4.0;
        b[i] = 8.0;
    }

    d = (float *) acc_malloc (N * sizeof (float));
    acc_map_data (c, d, N * sizeof (float));

#pragma acc parallel copyin (a[0:N]) present (c[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            c[ii] = a[ii];
            b[ii] = c[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 4.0)
            abort ();

        if (b[i] != 4.0)
            abort ();
    }

    if (acc_is_present (a, (N * sizeof (float))))
      abort ();

    if (acc_is_present (b, (N * sizeof (float))))
      abort ();

    if (!acc_is_present (c, (N * sizeof (float))))
      abort ();

    acc_unmap_data (c);

    acc_free (d);

    for (i = 0; i < N; i++)
    {
        a[i] = 4.0;
        b[i] = 8.0;
    }

    acc_copyin (a, N * sizeof (float));

    d = (float *) acc_malloc (N * sizeof (float));
    acc_map_data (b, d, N * sizeof (float));

    d = (float *) acc_malloc (N * sizeof (float));
    acc_map_data (c, d, N * sizeof (float));

#pragma acc parallel present (a[0:N]) present (c[0:N]) present (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            c[ii] = a[ii];
            b[ii] = c[ii];
        }
    }

    if (!acc_is_present (a, (N * sizeof (float))))
      abort ();

    if (!acc_is_present (b, (N * sizeof (float))))
      abort ();

    if (!acc_is_present (c, (N * sizeof (float))))
      abort ();

    d = (float *) acc_deviceptr (b);

    acc_memcpy_from_device (b, d, N * sizeof (float));

    for (i = 0; i < N; i++)
    {
        if (a[i] != 4.0)
            abort ();

        if (b[i] != 4.0)
            abort ();
    }

    acc_delete (a, N * sizeof (float));

    if (acc_is_present (a, N * sizeof (float)))
      abort ();

    d = (float *) acc_deviceptr (b);

    acc_unmap_data (b);

    if (acc_is_present (b, N * sizeof (float)))
      abort ();

    acc_free (d);

    d = (float *) acc_deviceptr (c);

    acc_unmap_data (c);

    if (acc_is_present (c, N * sizeof (float)))
      abort ();

    acc_free (d);

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 6.0;
    }

    d = (float *) acc_malloc (N * sizeof (float));

#pragma acc parallel copyin (a[0:N]) deviceptr (d) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            d[ii] = a[ii];
            b[ii] = d[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 3.0)
            abort ();
    }

    if (acc_is_present (a, (N * sizeof (float))))
      abort ();

    if (acc_is_present (b, (N * sizeof (float))))
      abort ();

    acc_free (d);

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
        b[i] = 0.0;
    }

    d = (float *) acc_copyin (&a[0], N * sizeof (float));

    for (i = 0; i < N; i++)
    {
        a[i] = 9.0;
    }

#pragma acc parallel pcopyin (a[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 6.0)
            abort ();
    }

    if (!acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    acc_delete (&a[0], N * sizeof (float));

    if (acc_is_present (&a[0], N * sizeof (float)))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
        b[i] = 0.0;
    }

#pragma acc parallel copyin (a[0:N]) pcopyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 6.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
        b[i] = 0.0;
    }

#pragma acc parallel pcopy (a[0:N], b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 6.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 7.0;
    }

#pragma acc parallel copyin (a[0:N]) pcreate (c[0:N]) copyout (b[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            c[ii] = a[ii];
            b[ii] = c[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 5.0)
            abort ();
    }

    if (acc_is_present (&a[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&b[0], (N * sizeof (float))))
      abort ();

    if (acc_is_present (&c[0], (N * sizeof (float))))
      abort ();

    return 0;
}
