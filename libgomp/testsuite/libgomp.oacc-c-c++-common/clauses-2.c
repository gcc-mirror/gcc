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
        a[i] = 2.0;
        b[i] = 5.0;
    }

    d = (float *) acc_malloc (N * sizeof (float));
    acc_map_data (c, d, N * sizeof (float));

    fprintf (stderr, "CheCKpOInT\n");
#pragma acc parallel copyin (a[0:N]) present_or_create (c[0:N+1]) copyout (b[0:N])
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

    return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "Trying to map into device \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) object when \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) is already mapped" } */
/* { dg-shouldfail "" } */
