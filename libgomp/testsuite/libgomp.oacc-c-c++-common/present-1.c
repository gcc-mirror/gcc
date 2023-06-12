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

    d = (float *) acc_malloc (N * sizeof (float));
    acc_map_data (c, d, N * sizeof (float));

    fprintf (stderr, "CheCKpOInT\n");
#pragma acc data present (a[0:N]) present (c[0:N]) present (b[0:N])
    {
#pragma acc parallel
        {
            int ii;

            for (ii = 0; ii < N; ii++)
            {
                c[ii] = a[ii];
                b[ii] = c[ii];
            }
        }
    }

    d = (float *) acc_deviceptr (c);
    acc_unmap_data (c);
    acc_free (d);

    free (a);
    free (b);
    free (c);

    return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "libgomp: present clause: not present on the device \\(addr: 0x\[0-9a-f\]+, size: \[0-9\]+ \\(0x\[0-9a-f\]+\\), dev: \[0-9\]+\\\)" } */
/* { dg-shouldfail "" } */
