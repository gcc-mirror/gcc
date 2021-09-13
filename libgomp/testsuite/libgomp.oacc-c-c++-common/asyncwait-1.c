/* { dg-do run } */
/* { dg-additional-options "-DUSE_CUDA_H" { target openacc_cuda } } */
/* { dg-additional-options "-lcuda" { target { openacc_nvidia_accel_selected && openacc_cuda } } } */

#include <openacc.h>
#include <stdlib.h>
#if defined ACC_DEVICE_TYPE_nvidia && defined USE_CUDA_H
#include "cuda.h"
#endif

#include <stdio.h>
#include <sys/time.h>

int
main (int argc, char **argv)
{
#if defined ACC_DEVICE_TYPE_nvidia && defined USE_CUDA_H
    CUresult r;
    CUstream stream1;
#endif
    int N = 128; //1024 * 1024;
    float *a, *b, *c, *d, *e;
    int i;
    int nbytes;

#if defined ACC_DEVICE_TYPE_nvidia && defined USE_CUDA_H
    acc_init (acc_device_nvidia);
#endif

    nbytes = N * sizeof (float);

    a = (float *) malloc (nbytes);
    b = (float *) malloc (nbytes);
    c = (float *) malloc (nbytes);
    d = (float *) malloc (nbytes);
    e = (float *) malloc (nbytes);

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copyin (N)
    {

#pragma acc parallel async
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc wait

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 3.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 2.0;
        b[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 2.0)
            abort ();

        if (b[i] != 2.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }


#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 9.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 2.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc parallel wait (1) async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            e[ii] = a[ii] + b[ii] + c[ii] + d[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 2.0)
            abort ();

        if (b[i] != 4.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();

        if (e[i] != 11.0)
            abort ();
    }


#if defined ACC_DEVICE_TYPE_nvidia && defined USE_CUDA_H
    r = cuStreamCreate (&stream1, CU_STREAM_NON_BLOCKING);
    if (r != CUDA_SUCCESS)
    {
        fprintf (stderr, "cuStreamCreate failed: %d\n", r);
        abort ();
    }

    acc_set_cuda_stream (1, stream1);
#endif

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 0.0;
    }

#pragma acc data copy (a[0:N], b[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 5.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 7.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 7.0)
            abort ();

        if (b[i] != 49.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc parallel wait (1) async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            e[ii] = a[ii] + b[ii] + c[ii] + d[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 9.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();

        if (e[i] != 17.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 4.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copyin (a[0:N], b[0:N], c[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc update host (a[0:N], b[0:N], c[0:N]) wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 4.0)
            abort ();

        if (b[i] != 16.0)
            abort ();

        if (c[i] != 4.0)
            abort ();
    }


    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copyin (a[0:N], b[0:N], c[0:N]) copyin (N)
    {

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc parallel async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc update host (a[0:N], b[0:N], c[0:N]) async (1)

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 25.0)
            abort ();

        if (c[i] != 4.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copyin (N)
    {

#pragma acc kernels async
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc wait

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 3.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 2.0;
        b[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 2.0)
            abort ();

        if (b[i] != 2.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }


#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 9.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 2.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc kernels wait (1) async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            e[ii] = a[ii] + b[ii] + c[ii] + d[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 2.0)
            abort ();

        if (b[i] != 4.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();

        if (e[i] != 11.0)
            abort ();
    }


#if defined ACC_DEVICE_TYPE_nvidia && defined USE_CUDA_H
    r = cuStreamCreate (&stream1, CU_STREAM_NON_BLOCKING);
    if (r != CUDA_SUCCESS)
    {
        fprintf (stderr, "cuStreamCreate failed: %d\n", r);
        abort ();
    }

    acc_set_cuda_stream (1, stream1);
#endif

    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 0.0;
    }

#pragma acc data copy (a[0:N], b[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 5.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 7.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 7.0)
            abort ();

        if (b[i] != 49.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];
    }

#pragma acc kernels wait (1) async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            e[ii] = a[ii] + b[ii] + c[ii] + d[ii];
    }

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 3.0)
            abort ();

        if (b[i] != 9.0)
            abort ();

        if (c[i] != 4.0)
            abort ();

        if (d[i] != 1.0)
            abort ();

        if (e[i] != 17.0)
            abort ();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 4.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copyin (a[0:N], b[0:N], c[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc update host (a[0:N], b[0:N], c[0:N]) wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 4.0)
            abort ();

        if (b[i] != 16.0)
            abort ();

        if (c[i] != 4.0)
            abort ();
    }


    for (i = 0; i < N; i++)
    {
        a[i] = 5.0;
        b[i] = 0.0;
        c[i] = 0.0;
        d[i] = 0.0;
        e[i] = 0.0;
    }

#pragma acc data copyin (a[0:N], b[0:N], c[0:N]) copyin (N)
    {

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];
    }

#pragma acc kernels async (1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
            c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];
    }

#pragma acc update host (a[0:N], b[0:N], c[0:N]) async (1)

#pragma acc wait (1)

    }

    for (i = 0; i < N; i++)
    {
        if (a[i] != 5.0)
            abort ();

        if (b[i] != 25.0)
            abort ();

        if (c[i] != 4.0)
            abort ();
    }

#if defined ACC_DEVICE_TYPE_nvidia && defined USE_CUDA_H
    acc_shutdown (acc_device_nvidia);
#endif

    return 0;
}
