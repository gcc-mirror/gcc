/* OpenACC 'self' clause.  */

/* This is 'if-1.c' with 'self(!cond)' instead of 'if(cond)' on compute
   constructs.  */

#include <openacc.h>
#include <stdlib.h>
#include <stdbool.h>

#define N   32

int
main(int argc, char **argv)
{
    float *a, *b, *d_a, *d_b, exp, exp2;
    int i;
    const int one = 1;
    const int zero = 0;
    int n;

    a = (float *) malloc (N * sizeof (float));
    b = (float *) malloc (N * sizeof (float));
    d_a = (float *) acc_malloc (N * sizeof (float));
    d_b = (float *) acc_malloc (N * sizeof (float));

    for (i = 0; i < N; i++)
        a[i] = 4.0;

#pragma acc parallel copyin(a[0:N]) copyout(b[0:N]) self(0)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 5.0;
#else
    exp = 4.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 16.0;

#pragma acc parallel self(1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 17.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 8.0;

#pragma acc parallel copyin(a[0:N]) copyout(b[0:N]) self(!one)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 9.0;
#else
    exp = 8.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 22.0;

#pragma acc parallel self(!zero)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 23.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 16.0;

#pragma acc parallel copyin(a[0:N]) copyout(b[0:N]) self(false)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 17.0;
#else
    exp = 16.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 76.0;

#pragma acc parallel self(true)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 77.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 22.0;

    n = 1;

#pragma acc parallel copyin(a[0:N]) copyout(b[0:N]) self(!n)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 23.0;
#else
    exp = 22.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 18.0;

    n = 0;

#pragma acc parallel self(!n)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 19.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 49.0;

    n = 1;

#pragma acc parallel copyin(a[0:N]) copyout(b[0:N]) self(!(n + n))
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 50.0;
#else
    exp = 49.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 38.0;

    n = 0;

#pragma acc parallel self(!(n + n))
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 39.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 91.0;

#pragma acc parallel copyin(a[0:N]) copyout(b[0:N]) self(!(-2))
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 92.0;
#else
    exp = 91.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 43.0;

#pragma acc parallel copyin(a[0:N]) copyout(b[0:N]) self(one != 1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 44.0;
#else
    exp = 43.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 87.0;

#pragma acc parallel self(one != 0)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 88.0)
            abort();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 9.0;
    }

#if ACC_MEM_SHARED
    exp = 0.0;
    exp2 = 0.0;
#else
    acc_map_data (a, d_a, N * sizeof (float));
    acc_map_data (b, d_b, N * sizeof (float));
    exp = 3.0;
    exp2 = 9.0;
#endif

#pragma acc update device(a[0:N], b[0:N]) if(1)

    for (i = 0; i < N; i++)
    {
        a[i] = 0.0;
        b[i] = 0.0;
    }

#pragma acc update host(a[0:N], b[0:N]) if(1)

    for (i = 0; i < N; i++)
    {
        if (a[i] != exp)
            abort();

        if (b[i] != exp2)
            abort();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 6.0;
        b[i] = 12.0;
    }

#pragma acc update device(a[0:N], b[0:N]) if(0)

    for (i = 0; i < N; i++)
    {
        a[i] = 0.0;
        b[i] = 0.0;
    }

#pragma acc update host(a[0:N], b[0:N]) if(1)

    for (i = 0; i < N; i++)
    {
        if (a[i] != exp)
            abort();

        if (b[i] != exp2)
            abort();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 26.0;
        b[i] = 21.0;
    }

#pragma acc update device(a[0:N], b[0:N]) if(1)

    for (i = 0; i < N; i++)
    {
        a[i] = 0.0;
        b[i] = 0.0;
    }

#pragma acc update host(a[0:N], b[0:N]) if(0)

    for (i = 0; i < N; i++)
    {
        if (a[i] != 0.0)
            abort();

        if (b[i] != 0.0)
            abort();
    }

#if !ACC_MEM_SHARED
    acc_unmap_data (a);
    acc_unmap_data (b);
#endif

    acc_free (d_a);
    acc_free (d_b);

    for (i = 0; i < N; i++)
    {
        a[i] = 4.0;
        b[i] = 0.0;
    }

#pragma acc data copyin(a[0:N]) copyout(b[0:N]) if(1)
{
#pragma acc parallel present(a[0:N])
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            b[ii] = a[ii];
        }
    }
}

    for (i = 0; i < N; i++)
    {
        if (b[i] != 4.0)
            abort();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 8.0;
        b[i] = 1.0;
    }

#pragma acc data copyin(a[0:N]) copyout(b[0:N]) if(0)
{
#if !ACC_MEM_SHARED
    if (acc_is_present (a, N * sizeof (float)))
        abort ();
#endif

#if !ACC_MEM_SHARED
    if (acc_is_present (b, N * sizeof (float)))
        abort ();
#endif
}

    for (i = 0; i < N; i++)
    {
        a[i] = 18.0;
        b[i] = 21.0;
    }

#pragma acc data copyin(a[0:N]) if(1)
{
#if !ACC_MEM_SHARED
    if (!acc_is_present (a, N * sizeof (float)))
        abort ();
#endif

#pragma acc data copyout(b[0:N]) if(0)
    {
#if !ACC_MEM_SHARED
        if (acc_is_present (b, N * sizeof (float)))
            abort ();
#endif

#pragma acc data copyout(b[0:N]) if(1)
        {
#pragma acc parallel present(a[0:N]) present(b[0:N])
            {
                int ii;

                for (ii = 0; ii < N; ii++)
                {
                    b[ii] = a[ii];
                }
            }
        }

#if !ACC_MEM_SHARED
        if (acc_is_present (b, N * sizeof (float)))
            abort ();
#endif
    }
}

    for (i = 0; i < N; i++)
    {
        if (b[i] != 18.0)
            abort ();
    }

#pragma acc enter data copyin (b[0:N]) if (0)

#if !ACC_MEM_SHARED
    if (acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc exit data delete (b[0:N]) if (0)

#pragma acc enter data copyin (b[0:N]) if (1)

#if !ACC_MEM_SHARED
    if (!acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc exit data delete (b[0:N]) if (1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc enter data copyin (b[0:N]) if (zero)

#if !ACC_MEM_SHARED
    if (acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc exit data delete (b[0:N]) if (zero)

#pragma acc enter data copyin (b[0:N]) if (one)

#if !ACC_MEM_SHARED
    if (!acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc exit data delete (b[0:N]) if (one)

#if !ACC_MEM_SHARED
    if (acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc enter data copyin (b[0:N]) if (one == 0)

#if !ACC_MEM_SHARED
    if (acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc exit data delete (b[0:N]) if (one == 0)

#pragma acc enter data copyin (b[0:N]) if (one == 1)

#if !ACC_MEM_SHARED
    if (!acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

#pragma acc exit data delete (b[0:N]) if (one == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b, N * sizeof (float)))
	abort ();
#endif

    for (i = 0; i < N; i++)
        a[i] = 4.0;

#pragma acc kernels copyin(a[0:N]) copyout(b[0:N]) self(0)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 5.0;
#else
    exp = 4.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 16.0;

#pragma acc kernels self(1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 17.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 8.0;

#pragma acc kernels copyin(a[0:N]) copyout(b[0:N]) self(!one)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 9.0;
#else
    exp = 8.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 22.0;

#pragma acc kernels self(!zero)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 23.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 16.0;

#pragma acc kernels copyin(a[0:N]) copyout(b[0:N]) self(false)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 17.0;
#else
    exp = 16.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 76.0;

#pragma acc kernels self(true)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 77.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 22.0;

    n = 1;

#pragma acc kernels copyin(a[0:N]) copyout(b[0:N]) self(!n)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 23.0;
#else
    exp = 22.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 18.0;

    n = 0;

#pragma acc kernels self(!n)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 19.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 49.0;

    n = 1;

#pragma acc kernels copyin(a[0:N]) copyout(b[0:N]) self((n + n) == 0)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 50.0;
#else
    exp = 49.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 38.0;

    n = 0;

#pragma acc kernels self(!(n + n))
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 39.0)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 91.0;

#pragma acc kernels copyin(a[0:N]) copyout(b[0:N]) self(!(-2))
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 92.0;
#else
    exp = 91.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 43.0;

#pragma acc kernels copyin(a[0:N]) copyout(b[0:N]) self(one != 1)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

#if ACC_MEM_SHARED
    exp = 44.0;
#else
    exp = 43.0;
#endif

    for (i = 0; i < N; i++)
    {
        if (b[i] != exp)
            abort();
    }

    for (i = 0; i < N; i++)
        a[i] = 87.0;

#pragma acc kernels self(one != 0)
    {
        int ii;

        for (ii = 0; ii < N; ii++)
        {
            if (acc_on_device (acc_device_host))
                b[ii] = a[ii] + 1;
            else
                b[ii] = a[ii];
        }
    }

    for (i = 0; i < N; i++)
    {
        if (b[i] != 88.0)
            abort();
    }

    for (i = 0; i < N; i++)
    {
        a[i] = 3.0;
        b[i] = 9.0;
    }

#if ACC_MEM_SHARED
    exp = 0.0;
    exp2 = 0.0;
#else
    acc_map_data (a, d_a, N * sizeof (float));
    acc_map_data (b, d_b, N * sizeof (float));
    exp = 3.0;
    exp2 = 9.0;
#endif

    return 0;
}
