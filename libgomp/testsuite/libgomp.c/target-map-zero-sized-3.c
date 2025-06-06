int
main ()
{
  int i, n;
  int data[] = {1,2};
  struct S {
    int **ptrset;
    int **ptrset2;
  };

  /* This is the same as target-map-zero-sized-3.c, but by mixing
     mapped and non-mapped items, the mapping before the ATTACH
     might (or here: is) not actually associated with the the
     pointer used for attaching. Thus, if one does a simple

       if (openmp_p
	   && (pragma_kind & GOMP_MAP_VARS_ENTER_DATA)
	   && mapnum == 1)
     check in target.c's gomp_map_vars_internal will fail
     as mapnum > 1 but still the map associated with this
     ATTACH is in a different set.  */

  struct S s1, *sptr1;
  s1.ptrset = (int **) __builtin_malloc (sizeof(void*) * 3);
  s1.ptrset2 = (int **) __builtin_malloc (sizeof(void*) * 3);
  s1.ptrset[0] = data;
  s1.ptrset[1] = data;
  s1.ptrset[2] = data;
  s1.ptrset2[0] = data;
  s1.ptrset2[1] = data;
  s1.ptrset2[2] = data;
  sptr1 = &s1;

  i = 1;
  n = 0;
  #pragma omp target enter data map(data)
  #pragma omp target enter data map(sptr1[:1], sptr1->ptrset[:3], sptr1->ptrset2[:3])
  #pragma omp target enter data map(sptr1->ptrset[i][:n], sptr1->ptrset2[i][:n])

  #pragma omp target map(sptr1->ptrset[i][:n], sptr1->ptrset2[i][:n])
    if (sptr1->ptrset2[1][0] != 1 || sptr1->ptrset2[1][1] != 2)
      __builtin_abort ();

  #pragma omp target exit data map(sptr1->ptrset[i][:n], sptr1->ptrset2[i][:n])
  #pragma omp target exit data map(sptr1[:1], sptr1->ptrset[:3], sptr1->ptrset2[:3])
  #pragma omp target exit data map(data)

  __builtin_free (s1.ptrset);
  __builtin_free (s1.ptrset2);
}
