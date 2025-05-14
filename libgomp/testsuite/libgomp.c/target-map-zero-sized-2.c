int
main ()
{
  int i, n;
  int data[] = {1,2};
  struct S { int **ptrset; };

// -----------------------------------

/* The produced mapping for sptr1->ptrset[i][:n]

   GOMP_MAP_STRUCT (size = 1)
      GOMP_MAP_ZERO_LEN_ARRAY_SECTION
   GOMP_MAP_ZERO_LEN_ARRAY_SECTION
      GOMP_MAP_ATTACH
   GOMP_MAP_ATTACH -> attaching to 2nd GOMP_MAP_ZERO_LEN_ARRAY_SECTION

which get split into 3 separate map_vars call; in particular,
the latter is separate and points to an unmpapped variable.

Thus, it failed with:
   libgomp: pointer target not mapped for attach  */

  struct S s1, *sptr1;
  s1.ptrset = (int **) __builtin_malloc (sizeof(void*) * 3);
  s1.ptrset[0] = data;
  s1.ptrset[1] = data;
  s1.ptrset[2] = data;
  sptr1 = &s1;

  i = 1;
  n = 0;
  #pragma omp target enter data map(sptr1[:1], sptr1->ptrset[:3])
  #pragma omp target enter data map(sptr1->ptrset[i][:n])

  #pragma omp target exit data map(sptr1->ptrset[i][:n])
  #pragma omp target exit data map(sptr1[:1], sptr1->ptrset[:3])

  __builtin_free (s1.ptrset);

// -----------------------------------

/* The produced mapping for sptr2->ptrset[i][:n] is similar:

   GOMP_MAP_STRUCT (size = 1)
      GOMP_MAP_ZERO_LEN_ARRAY_SECTION
   GOMP_MAP_TO  ! this one has now a finite size
      GOMP_MAP_ATTACH
   GOMP_MAP_ATTACH -> attach to the GOMP_MAP_TO

As the latter GOMP_MAP_ATTACH has now a pointer target,
the attachment worked.  */

  struct S s2, *sptr2;
  s2.ptrset = (int **) __builtin_malloc (sizeof(void*) * 3);
  s2.ptrset[0] = data;
  s2.ptrset[1] = data;
  s2.ptrset[2] = data;
  sptr2 = &s2;

  i = 1;
  n = 2;
  #pragma omp target enter data map(sptr2[:1], sptr2->ptrset[:3])
  #pragma omp target enter data map(sptr2->ptrset[i][:n])

  #pragma omp target
    if (sptr2->ptrset[1][0] != 1 || sptr2->ptrset[1][1] != 2)
      __builtin_abort ();

  #pragma omp target exit data map(sptr2->ptrset[i][:n])
  #pragma omp target exit data map(sptr2[:1], sptr2->ptrset[:3])

  __builtin_free (s2.ptrset);
}
