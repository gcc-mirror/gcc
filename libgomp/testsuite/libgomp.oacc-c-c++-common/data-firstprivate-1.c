/* Test behavior of 'firstprivate' lexically vs. dynamically nested inside a
   'data' region.  */

#include <stdlib.h>


#define VERIFY(x) \
  do { \
    if (!(x)) \
      abort (); \
  } while (0);


/* This is basically and extended version of 't2' from 'firstprivate-1.c'.  */

int lexically_nested_val = 2;

static void
lexically_nested ()
{
#pragma acc data \
  copy (lexically_nested_val)
  {
    VERIFY (lexically_nested_val == 2);

#pragma acc parallel \
  present (lexically_nested_val)
    {
      VERIFY (lexically_nested_val == 2);

      /* This updates the device copy, or shared variable.  */
      lexically_nested_val = 7;
    }

#if ACC_MEM_SHARED
    VERIFY (lexically_nested_val == 7);
#else
    VERIFY (lexically_nested_val == 2);
#endif

    /* This only updates the local/shared variable, but not the device
       copy.  */
    lexically_nested_val = 5;

#pragma acc parallel \
    firstprivate (lexically_nested_val)
      {
#if 1 /* Current behavior.  */
	/* The 'firstprivate' copy is initialized from the device copy, or
	   shared variable.  */
# if ACC_MEM_SHARED
	VERIFY (lexically_nested_val == 5);
# else
	VERIFY (lexically_nested_val == 7);
# endif
#else /* Expected behavior per PR92036.  */
	/* The 'firstprivate' copy is initialized from the local thread.  */
	VERIFY (lexically_nested_val == 5);
#endif

	/* This updates the 'firstprivate' copy only, but not the shared
	   variable.  */
	lexically_nested_val = 9;
      }

    VERIFY (lexically_nested_val == 5);
  }
  /* If not shared, the device copy has now been copied back.  */

#if ACC_MEM_SHARED
  VERIFY (lexically_nested_val == 5);
#else
  VERIFY (lexically_nested_val == 7);
#endif
}


int dynamically_nested_val = 2;

/* Same as above, but compute construct 1 broken out, so no longer lexically
   nested inside 'data' region.  */

static void
dynamically_nested_compute_1 ()
{
#pragma acc parallel \
  present (dynamically_nested_val)
  {
    VERIFY (dynamically_nested_val == 2);

    /* This updates the device copy, or shared variable.  */
    dynamically_nested_val = 7;
  }
}

/* Same as above, but compute construct 2 broken out, so no longer lexically
   nested inside 'data' region.  */

static void
dynamically_nested_compute_2 ()
{
#pragma acc parallel \
  firstprivate (dynamically_nested_val)
    {
#if 1 /* Current behavior.  */
      /* The 'firstprivate' copy is initialized from the device copy, or shared
	 variable.  */
# if ACC_MEM_SHARED
      VERIFY (dynamically_nested_val == 5);
# else
      VERIFY (dynamically_nested_val == 7);
# endif
#else /* Expected behavior per PR92036.  */
      /* The 'firstprivate' copy is initialized from the local thread.  */
      VERIFY (dynamically_nested_val == 5);
#endif

      /* This updates the 'firstprivate' copy only, but not the shared
	 variable.  */
      dynamically_nested_val = 9;
    }
}

static void
dynamically_nested ()
{
#pragma acc data \
  copy (dynamically_nested_val)
  {
    VERIFY (dynamically_nested_val == 2);

    dynamically_nested_compute_1 ();

#if ACC_MEM_SHARED
    VERIFY (dynamically_nested_val == 7);
#else
    VERIFY (dynamically_nested_val == 2);
#endif

    /* This only updates the local/shared variable, but not the device
       copy.  */
    dynamically_nested_val = 5;

    dynamically_nested_compute_2 ();

    VERIFY (dynamically_nested_val == 5);
  }
  /* If not shared, the device copy has now been copied back.  */

#if ACC_MEM_SHARED
  VERIFY (dynamically_nested_val == 5);
#else
  VERIFY (dynamically_nested_val == 7);
#endif
}


int
main()
{
  lexically_nested ();
  dynamically_nested ();

  return 0;
}
