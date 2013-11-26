/* Test __atomic routines for existence and proper execution on 1 byte 
   values with each valid memory model.  */
/* { dg-do run } */


/* Test the execution of the __atomic_load_n builtin for a char.  */

extern void abort(void);

char v, count;

int
main ()
{
  v = 0;
  count = 0;

  if (__atomic_load_n (&v, __ATOMIC_RELAXED) != count++) 
    abort(); 
  else 
    v++;

  if (__atomic_load_n (&v, __ATOMIC_ACQUIRE) != count++) 
    abort(); 
  else 
    v++;

  if (__atomic_load_n (&v, __ATOMIC_CONSUME) != count++) 
    abort(); 
  else 
    v++;

  if (__atomic_load_n (&v, __ATOMIC_SEQ_CST) != count++) 
    abort(); 
  else 
    v++;

  /* Now test the generic variants.  */

  __atomic_load (&v, &count, __ATOMIC_RELAXED);
  if (count != v)
    abort(); 
  else 
    v++;

  __atomic_load (&v, &count, __ATOMIC_ACQUIRE);
  if (count != v)
    abort(); 
  else 
    v++;

  __atomic_load (&v, &count, __ATOMIC_CONSUME);
  if (count != v)
    abort(); 
  else 
    v++;

  __atomic_load (&v, &count, __ATOMIC_SEQ_CST);
  if (count != v)
    abort(); 
  else 
    v++;

  return 0;
}

