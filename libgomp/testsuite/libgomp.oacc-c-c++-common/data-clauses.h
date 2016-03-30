int i;

int main(void)
{
  int j, v;

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) copyin (i, j)
  {
    if (i != -1 || j != -2)
      __builtin_abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
#if ACC_MEM_SHARED
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();
#else
  if (v != 1 || i != -1 || j != -2)
    __builtin_abort ();
#endif

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) copyout (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) copy (i, j)
  {
    if (i != -1 || j != -2)
      __builtin_abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) create (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
#if ACC_MEM_SHARED
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();
#else
  if (v != 1 || i != -1 || j != -2)
    __builtin_abort ();
#endif

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) present_or_copyin (i, j)
  {
    if (i != -1 || j != -2)
      __builtin_abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
  if (v != 1)
    __builtin_abort ();
#if ACC_MEM_SHARED
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();
#else
  if (v != 1 || i != -1 || j != -2)
    __builtin_abort ();
#endif

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) present_or_copyout (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) present_or_copy (i, j)
  {
    if (i != -1 || j != -2)
      __builtin_abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) present_or_create (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      __builtin_abort ();
    v = 1;
  }
  if (v != 1)
    __builtin_abort ();
#if ACC_MEM_SHARED
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();
#else
  if (v != 1 || i != -1 || j != -2)
    __builtin_abort ();
#endif

  i = -1;
  j = -2;
  v = 0;

#pragma acc data copyin (i, j)
  {
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v) present (i, j)
    {
      if (i != -1 || j != -2)
	__builtin_abort ();
      i = 2;
      j = 1;
      if (i != 2 || j != 1)
	__builtin_abort ();
      v = 1;
    }
  }
#if ACC_MEM_SHARED
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();
#else
  if (v != 1 || i != -1 || j != -2)
    __builtin_abort ();
#endif

  i = -1;
  j = -2;
  v = 0;

#pragma acc data copyin(i, j)
  {
#pragma acc CONSTRUCT /* copyout */ present_or_copyout (v)
    {
      if (i != -1 || j != -2)
	__builtin_abort ();
      i = 2;
      j = 1;
      if (i != 2 || j != 1)
	__builtin_abort ();
      v = 1;
    }
  }
#if ACC_MEM_SHARED
  if (v != 1 || i != 2 || j != 1)
    __builtin_abort ();
#else
  if (v != 1 || i != -1 || j != -2)
    __builtin_abort ();
#endif

  return 0;
}
