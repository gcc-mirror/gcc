/* This test checks for two slightly different overflow scenarios in
 * array allocation.
 *
 * The first is that the number of bytes needed for an array size
 * overflows on a 32 bit machine.
 *
 * The second is that on a 64 machine, the number of bytes silently
 * gets truncated, resulting in too small an object being
 * allocated.  */

class newarray_overflow
{
  static boolean failed = false;

  static void int_check()
  {
    int[] x;
    try
      {
	x = new int [1 << 30];
      }
    catch (OutOfMemoryError e)
      {
	return;
      }
    /* If we really get away with it (64 bit machine), that's cool.  */
    if (x == null) {
      System.err.println ("int check: new returned null.");
      failed = true;
      return;
    }
    try
      {
	// Only check a few places so we don't thrash too badly.
	for (int i = 0; i < x.length; i += (1 << 24))
	  if (x[i] != 0)
	    failed = true;
      }
    catch (Throwable e)
      {
	System.err.print ("int check: ");
	System.err.println (e);
	failed = true;
      }
  }

  static void object_check()
  {
    Object[] x;
    try
      {
	x = new Object [1 << 30];
	System.err.println ("Alloc succeeded.");
	System.err.println (x);
      }
    catch (OutOfMemoryError e)
      {
	return;
      }
    /* If we really get away with it (64 bit machine), that's cool.  */
    if (x == null) {
      System.err.println ("Object check: new returned null.");
      failed = true;
      return;
    }
    try
      {
	for (int i = 0; i < x.length; i += (1 << 24))
	  if (x[i] != null)
	    failed = true;
      }
    catch (Throwable e)
      {
	System.err.print ("Object check: ");
	System.err.println (e);
	failed = true;
      }
  }

  public static void main (String[] ignore)
  {
    int_check();
    object_check();

    if (!failed)
      System.out.println ("ok");
  }
}
