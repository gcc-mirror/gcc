// Check that NullPointerExceptions thrown from library code are
// caught.  This detects a number of failures that can be caused by
// libgcj being built incorrectly.  In particular, we ensure that a
// SEGV in native (i.e. C++) code in libgcj is handled correctly.

// Regrettably, we cannot guarantee that Double.parseDouble() will
// always be native code, or that it will never be inlined.  It could
// be argued that we should add a method to libgcj that will be
// guaranteed forever to be native, but I'm reluctant to add to the
// library for the sole purpose of performing this test.

public class Throw_2
{
  public static Throwable obj()
    {
      return null;
    }

  public static String str()
    {
      return null;
    }

  static double d;

  public static void main (String[] args)
    {
      // This NullPointerException will, at the time of writing, be
      // thrown from Java code in libgcj.
      try
	{
	  java.util.Vector v = new java.util.Vector (null);
	  System.out.println ("fail: no exception thrown");
	}
      catch (NullPointerException _)
	{
	  System.out.println ("1");
	}
      catch (Throwable _)
	{
	  System.out.println ("fail: " + _);
	}
      // This one will, at the time of writing, be thrown from C++
      // code in libgcj.
      try
	{
	  d = Double.parseDouble(str());
	  System.out.println ("fail: no exception thrown");
	}
      catch (NullPointerException _)
	{
	  System.out.println ("2");
	}
      catch (Throwable _)
	{
	  System.out.println ("fail: " + _);
	}
    }
}
