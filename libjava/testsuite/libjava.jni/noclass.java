// Test to make sure JNI implementation catches exceptions.

public class noclass
{
  static
  {
    System.loadLibrary ("noclass");
  }

  public static native void find_it ();

  public static void main (String[] args)
  {
    try
      {
	find_it ();
      }
    catch (Throwable _)
      {
	// If find_it() causes a crash, or doesn't throw an exception,
	// we won't be running this next line.
	System.out.println ("Ok");
      }
  }
}
