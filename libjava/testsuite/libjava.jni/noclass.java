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
    find_it ();
    // If find_it() causes a crash, we won't be running this next line.
    System.out.println ("Ok");
  }
}
