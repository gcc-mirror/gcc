// Test to see if throw works.

public class throwit
{
  static
  {
    System.loadLibrary ("throwit");
  }

  public static native void throwit (String name, boolean is_new);

  public static void main (String[] args)
  {
    try
      {
	throwit ("java/lang/UnknownError", false);
      }
    catch (Throwable x)
      {
	System.out.println (x.getClass ());
	System.out.println (x.getMessage ());
      }
    try
      {
	throwit ("java/lang/Throwable", true);
      }
    catch (Throwable x)
      {
	System.out.println (x.getClass ());
	System.out.println (x.getMessage ());
      }
  }
}
