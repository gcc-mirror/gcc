public class initexc
{
  public static class fail
  {
    static
    {
      // Static initializers must be able to complete normally.
      if (true)
	throw new NullPointerException("nope");
    }

    public static int val ()
    {
      return 23;
    }
  }

  public static void main (String[] args)
  {
    try
      {
	System.out.println (fail.val ());
      }
    catch (ExceptionInInitializerError _)
      {
	// Ok.
      }
    try
      {
	System.out.println (fail.val ());
      }
    catch (NoClassDefFoundError _)
      {
	// Ok.
      }
  }
}
