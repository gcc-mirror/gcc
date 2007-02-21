public class getargssize
{
  static
    {
      System.loadLibrary("natgetargssize");
    }

  public int aMethod (float fone, int ione)
  {
    return 0;
  }
  
  public long bMethod (long lone, double done, int ione)
  {
    return 0;
  }
  
  public static boolean cMethod ()
  {
    return false;
  }
  
  public static Object dMethod (Object op)
  {
    return op;
  }

  public static native int do_getargssize_tests ();

  public static void main (String[] args)
  {
    System.out.println ("JVMTI getargssize Interpreted Test");

    do_getargssize_tests ();
  }
}
