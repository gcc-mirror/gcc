// Test JVMTI GetErrorName

public class geterrorname
{
  public static native void do_errorname_tests ();

  public static void main (String[] args)
  {
    System.out.println ("JVMTI GetErrorName tests");
    do_errorname_tests ();
  }
}
