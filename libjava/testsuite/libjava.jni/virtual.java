// Minimal test of a virtual method.

public class virtual
{
  static
  {
    System.loadLibrary ("virtual");
  }

  public native boolean equals (Object obj);

  public static void main (String[] args)
  {
    Object v = new virtual ();
    System.out.println (v.equals (v));
  }
}
