// Test to make sure the minimal invocation works.

public class invoke
{
  public static native int val ();

  static
  {
    System.out.println ("trying...");
    System.loadLibrary ("invoke");
    System.out.println ("loaded");
  }

  public static void main (String[] args)
  {
    System.out.println (val ());
  }
}
