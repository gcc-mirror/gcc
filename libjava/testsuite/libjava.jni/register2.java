// Another test of RegisterNatives.
// We neglected to track the class name in our internal hash table.
// This is a regression test for the fix.

public class register2
{
  static
  {
    System.loadLibrary ("register2");
  }

  static class I1
  {
    public static native int doit ();
  }

  static class I2
  {
    public static native int doit ();
  }

  public static void main (String[] args)
  {
    System.out.println (new I1().doit());
    System.out.println (new I2().doit());
  }
}
