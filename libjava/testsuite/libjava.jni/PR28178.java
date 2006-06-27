// Regression test for PR 28178.

public class PR28178
{
  static {
    System.loadLibrary("PR28178");
  }

  public static native void m();

  public static void main(String[] args)
  {
    m();
  }
}
