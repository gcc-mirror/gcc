// PR18116.java - Test RegisterNatives with more complex signatures.

public class PR18116
{
  static
  {
    System.loadLibrary ("PR18116");
  }

  public static native int doit (java.lang.String s);

  public static void main (String[] args)
  {
    System.out.println (doit ("Hello World!"));
  }
}
