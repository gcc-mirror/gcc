// register.java - Test RegisterNatives.

public class register
{
  static
  {
    System.loadLibrary ("register");
  }

  public static native int doit (int z);

  public static void main (String[] args)
  {
    System.out.println (doit (24));
  }
}
