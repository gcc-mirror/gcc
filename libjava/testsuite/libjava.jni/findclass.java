// Test FindClass part of JNI.

public class findclass
{
  static
  {
    System.loadLibrary ("findclass");
  }

  public static native Class doit (String name);

  public static void main (String[] args)
  {
    System.out.println ("" + doit ("java/lang/String"));
  }
}
