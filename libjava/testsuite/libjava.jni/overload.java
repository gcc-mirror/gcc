// Test to make sure overloaded functions with long names work.

public class overload
{
  static
  {
    System.loadLibrary ("overload");
  }

  public static native int over (int one);
  public static native int over (int one, int two);

  public static void main (String[] args)
  {
    System.out.println (over (1));
    System.out.println (over (1, 2));
  }
}
