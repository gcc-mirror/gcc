// Test a simple static function with an `int' argument.

public class simple_int
{
  public static native int nat (int z);

  static
  {
    System.loadLibrary ("simple_int");
  }

  public static void main (String[] args)
  {
    System.out.println (nat (23));
  }
}
