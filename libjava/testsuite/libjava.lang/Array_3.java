// Test to make sure null arrays throw the right execption

public class Array_3
{
  static Object foo ()
  {
    return null;
  }

  static int[] bar ()
  {
    return null;
  }

  public static void main(String args[])
  {
    boolean ok = false;
    int nn = 0;

    try
      {
	int[] x = (int[])foo();
	nn = x.length;
      }
    catch (NullPointerException _)
      {
	ok = true;
      }
    if (!ok)
      throw new RuntimeException("test failed");

    ok = false;
    try
      {
	int[] x = bar();
	nn = x.length;
      }
    catch (NullPointerException _)
      {
	ok = true;
      }
    if (!ok)
      throw new RuntimeException("test failed");

    ok = false;
    try
      {
	int[] x = bar();
	nn = x[0];
      }
    catch (NullPointerException _)
      {
	ok = true;
      }

    if (!ok || nn != 0)
      throw new RuntimeException("test failed");
  }
}
