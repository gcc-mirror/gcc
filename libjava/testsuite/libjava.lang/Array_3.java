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

  static int baz ()
  {
    int[] x = (int[])null;
    int nn = x.length;
    return 5;
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
      throw new RuntimeException("test failed:1");

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
      throw new RuntimeException("test failed:2");

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
      throw new RuntimeException("test failed:3");

    ok = false;
    try
      {
	int[] x = (int[])null;
	nn = x.length;
      }
    catch (NullPointerException _)
      {
	ok = true;
      }
    if (!ok)
      throw new RuntimeException("test failed:4");

    ok = false;
    try
      {
	nn = baz ();
      }
    catch (NullPointerException _)
      {
	ok = true;
      }
    if (!ok)
      throw new RuntimeException("test failed:5");
  }
}
