// Test for an array assignment bug we've had.

public class assign2
{
  public static Object[][] c () { return new Long[5][5]; }

  public static Object[] d () { return new Integer[3]; }

  public static void main(String[] args)
  {
    try
      {
	Object[][] x = c();
	x[0] = d();
      }
    catch (ArrayStoreException _)
      {
	System.out.println("good");
      }
  }
}
