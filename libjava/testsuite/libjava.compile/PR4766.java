// Test that bytecode generation works even when `finally' clause
// doesn't return normally.

public class PR4766
{
  public static int myfunction()
  {
    try
      {
	System.out.println ("hi");
      }
    catch( Exception e )
      {
	e.printStackTrace();
      }
    finally
      {
	return 0;
      }
  }

  public static void main (String[] args)
  {
  }
}
