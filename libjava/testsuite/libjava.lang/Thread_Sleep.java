// Test that Thread.sleep() works.
// Origin: Bryce McKinlay <bryce@albatross.co.nz>

public class Thread_Sleep
{
  public static void main(String args[])
  {
    try
    {
      long start = System.currentTimeMillis();
      System.out.println("sleeping");
      Thread.sleep(1000);
      long end = System.currentTimeMillis();
      if ((end - start) > 1100 || (end - start) < 990)
        System.out.println ("failed");
      else
	System.out.println("ok");
    }
    catch (InterruptedException x)
    {
      System.out.println("error: Thread interrupted.");
    }
  }
}
