// Test that Thread.sleep() works.

public class Thread_Sleep
{
  public static void main(String args[])
  {
    try
    {
      long start = System.currentTimeMillis();
      System.out.println("sleeping");
      Thread.sleep(50);
      long end = System.currentTimeMillis();
      if ((end - start) < 50)
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
