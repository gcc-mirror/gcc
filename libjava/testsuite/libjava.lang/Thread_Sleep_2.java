// Test that Thread.sleep() is accurate
// and that nanoTime actually measures in nanoseconds.

public class Thread_Sleep_2
{
  public static void main(String args[])
  {
    try
    {
      boolean ok = true;
      for (int i = 0; i < 100; i++)
	{
	  long start = System.nanoTime();
	  Thread.sleep(10);
	  long end = System.nanoTime();
	  if ((end - start) < 10000000)
	    {
	      System.out.print ("failed, iteration ");
	      System.out.print (i);
	      System.out.print (", time ");
	      System.out.print (end - start);
	      System.out.println ("ns");
	      ok = false;
	    }
	}
      if (ok)
	System.out.println ("ok");
    }
    catch (InterruptedException x)
    {
      System.out.println("error: Thread interrupted.");
    }
  }
}
