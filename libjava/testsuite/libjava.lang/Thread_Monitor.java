// Test that monitor locks work and are recursive.

class T implements Runnable
{
  public int count = 0;
  Counter c;
  
  public T (Counter c)
  {
    this.c = c;
  }

  public void run()
  {
    while (true)
      {
        // NOTE: double-synchronization here.
	synchronized (c)
	{
	  if (c.getCount() <= 100000)
	    count++;
	  else
	    break;
	}
      }
  }
}

class Counter
{
  int i = 0;
  public synchronized int getCount ()
  {
    return ++i; 
  }
}

public class Thread_Monitor
{
  public static void main(String args[])
  {
    Counter c = new Counter();
    T t1 = new T(c);
    T t2 = new T(c);
    
    Thread th1 = new Thread(t1);
    Thread th2 = new Thread(t2);
    th1.start();
    th2.start();
    try
    {
      th1.join();
      th2.join();
    } 
    catch (InterruptedException x)
    {
      System.out.println("failed: Interrupted");
    }
    if (t1.count + t2.count == 100000)
      System.out.println ("ok");
    else
      System.out.println ("failed: total count incorrect");
  }
}
