// Test interrupt() behaviour on a thread in wait(), sleep(), and spinning 
// in a loop.
// Origin: Bryce McKinlay <bryce@albatross.co.nz>

class Waiter extends Thread
{
  public synchronized void run()
  {
    System.out.println ("wait()");
    try
    {
      wait();
      System.out.println("Error: wait() completed normally.");
    }
    catch (InterruptedException x)
    {
      if (isInterrupted() || interrupted()) 
        System.out.println("Error: interrupt flag is still set.");
    
    }
      System.out.println("interrupted - ok");
  }
}

class Sleeper extends Thread
{
  public void run()
  {
    System.out.println ("sleep()");
    try
    {
      sleep(2000);
      System.out.println("Error: sleep() completed normally.");
    }
    catch (InterruptedException x)
    {
      if (isInterrupted() || interrupted()) 
        System.out.println("Error: interrupt flag is still set.");
    
      System.out.println("interrupted - ok");
    }
  }
}

class Looper extends Thread
{
  public void run()
  {
    System.out.println ("Busy waiting");

    int count = 0;
    for (int i=0; i < 1000000; i++)
      {
        Thread.yield();
	count += 5;
	if (isInterrupted ())
	  break;
      }
    synchronized (this)
    {  
      if (interrupted ())
	{
	  System.out.println ("interrupted - ok");
	  if (isInterrupted () || interrupted ())
	    System.out.println("Error: interrupt flag is still set.");
	}
      else
	System.out.println ("Error: Busy wait was not interrupted.");          
    }
  }
}

class Joiner extends Thread
{
  public void run()
  {
    System.out.println("join()");
    try
    {
      join(2000);
      System.out.println("Error: join() completed normally??!");
    }
    catch (InterruptedException x)
    {
      if (isInterrupted() || interrupted()) 
        System.out.println("Error: interrupt flag is still set.");
    
      System.out.println("interrupted - ok");
    }

  }
}

public class Thread_Interrupt
{
  public static void main(String args[])
  {
    Waiter w = new Waiter();
    w.start ();
    sleep_and_interrupt (w);
    
    Sleeper s = new Sleeper();
    s.start ();
    sleep_and_interrupt (s);

    Looper l = new Looper ();
    l.start ();
    sleep_and_interrupt (l);

    Joiner j = new Joiner ();
    j.start ();
    sleep_and_interrupt (j);
  }
  
  public static void sleep_and_interrupt(Thread t)
  {
    try
    {
      Thread.sleep (250);
      t.interrupt ();
      long t1 = System.currentTimeMillis();
      t.join (5000);
      long time = System.currentTimeMillis() - t1;
      if (time > 2900)
        {
	  System.out.println ("Error: join() from main thread timed out");
	}
    }
    catch (InterruptedException x)
    {
      System.out.println("Error: main thread interrupted.");
    }
  }
}
