// Test interrupt() behaviour on a thread in wait(), sleep(), and spinning 
// in a loop.

class ThreadBase extends Thread
{
  boolean ready = false;
  
  synchronized void ready()
  {
    ready = true;
  }
}

class Waiter extends ThreadBase
{
  public synchronized void run()
  {
    super.ready();
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

class Sleeper extends ThreadBase
{
  public void run()
  {
    super.ready();
    System.out.println ("sleep()");
    try
    {
      sleep(5000);
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

class Looper extends ThreadBase
{
  public void run()
  {
    super.ready();
    System.out.println ("Busy waiting");

    int count = 0;
    long start = System.currentTimeMillis();
    while (true)
      {
        Thread.yield();
	if (isInterrupted ())
	  break;
	long now = System.currentTimeMillis();	
	if ((now - start) > 5000)
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

class Joiner extends ThreadBase
{
  public void run()
  {
    super.ready();
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
  
  public static void sleep_and_interrupt(ThreadBase t)
  {
    try
    {
      synchronized (t)
        {
	  while (!t.ready)
	    t.wait(10);
	}
    
      Thread.sleep (50);
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
