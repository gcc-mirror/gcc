// Create many threads waiting on a monitor. Interrupt some of them. Do the 
// others wake up correctly with notify() and/or notifyAll()?
// Origin: Bryce McKinlay <bryce@albatross.co.nz>

import java.util.Vector;

class Waiter extends Thread
{
  Object monitor;
  int thread_num;
  boolean interrupted = false;
  boolean notified = false;

  Waiter (Object monitor, int thread_num)
  {
    this.monitor = monitor;
    this.thread_num = thread_num;
  }  
  
  public void run()
  {
    synchronized (monitor)
      {
	try
	{
	  monitor.wait();
	  notified = true;
	}
	catch (InterruptedException x)
	{
	  interrupted = true;
	}
      }
    
  }
}

public class Thread_Wait_2
{
  static Vector threads;
  static Object monitor = new Object();
  
  static final int NUM_THREADS = 10;
  
  public static void main(String args[])
  {
    
    
    try
    {
      makeThreads ();
      
      Thread.sleep(250);
      
      // Interrupt a few threads...
      Waiter i1 = (Waiter) threads.elementAt(3);
      Waiter i2 = (Waiter) threads.elementAt(4);
      Waiter i3 = (Waiter) threads.elementAt(9);
      i1.interrupt();
      i2.interrupt();
      i3.interrupt();
      
      // Call notify the exact number of times required to wake the remaining
      // threads.
      synchronized (monitor)
      {
	for (int i=0; i < NUM_THREADS -3 ; i++)
	{
	  monitor.notify ();
	}
      }
      
      joinAll();
      printStatus();
      
      // Repeat all the above, but use notifyAll() instead.
      makeThreads();

      Thread.sleep(250);
      
      // Interrupt a few threads...
      i1 = (Waiter) threads.elementAt(0);
      i2 = (Waiter) threads.elementAt(1);
      i3 = (Waiter) threads.elementAt(9);
      i1.interrupt();
      i2.interrupt();
      i3.interrupt();
      
      // Call notifyAll to wake the remaining threads.
      synchronized (monitor)
      {
        monitor.notifyAll ();
      }

      joinAll();
      printStatus();

    }
    catch (InterruptedException x)
    {
      System.out.println (x);
    }


  }
  
  static void makeThreads()
  {
    threads = new Vector(NUM_THREADS);
    
    for (int i=0; i < NUM_THREADS; i++)
      {
	Waiter w = new Waiter(monitor, i);
	w.start();
	threads.addElement(w);
      }
  }
  
  static void joinAll()
  {
    try
    {
      for (int i=0; i < threads.size(); i++)
	{
          Thread t = (Thread) threads.elementAt(i);
	  t.join();
	}
    }
    catch (InterruptedException x) {}
  }
  
  static void printStatus()
  {
    for (int i=0; i < threads.size(); i++)
      {
        Waiter w = (Waiter) threads.elementAt(i);
	if (w.interrupted)
	  System.out.println (i + " interrupted.");
	if (w.notified)
	  System.out.println (i + " notified.");
      }
  }
  
}
