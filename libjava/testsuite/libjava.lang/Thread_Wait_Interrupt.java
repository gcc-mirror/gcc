// Create two threads waiting on a monitor. Interrupt one of them. Does the 
// other wake up correctly?
// Origin: Bryce McKinlay <bryce@albatross.co.nz>

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
        System.out.println ("Thread waiting.");
	try
	{
	  long start = System.currentTimeMillis();
	  monitor.wait(1000);
	  long time = System.currentTimeMillis() - start;
	  if (time > 990)
	    System.out.println ("Error: wait on thread " + thread_num 
	                        + " timed out.");
	  else
	    notified = true;
	}
	catch (InterruptedException x)
	{
	  interrupted = true;
	}
      }
    
  }
}

public class Thread_Wait_Interrupt
{
  public static void main(String args[])
  {
    Object monitor = new Object();
    Waiter w1 = new Waiter(monitor, 1);
    Waiter w2 = new Waiter(monitor, 2);
    w1.start();
    w2.start();
    try
    {
      Thread.sleep(250);

      synchronized (monitor)
      {
	w1.interrupt();
	monitor.notify();
      }

      w1.join();
      w2.join();
      System.out.println("join ok");
      System.out.println("Thread 1 " + 
                         (w1.interrupted ? "interrupted ok" : "error"));
      System.out.println("Thread 2 " +
                         (w2.notified ? "notified ok" : "error"));

    }
    catch (InterruptedException x)
    {
      System.out.println (x);
    }
  }
}
