// Test the status of the isAlive() flag before, during, and after thread 
// execution. Check that thread's threadgroup is null after thread exits.

public class Thread_Alive implements Runnable
{
  public static void main(String args[]) throws InterruptedException
  {
    Thread_Alive ta = new Thread_Alive();
    Thread t = new Thread(ta);
    System.out.println(t.isAlive());
    t.start();
    System.out.println(t.isAlive());

    Thread.sleep(50);
    
    synchronized (ta)
    {
      ta.notifyAll();
    }

    t.join();
    System.out.println(t.isAlive());
    
    try
    {
      t.start();
      System.out.println("Error: dead thread can be restarted.");
    }
    catch (IllegalThreadStateException x)
    {
      System.out.println ("ok");
    }

    System.out.println(t.getThreadGroup());
  }
  
  public synchronized void run()
  {
    try
    {
      wait();
    }
    catch (InterruptedException x) {}
  }
  
}
