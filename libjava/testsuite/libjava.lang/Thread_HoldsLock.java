// Test that Thread.holdsLock() works.

class Lock {}

public class Thread_HoldsLock
{
  static Lock lock = new Lock();
  
  public static void main(String args[]) throws InterruptedException
  {
    Thread_HoldsLock thl = new Thread_HoldsLock();
    
    thl.check();
    synchronized (lock)
      {
        thl.check();
      }
    thl.check();
  }
  
  public void check()
  {
    boolean held = Thread.currentThread().holdsLock(lock);
    System.out.println(held);
  }
}

