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
    Thread this_thread = Thread.currentThread();
    System.out.println(this_thread.holdsLock(lock));
  }
}

