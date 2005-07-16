public class SyncronizedTest
  implements Runnable
{
  public static int count = 0;
  String _name;

  public SyncronizedTest(String name)
    {
      _name = name;
    }

  public void run()
    {
      if (_name.equals("timer")) {
	try {
	  Thread.sleep(10000);
	} catch (InterruptedException e){}
	System.out.println("FAILED: timer triggered");
	System.exit(1);
      }
      try {
	count++;

	synchronized(this) {
	  notifyAll();
	}
      } catch (Exception e) {
	System.out.println("FAILED: receiver: " + e);
	System.exit(1);
      }
    }
  public static void main(String args[])
    {
      try {
	SyncronizedTest tester = new SyncronizedTest("tester");
	Thread tester_thread = new Thread(tester);

	SyncronizedTest timer = new SyncronizedTest("timer");
	Thread timer_thread = new Thread(timer);
	timer_thread.start();

	synchronized(tester) {
	  tester_thread.start();
	  tester.wait();
	}

	if (0 == count)
	  throw new Exception("Thread did not run.");

	tester_thread.join();
	  
	System.out.println("PASSED: count="+count);
	System.exit(0);
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
	System.exit(1);
      }
    }
}
