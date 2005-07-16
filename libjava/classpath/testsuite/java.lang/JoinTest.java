public class JoinTest
  implements Runnable
{
  public static int count = 0;

  void send()
    throws Exception
    {
      Thread.sleep(2000);
      System.out.println("PASSED: Sender completed");
    }
  void receive()
    throws Exception
    {
      synchronized(this) {
	notifyAll();
      }

      Thread.sleep(5000);
      count++;
      System.out.println("PASSED: Receiver completed");
    }

  public void run()
    {
      String name = Thread.currentThread().getName();
      if (name.equals("timer")) {
	try {
	  Thread.sleep(10000);
	} catch (InterruptedException e){}
	System.out.println("FAILED: timer triggered");
	System.exit(1);
      }
      try {
	receive();
      } catch (Exception e) {
	System.out.println("FAILED: receiver: " + e);
	System.exit(1);
      }
    }
  public static void main(String args[])
    {
      try {
	JoinTest sender =
	  new JoinTest();
	JoinTest receiver =
	  new JoinTest();
	Thread receiver_thread = new Thread(receiver);

	/* Make sure the test terminates even if it hangs on network */
	JoinTest timer = new JoinTest();
	Thread timer_thread = new Thread(timer, "timer");
	timer_thread.start();

	synchronized(receiver) {
	  receiver_thread.start();
	  receiver.wait();
	}
	try {
	  sender.send();
	} catch (Exception e) {
	  System.out.println("FAILED: sender: " + e);
	  System.exit(1);
	}
	receiver_thread.join();
	  
	if (0 == count)
	  throw new Exception("Nothing received");

	System.out.println("PASSED: Join send/receive count="+count);
	System.exit(0);
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
	System.exit(1);
      }
    }
}
