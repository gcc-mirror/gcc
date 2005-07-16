import java.lang.*;

/* Simple producer/consumer thread test. */

public class ThreadTest implements Runnable {
 
  static String threadName = "Running thread";
  static int count = 0;
  static int max = 4; // XXX Seem to fail when >4 on kaffe 0.9.0

  public void run() {
    if (! Thread.currentThread().isAlive() ) {
      System.out.println("FAILED: isAlive() false in new thread!");
    } else {
      System.out.println("PASSED: isAlive() working in new thread");
    }
    while (0 <= count && count <= max) {
      count ++;
    }
  }

  public static void main (String args[]) {
    try {
      if (! Thread.currentThread().isAlive() ) {
	System.out.println("FAILED: isAlive() false in initial thread!");
      } else {
	System.out.println("PASSED: isAlive() working in initial thread");
      }
      ThreadTest test = new ThreadTest();

      Thread testThread = new Thread(test, threadName);

      testThread.setDaemon(true);
      testThread.start();

      Thread.currentThread().sleep(3000);

      if (count < max) {
	System.out.println("FAILED: unable to run new thread");
      } else {	
	System.out.println("PASSED: Theads worked");
      }
      System.exit(0);
    } catch (Exception e) {
      System.out.println("FAILED: "+e);
    }
  }
}
