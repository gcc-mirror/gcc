// Test atomic increment via synchronized blocks.
public class SyncTest implements Runnable {
  static int counter;

  public void run() {
    for (int n = 0; n < 1000000; n++)
      synchronized (SyncTest.class) {
        counter++;
      }
  }

  public static void main(String[] args) {
    SyncTest test = new SyncTest();
    Thread[] thr = new Thread[4];

    for (int n = 0; n < thr.length; n++) {
      thr[n] = new Thread(test);
      thr[n].start();
    }

    for (int n = 0; n < thr.length; n++) {
      try {
        thr[n].join();
      } catch (InterruptedException ex) {
      }
    }

    System.out.println(counter == 1000000 * thr.length ?
      "ok" : "fail: " + counter);
  }
}
