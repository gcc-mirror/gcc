class PR27908
{
  public static void main (String[] argv)
    throws InterruptedException
  {
    run1 r1 = new run1();
    run2 r2 = new run2();
    run3 r3 = new run3();

    Thread t1, t2, t3;

    (t1 = new Thread (r1)).start();
    (t2 = new Thread (r2)).start();
    (t3 = new Thread (r3)).start();

    while (! (r1.isRunning() && r2.isRunning() && r3.isRunning()))
      Thread.yield();

    r1.stop();
    r2.stop();
    r3.stop();

    Thread.sleep(5000);

    if (t1.isAlive() || t2.isAlive() || t3.isAlive())
      {
	System.out.println ("fail");
	System.exit(1);
      }
  }

  private static class run1 implements Runnable
  {
    volatile int counter;
    volatile boolean running;

    public void run ()
    {
      counter = 0;
      running = true;
      while (running)
        counter++;
    }

    void stop ()
    {
      running = false;
    }

    public boolean isRunning()
    {
      return running;
    }
  }

  private static class run2 implements Runnable
  {
    volatile int counter;
    boolean running;

    public void run ()
    {
      counter = 0;
      running = true;
      while (running)
        counter++;
    }

    void stop ()
    {
      running = false;
    }

    public boolean isRunning()
    {
      return running;
    }
  }

  static class run3 implements Runnable
  {
    volatile int counter;
    private volatile boolean running;

    public void run ()
    {
      counter = 0;
      running = true;
      while (running)
        counter++;
    }

    void stop ()
    {
      running = false;
    }

    public boolean isRunning()
    {
      return running;
    }
  }
}
