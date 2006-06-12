class PR27908
{
  public static void main (String[] argv)
  {
    run1 r1 = new run1();
    run2 r2 = new run2();
    run3 r3 = new run3();

    new Thread (r1).start();
    new Thread (r2).start();
    new Thread (r3).start();

    Thread.yield();

    r1.stop();
    r2.stop();
    r3.stop();
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
      System.out.println ("run1 exits!");
    }

    private void stop ()
    {
      running = false;
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
      System.out.println ("run2 exits!");
    }

    private void stop ()
    {
      running = false;
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
      System.out.println ("run3 exits!");
    }

    void stop ()
    {
      running = false;
    }
  }

}
