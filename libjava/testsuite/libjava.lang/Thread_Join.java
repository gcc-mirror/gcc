// Many threads join a single thread.
// Origin: Bryce McKinlay <bryce@albatross.co.nz>

class Sleeper implements Runnable
{
  int num = -1;
  
  public Sleeper(int num)
  {
    this.num = num;
  }
  
  public void run()
  {
    System.out.println("sleeping");
    try
    {
      Thread.sleep(500);
    }
    catch (InterruptedException x)
    {
      System.out.println("sleep() interrupted");
    }
    System.out.println("done");
  }
}

class Joiner implements Runnable
{
  Thread join_target;
  
  public Joiner(Thread t)
  {
    this.join_target = t;
  }
  
  public void run()
  {
    try
    {
      long start = System.currentTimeMillis();
      join_target.join(2000);
      if ((System.currentTimeMillis() - start) > 1900)
        System.out.println("Error: Join timed out");
      else
        System.out.println("ok");
    }
    catch (InterruptedException x)
    {
      System.out.println("join() interrupted");
    }
  }
  
}

public class Thread_Join
{
  public static void main(String[] args)
  {
    Thread primary = new Thread(new Sleeper(1));
    primary.start();
    for (int i=0; i < 10; i++)
    {
      Thread t = new Thread(new Joiner(primary));
      t.start();
    }
  }
}
