public class Synch
{
  public synchronized void s()
    {
      // This call to notify() isn't supposed to cause a
      // java.lang.IllegalMonitorStateException.
      notify ();
    }

  public static void main (String[] args)
    {
      (new Synch()).s();
      System.out.println ("Ok");
    }
}


