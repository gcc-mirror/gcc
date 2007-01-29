public class getstacktrace
  extends Thread
{
  public boolean done = false;

  // num_frames is the number of frames > the original run () call so if
  // num_frames = 1, the thread will have 2 frames, the original Thread.run
  // call, plus one additional
  public int num_frames, thread_num;

  public static int num_threads = 1;

  static
    {
      System.loadLibrary("natgetstacktrace");
    }

  public void run ()
  {
    thread_num = num_threads++;
    num_frames = thread_num;

    if (num_frames <= 1)
      {
        natRunner ();
      }
    else
      {
        if (thread_num % 2 == 0)
          natPlaceholder ();
        else
          placeholder ();
      }
  }

  public void placeholder ()
  {
    num_frames--;
    if (num_frames <= 1)
      {
        if (thread_num % 2 == 1)
          natRunner ();
        else
          runner ();
      }
    else
      {
        if (thread_num % 2 == 0)
          natPlaceholder ();
        else
          placeholder ();
      }
  }
  
  public void runner ()
  {
    done = true;
    while (done)
      yield ();
  }

  public native void natPlaceholder ();
  public native void natRunner ();

  public static native int do_getstacktrace_tests (Thread[] threads);

  public static void main (String[] args)
  {
    System.out.println ("JVMTI GetStackTrace Interpreted Test");

    getstacktrace[] threads = new getstacktrace[10];

    for (int i = 0; i < threads.length; i++)
      {
        threads[i] = new getstacktrace ();
        threads[i].start ();
        while (!threads[i].done)
          yield ();
      }

    do_getstacktrace_tests (threads);

    for (int i = 0; i < threads.length; i++)
      {
        threads[i].done = false;
      }
  }
}
