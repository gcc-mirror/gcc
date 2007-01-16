// Test JVMTI GetAllThreads

import java.util.ArrayList;

public class getallthreads extends Thread
{
	 public static int thread_num;
	 public static ArrayList threads;

	 public int ex_frames;
  public boolean done = false;
  
  public static native void do_getallthreads_tests ();

  public void run ()
	 {
    ex_frames = thread_num;
		 thread_num++;

		 if (ex_frames > 0)
		   {
        if ((ex_frames % 2) == 0)
					 placeholder ();
				 else
					 natPlaceholder ();
		   }
		 else
      runner ();
	 }

	 public native void natPlaceholder ();
	 public native void natRunner ();

	 public void placeholder ()
	 {
		 ex_frames--;

		 if (ex_frames > 0)
		   {
				 if ((thread_num % 2) == 0)
					 placeholder ();
				 else
					 natPlaceholder ();
			 }
		 else
			 runner ();
   }

	 public void runner ()
	 {
		 done = true;
		 while (done)
			yield ();
	 }

  public static void main (String[] args)
  {
    System.out.println ("JVMTI GetAllThreads tests");
    threads = new ArrayList (20);

		getallthreads t;

		for (int i = 0; i < 20; i++)
		  {
				t = new getallthreads ();
				threads.add (t);
				t.start ();
				while (!t.done)
					yield ();
			}

		do_getallthreads_tests ();

		for (int i = 0; i < 20; i++)
		  {
				t = (getallthreads) threads.get(i);
				t.done = false;
			}
  }
}
