// Various thread tests.

public class Thread_1 extends Thread
{
  // The group for the workers.
  static ThreadGroup subgroup;

  // Which piece of test code to try.
  static int test_case;

  // Names of the tests.
  static final int JOIN_GOOD = 0;
  static final int JOIN_TIMEOUT = 1;
  static final int JOIN_INTERRUPTED = 2;
  static final int THREAD_EXIT = 3;

  // True if this is normal; false if daemon.
  boolean normal;
  // The other thread in the test.
  Thread_1 other;
  // True when the thread has entered run().
  boolean started;

  public void run ()
  {
    try
      {
	if (normal)
	  {
	    System.out.println ("test " + test_case);
	    // Tell the main thread to start the daemon thread.
	    synchronized (this)
	      {
		started = true;
		notify ();
	      }
	    // Now wait for daemon to start.
	    synchronized (other)
	      {
		while (! other.started)
		  other.wait ();
	      }
	    switch (test_case)
	      {
	      case JOIN_GOOD:
		other.join ();
		System.out.println ("joined");
		break;
	      case JOIN_TIMEOUT:
		other.join (10);
		System.out.println (other.isAlive());
		other.join ();
		break;
	      case JOIN_INTERRUPTED:
		other.join ();
		System.out.println ("joined");
		break;
	      case THREAD_EXIT:
		// Nothing.
		break;

	      default:
		System.out.println ("failure");
		break;
	      }
	  }
	else
	  {
	    // Let the normal thread start first.
	    synchronized (other)
	      {
		while (! other.started)
		  other.wait();
	      }
	    // Tell normal thread that we've started.
	    synchronized (this)
	      {
		started = true;
		notify ();
	      }
	    switch (test_case)
	      {
	      case JOIN_GOOD:
		System.out.println ("daemon done");
		break;
	      case JOIN_TIMEOUT:
		sleep (50);
		break;
	      case JOIN_INTERRUPTED:
		other.interrupt ();
		break;
	      case THREAD_EXIT:
		// Wait for a while.  However, don't wait indefinitely
		// -- we want this thread to terminate so that the
		// process won't hang if there is a bug.
		sleep (10000);
		System.out.println ("daemon still alive");
		break;

	      default:
		System.out.println ("failure");
		break;
	      }
	  }
      }
    catch (InterruptedException e)
      {
	System.out.println ("interrupted");
      }
  }

  public void setOther (Thread_1 x)
  {
    other = x;
  }

  Thread_1 (String name, boolean x)
  {
    super (subgroup, name);
    normal = x;
    started = false;
    setDaemon (! normal);
  }

  // Run a single test.
  static Thread_1 doit (int what)
  {
    // FIXME: we used to just use the same threads each time.  That
    // didn't work -- must debug.
    Thread_1 dt = new Thread_1 ("daemon", false);
    Thread_1 nt = new Thread_1 ("normal", true);

    dt.setOther(nt);
    nt.setOther(dt);

    test_case = what;
    try
      {
	nt.start();
	dt.start();

	// Don't wait for the threads if we're doing the exit test.
	if (what != THREAD_EXIT)
	  {
	    nt.join ();
	    dt.join ();
	  }
      }
    catch (InterruptedException e)
      {
	System.out.println ("caught bad exception");
      }

    return dt;
  }

  public static void main (String[] args)
  {
    subgroup = new ThreadGroup ("sub");

    doit (JOIN_GOOD);

    System.out.println ("active count = " + subgroup.activeCount ());

    Thread_1 dt = doit (JOIN_TIMEOUT);
    // Make sure that joining a dead thread works.
    System.out.println ("still alive: " + dt.isAlive ());
    try
      {
	dt.join ();
      }
    catch (InterruptedException e)
      {
	System.out.println ("exception caught");
      }

    doit (JOIN_INTERRUPTED);

    // Note: this test has a race conditoin.  So we don't run it any
    // more.
    // This test must come last.
    // doit (THREAD_EXIT);
  }
}
