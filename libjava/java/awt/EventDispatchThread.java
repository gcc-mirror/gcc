/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/** @author Bryce McKinlay */

/* Status: believed complete, but untested. */

package java.awt;

class EventDispatchThread extends Thread
{
  private static int dispatchThreadNum = 1;

  private EventQueue queue;

  EventDispatchThread(EventQueue queue)
  {
    super();
    setName("AWT-EventQueue-" + dispatchThreadNum++);
    this.queue = queue;
    setPriority(NORM_PRIORITY + 1);
    start();
  }

  public void run()
  {
    while (true)
      {
        try
	{
	  AWTEvent evt = queue.getNextEvent();
	  queue.dispatchEvent(evt);
	}
	catch (Throwable x)
	{
	  System.err.println("Exception during event dispatch:");
	  x.printStackTrace(System.err);
	}
      }
  }
}
