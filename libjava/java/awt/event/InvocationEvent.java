/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */

/* Status: Believed to be complete and correct.  */

public class InvocationEvent extends AWTEvent implements ActiveEvent
{
  public static final int INVOCATION_DEFAULT = 1200;
  public static final int INVOCATION_FIRST = 1200;
  public static final int INVOCATION_LAST = 1200;

  protected InvocationEvent (Object source, int id, Runnable runnable,
			     Object notifier, boolean catchExceptions)
  {
    super (source, id);
    this.runnable = runnable;
    this.notifier = notifier;
    this.catchExceptions = catchExceptions;
  }

  public InvocationEvent (Object source, Runnable runnable)
  {
    super (source, INVOCATION_DEFAULT);
    this.runnable = runnable;
  }

  public InvocationEvent(Object source, Runnable runnable, Object notifier,
                	 boolean catchExceptions)
  {
    super (source, INVOCATION_DEFAULT);
    this.runnable = runnable;
    this.notifier = notifier;
    this.catchExceptions = catchExceptions;
  }

  public void dispatch ()
  {
    Exception e = null;
    if (catchExceptions)
      try
	{
	  runnable.run ();
	}
      catch (Exception x)
	{
	  exception = x;
	}
    else
      runnable.run ();

    if (notifier != null)
      {
        synchronized (notifier)
	  {
	    notifier.notifyAll ();
	  }
      }
  }

  public Exception getException ()
  {
    return exception;
  }

  public String paramString ()
  {
    String r;
    if (id == INVOCATION_DEFAULT)
      r = "INVOCATION_DEFAULT";
    else
      r = "unknown type";

    r += ",runnable=" + runnable + ",notifier=" + notifier + 
         ",catchExceptions=" + catchExceptions;    
    return r;
  }

  protected boolean catchExceptions;
  protected Object notifier;
  protected Runnable runnable;

  private Exception exception;
}
