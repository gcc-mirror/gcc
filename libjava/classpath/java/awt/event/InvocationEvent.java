/* InvocationEvent.java -- call a runnable when dispatched
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.awt.event;

import java.awt.AWTEvent;
import java.awt.ActiveEvent;
import java.awt.EventQueue;

/**
 * This event executes {@link Runnable#run()} of a target object when it is
 * dispatched. This class is used by calls to <code>invokeLater</code> and
 * <code>invokeAndWait</code>, so client code can use this fact to avoid
 * writing special-casing AWTEventListener objects.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see ActiveEvent
 * @see EventQueue#invokeLater(Runnable)
 * @see EventQueue#invokeAndWait(Runnable)
 * @see AWTEventListener
 * @since 1.2
 * @status updated to 1.4
 */
public class InvocationEvent extends AWTEvent implements ActiveEvent
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 436056344909459450L;

  /** This is the first id in the range of event ids used by this class. */
  public static final int INVOCATION_FIRST = 1200;

  /** This is the default id for this event type. */
  public static final int INVOCATION_DEFAULT = 1200;

  /** This is the last id in the range of event ids used by this class. */
  public static final int INVOCATION_LAST = 1200;

  /**
   * This is the <code>Runnable</code> object to call when dispatched.
   *
   * @serial the runnable to execute
   */
  protected Runnable runnable;

  /**
   * This is the object to call <code>notifyAll()</code> on when
   * the call to <code>run()</code> returns, or <code>null</code> if no
   * object is to be notified.
   *
   * @serial the object to notify
   */
  protected Object notifier;

  /**
   * This variable is set to <code>true</code> if exceptions are caught
   * and stored in a variable during the call to <code>run()</code>, otherwise
   * exceptions are ignored and propagate up.
   *
   * @serial true to catch exceptions
   */
  protected boolean catchExceptions;

  /**
   * This is the caught exception thrown in the <code>run()</code> method. It
   * is null if exceptions are ignored, the run method hasn't completed, or
   * there were no exceptions.
   *
   * @serial the caught exception, if any
   */
  private Exception exception;

  /**
   * This is the caught Throwable thrown in the <code>run()</code> method.
   * It is null if throwables are ignored, the run method hasn't completed,
   * or there were no throwables thrown.
   */
  private Throwable throwable;

  /**
   * The timestamp when this event was created.
   *
   * @see #getWhen()
   * @serial the timestamp
   * @since 1.4
   */
  private final long when = EventQueue.getMostRecentEventTime();

  /**
   * Initializes a new instance of <code>InvocationEvent</code> with the
   * specified source and runnable.
   *
   * @param source the source of the event
   * @param runnable the <code>Runnable</code> object to invoke
   * @throws IllegalArgumentException if source is null
   */
  public InvocationEvent(Object source, Runnable runnable)
  {
    this(source, INVOCATION_DEFAULT, runnable, null, false);
  }

  /**
   * Initializes a new instance of <code>InvocationEvent</code> with the
   * specified source, runnable, and notifier.  It will also catch exceptions
   * if specified. If notifier is non-null, this will call notifyAll() on
   * the object when the runnable is complete. If catchExceptions is true,
   * this traps any exception in the runnable, otherwise it lets the exception
   * propagate up the Event Dispatch thread.
   *
   * @param source the source of the event
   * @param runnable the <code>Runnable</code> object to invoke
   * @param notifier the object to notify, or null
   * @param catchExceptions true to catch exceptions from the runnable
   */
  public InvocationEvent(Object source, Runnable runnable, Object notifier,
                         boolean catchExceptions)
  {
    this(source, INVOCATION_DEFAULT, runnable, notifier, catchExceptions);
  }

  /**
   * Initializes a new instance of <code>InvocationEvent</code> with the
   * specified source, runnable, and notifier.  It will also catch exceptions
   * if specified. If notifier is non-null, this will call notifyAll() on
   * the object when the runnable is complete. If catchExceptions is true,
   * this traps any exception in the runnable, otherwise it lets the exception
   * propagate up the Event Dispatch thread. Note that an invalid id leads to
   * unspecified results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param runnable the <code>Runnable</code> object to invoke
   * @param notifier the object to notify, or null
   * @param catchExceptions true to catch exceptions from the runnable
   */
  protected InvocationEvent(Object source, int id, Runnable runnable,
                            Object notifier, boolean catchExceptions)
  {
    super(source, id);
    this.runnable = runnable;
    this.notifier = notifier;
    this.catchExceptions = catchExceptions;
  }

  /**
   * This method calls the <code>run()</code> method of the runnable, traps
   * exceptions if instructed to do so, and calls <code>notifyAll()</code>
   * on any notifier if all worked successfully.
   */
  public void dispatch()
  {
    if (catchExceptions)
      try
        {
          runnable.run();
        }
      catch (Throwable t)
        {
          throwable = t;
          if (t instanceof Exception)
            exception = (Exception)t;
        }
    else
      runnable.run();

    Object o = notifier;
    if (o != null)
      synchronized(o)
        {
          o.notifyAll();
        }
  }

  /**
   * This method returns the exception that occurred during the execution of
   * the runnable, or <code>null</code> if not exception was thrown or
   * exceptions were not caught.
   *
   * @return the exception thrown by the runnable
   */
  public Exception getException()
  {
    return exception;
  }

  /**
   * Returns a throwable caught while executing the Runnable's run() method.
   * Null if none was thrown or if this InvocationEvent doesn't catch
   * throwables.
   * @return the caught Throwable
   * @since 1.5
   */
  public Throwable getThrowable()
  {
    return throwable;
  }

  /**
   * Gets the timestamp of when this event was created.
   *
   * @return the timestamp of this event
   * @since 1.4
   */
  public long getWhen()
  {
    return when;
  }

  /**
   * This method returns a string identifying this event. This is formatted as:
   * <code>"INVOCATION_DEFAULT,runnable=" + runnable + ",notifier=" + notifier
   * + ",catchExceptions=" + catchExceptions + ",when=" + getWhen()</code>.
   *
   * @return a string identifying this event
   */
  public String paramString()
  {
    return (id == INVOCATION_DEFAULT ? "INVOCATION_DEFAULT,runnable="
            : "unknown type,runnable=") + runnable + ",notifier=" + notifier
      + ",catchExceptions=" + catchExceptions + ",when=" + when;
  }
} // class InvocationEvent
