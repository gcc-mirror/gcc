/* Timer.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.Serializable;
import java.util.EventListener;

import javax.swing.event.EventListenerList;

/**
 * Fires one or more action events after the specified delay.
 * @author Ronald Veldema
 * @author Audrius Meskauskas (audriusa@Bionformatics.org) - bug fixes
 * and documentation comments
 */
public class Timer
  implements Serializable
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = -1116180831621385484L;

  /** DOCUMENT ME! */
  protected EventListenerList listenerList = new EventListenerList();

  // This object manages a "queue" of virtual actionEvents, maintained as a
  // simple long counter. When the timer expires, a new event is queued,
  // and a dispatcher object is pushed into the system event queue. When
  // the system thread runs the dispatcher, it will fire as many
  // ActionEvents as have been queued, unless the timer is set to
  // coalescing mode, in which case it will fire only one ActionEvent.

  /** DOCUMENT ME! */
  private long queue;

  /** DOCUMENT ME! */
  private Object queueLock = new Object();

  /** DOCUMENT ME! */
  private Waker waker;

  private Runnable drainer = new Runnable() 
    {
      public void run()
      {
        drainEvents();
      }
    };

  /**
   * DOCUMENT ME!
   * Package-private to avoid an accessor method.
   */
  private void queueEvent()
  {
    synchronized (queueLock)
      {
	queue++;
	if (queue == 1)
	  SwingUtilities.invokeLater(drainer);
      }
  }

  /**
   * DOCUMENT ME!
   * This is package-private to avoid an accessor method.
   */
  void drainEvents()
  {
    synchronized (queueLock)
      {
	if (isCoalesce())
	  {
	    if (queue > 0)
	      fireActionPerformed();
	  }
	else
	  {
	    while (queue > 0)
	      {
		fireActionPerformed();
		queue--;
	      }
	  }
	queue = 0;
      }
  }

  static boolean logTimers;

  /**
   * <code>true</code> if the timer coalesces events.
   */
  boolean coalesce = true;

  /**
   * <code>true</code> if the timer is firing repetetive events.
   */
  boolean repeats = true;

  /**
   * <code>true</code> if the timer is currently active, firing events
   * as scheduled.
   */
  boolean running;

  /** DOCUMENT ME! */
  int ticks;

  /**
   * The delay between subsequent repetetive events.
   */
  int delay;

  /**
   * The initial delay before the first event.
   */
  int initialDelay;

  /**
   * DOCUMENT ME!
   */
  private class Waker extends Thread
  {
    /**
     * DOCUMENT ME!
     */
    public void run()
    {
      running = true;
      try
        {
	  sleep(initialDelay);

	  while (running)
	    {
	      try
	        {
		  sleep(delay);
	        }
	      catch (InterruptedException e)
	        {
		  return;
	        }
	      queueEvent();

	      if (logTimers)
		System.out.println("javax.swing.Timer -> clocktick");

	      if (! repeats)
		break;
	    }
	  running = false;
        }
      catch (Exception e)
        {
//	  System.out.println("swing.Timer::" + e);
        }
    }
  }

  /**
   * Creates a new Timer object.
   *
   * @param d the default value for both initial and between event delay, in
   * milliseconds.
   * @param listener the first action listener, can be <code>null</code>.
   */
  public Timer(int d, ActionListener listener)
  {
    delay = d;

    if (listener != null)
      addActionListener(listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   */
  public void setCoalesce(boolean c)
  {
    coalesce = c;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isCoalesce()
  {
    return coalesce;
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void addActionListener(ActionListener listener)
  {
    listenerList.add(ActionListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void removeActionListener(ActionListener listener)
  {
    listenerList.remove(ActionListener.class, listener);
  }

  /**
   * Get the event listeners of the given type that are listening for the
   * events, fired by this timer.
   *
   * @param listenerType the listener type (for example, ActionListener.class)
   *
   * @return the array of event listeners that are listening for the events,
   * fired by this timer
   * @since 1.3
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   *
   * @since 1.4
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) listenerList.getListeners(ActionListener.class);
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireActionPerformed(ActionEvent event)
  {
    ActionListener[] listeners = getActionListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners[i].actionPerformed(event);
  }

  /**
   * DOCUMENT ME!
   */
  void fireActionPerformed()
  {
    fireActionPerformed(new ActionEvent(this, ticks++, "Timer"));
  }

  /**
   * Set the timer logging state. If it is set to <code>true</code>, the
   * timer prints a message to {@link System#out} when firing each
   * action event.
   *
   * @param lt <code>true</code> if logging is enabled, <code>false</code>
   * (default value) otherwise
   */
  public static void setLogTimers(boolean lt)
  {
    logTimers = lt;
  }

  /**
   * Return the logging state.
   *
   * @return <code>true</code> if the timer is printing a message to
   * {@link System#out}
   * when firing each action event
   */
  public static boolean getLogTimers()
  {
    return logTimers;
  }

  /**
   * Set the delay between firing the subsequent events.
   * This parameter does not change the value of the initial delay before
   * firing the first event.
   *
   * @param d The time gap between the subsequent events, in milliseconds
   */
  public void setDelay(int d)
  {
    delay = d;
  }

  /**
   * Get the delay between firing the subsequent events.
   *
   * @return The delay between subsequent events, in milliseconds
   */
  public int getDelay()
  {
    return delay;
  }

  /**
   * Set the intial delay before firing the first event since calling
   * the {@link #start()} method. If the initial delay has not been
   * set, it is assumed having the same value as the delay between the
   * subsequent events.
   *
   * @param i the initial delay, in milliseconds
   */
  public void setInitialDelay(int i)
  {
    initialDelay = i;
  }

  /**
   * Get the intial delay before firing the first event since calling
   * the {@link #start()} method. If the initial delay has not been
   * set, returns the same value as {@link #getDelay()}.
   *
   * @return the initial delay before firing the first action event.
   */
  public int getInitialDelay()
  {
    return initialDelay;
  }

  /**
   * Enable firing the repetetive events.
   *
   * @param r <code>true</code> (default value) to fire repetetive events.
   * <code>false</code> to fire
   * only one event after the initial delay
   */
  public void setRepeats(boolean r)
  {
    repeats = r;
  }

  /**
   * Check is this timer fires repetetive events.
   *
   * @return <code>true</code> if the timer fires repetetive events,
   * <code>false</code> if it fires
   * only one event after the initial delay
   */
  public boolean isRepeats()
  {
    return repeats;
  }

  /**
   * Get the timer state.
   *
   * @return <code>true</code> if the timer has been started and is firing
   * the action events as scheduled. <code>false</code>
   * if the timer is inactive.
   */
  public boolean isRunning()
  {
    return running;
  }

  /**
   * DOCUMENT ME!
   */
  public void start()
  {
    if (isRunning())
      return;
    waker = new Waker();
    waker.start();
  }

  /**
   * DOCUMENT ME!
   */
  public void restart()
  {
    stop();
    start();
  }

  /**
   * DOCUMENT ME!
   */
  public void stop()
  {
    running = false;
    if (waker != null)
      waker.interrupt();
    synchronized (queueLock)
      {
        queue = 0;
      }
  }
}
