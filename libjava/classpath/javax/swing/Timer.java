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


package javax.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.Serializable;
import java.util.EventListener;

import javax.swing.event.EventListenerList;

/**
 * Fires one or more action events after the specified delay.  This is
 * a specialised version of <code>java.util.Timer</code> just for
 * firing <code>ActionEvent</code>s. All Timers share one (daemon)
 * Thread (or java.util.Timer). All events are fired from the event
 * queue.
 * 
 * @author Ronald Veldema
 * @author Audrius Meskauskas (audriusa@Bionformatics.org) - bug fixes
 * and documentation comments
 */
public class Timer
  implements Serializable
{
  /**
   * Given to the shared java.util.Timer to (possibly repeatedly) call
   * queueEvent().
   */
  private class Task extends java.util.TimerTask
  {
    public void run()
    {
      if (logTimers)
	System.out.println("javax.swing.Timer -> queueEvent()");
      queueEvent();

      if (!repeats)
	task = null;
    }
  }

  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = -1116180831621385484L;

  /**
   * The encloding class, used with {@link SwingUtilities#invokeLater}
   * to invoke the {@link #drainEvents()}.
   */
  private Runnable drainer = new Runnable()
    {
      public void run()
      {
        drainEvents();
      }
    };

  /**
   * The static java.util.Timer daemon which will be used to schedule
   * all javax.swing.Timer.Task objects. The daemon will always be
   * running, even if there's no task scheduled in it.
   */
  private static java.util.Timer timer = new java.util.Timer("swing.Timer",
							     true);

  /**
   * If <code>true</code>, the timer prints a message to
   * {@link System#out} when firing each event.
   */
  static boolean logTimers;

  /**
   * A field to store all listeners who are listening to this timer.
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * <code>true</code> if the timer coalesces events.
   */
  boolean coalesce = true;

  /**
   * <code>true</code> if the timer is firing repetetive events.
   */
  boolean repeats = true;

  /**
   * The delay between subsequent repetetive events.
   */
  int delay;

  /**
   * The initial delay before the first event.
   */
  int initialDelay;

  /**
   * The number of events that have been already fired by this timer.
   * This is used as a numeric identifier for the next event that would
   * be fired.
   */
  int ticks;

  /**
   * The task that calls queueEvent(). When null this Timer is stopped.
   */
  private Task task;

  /**
   * This object manages a "queue" of virtual actionEvents, maintained as a
   * simple long counter. When the timer expires, a new event is queued,
   * and a dispatcher object is pushed into the system event queue. When
   * the system thread runs the dispatcher, it will fire as many
   * ActionEvents as have been queued, unless the timer is set to
   * coalescing mode, in which case it will fire only one ActionEvent.
   */
  private long queue;

  /**
   * <code>synchronized(queueLock)</code> replaces
   * <code>synchronized(queue)</code> that is not supported by this language.
   */
  private Object queueLock = new Object();

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
    initialDelay = d;

    if (listener != null)
      addActionListener(listener);
  }

  /**
   * Get the array of action listeners.
   *
   * @return the array of action listeners that are listening for the events,
   * fired by this timer
   *
   * @since 1.4
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) listenerList.getListeners(ActionListener.class);
  }

  /**
   * Sets whether the Timer coalesces multiple pending event firings.
   * If the coalescing is enabled, the multiple events that have not been
   * fired on time are replaced by the single event. The events may not
   * be fired on time if the application is busy.
   *
   * @param c <code>true</code> (default) to enable the event coalescing,
   * <code>false</code> otherwise
   */
  public void setCoalesce(boolean c)
  {
    coalesce = c;
  }

  /**
   * Checks if the Timer coalesces multiple pending event firings.
   * If the coalescing is enabled, the multiple events that have not been
   * fired on time are replaced by the single event. The events may not
   * be fired on time if the application is busy.
   *
   * @return <code>true</code> if the coalescing is enabled,
   * <code>false</code> otherwise
   */
  public boolean isCoalesce()
  {
    return coalesce;
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
    return task != null;
  }

  /**
   * Add the action listener
   *
   * @param listener the action listener to add
   */
  public void addActionListener(ActionListener listener)
  {
    listenerList.add(ActionListener.class, listener);
  }

  /**
   * Remove the action listener.
   *
   * @param listener the action listener to remove
   */
  public void removeActionListener(ActionListener listener)
  {
    listenerList.remove(ActionListener.class, listener);
  }

  /**
   * Cancel all pending tasks and fire the first event after the initial
   * delay.
   */
  public void restart()
  {
    stop();
    start();
  }

  /**
   * Start firing the action events.
   */
  public void start()
  {
    Task t = task;
    if (t == null)
      {
	t = new Task();
	if (isRepeats())
	  timer.schedule(t, getInitialDelay(), getDelay());
	else
	  timer.schedule(t, getInitialDelay());
	task = t;
      }
  }

  /**
   * Stop firing the action events.
   */
  public void stop()
  {
    Task t = task;
    if (t != null)
      {
	t.cancel();
	task = null;
      }
  }

  /**
   * Fire the given action event to the action listeners.
   *
   * @param event the event to fire
   */
  protected void fireActionPerformed(ActionEvent event)
  {
    ActionListener[] listeners = getActionListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners [ i ].actionPerformed(event);
  }

  /**
   * Fire the action event, named "Timer" and having the numeric
   * identifier, equal to the numer of events that have been
   * already fired before.
   */
  void fireActionPerformed()
  {
    fireActionPerformed(new ActionEvent(this, ticks++, "Timer"));
  }

  /**
   * Fire the queued action events.
   * In the coalescing mode, a single event is fired as a replacement
   * for all queued events. In non coalescing mode, a series of
   * all queued events is fired.
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

  /**
  * Post a scheduled event to the event queue.
  * Package-private to avoid an accessor method.
  */
  void queueEvent()
  {
    synchronized(queueLock)
      {
	queue++;
	if (queue == 1)
	  SwingUtilities.invokeLater(drainer);
      }
  }
}
