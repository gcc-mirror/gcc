/* TimerTask.java -- Task that can be run at a later time if given to a Timer.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

package java.util;

/**
 * Task that can be run at a later time if given to a Timer.
 * The TimerTask must implement a run method that will be called by the
 * Timer when the task is scheduled for execution. The task can check when
 * it should have been scheduled and cancel itself when no longer needed.
 * <p>
 * Example:
 * <pre>
 *  Timer timer = new Timer();
 *  TimerTask task = new TimerTask() {
 *      public void run() {
 *      if (this.scheduledExecutionTime() &lt; System.currentTimeMillis() + 500)
 *          // Do something
 *      else
 *          // Complain: We are more then half a second late!
 *      if (someStopCondition)
 *          this.cancel(); // This was our last execution
 *  };
 *  timer.scheduleAtFixedRate(task, 1000, 1000); // schedule every second
 * </pre>
 * <p>
 * Note that a TimerTask object is a one shot object and can only given once
 * to a Timer. (The Timer will use the TimerTask object for bookkeeping,
 * in this implementation).
 * <p>
 * This class also implements <code>Runnable</code> to make it possible to
 * give a TimerTask directly as a target to a <code>Thread</code>.
 *
 * @see Timer
 * @since 1.3
 * @author Mark Wielaard (mark@klomp.org)
 */
public abstract class TimerTask implements Runnable
{
  /**
   * If positive the next time this task should be run.
   * If negative this TimerTask is canceled or executed for the last time.
   */
  long scheduled;

  /**
   * If positive the last time this task was run.
   * If negative this TimerTask has not yet been scheduled.
   */
  long lastExecutionTime;

  /**
   * If positive the number of milliseconds between runs of this task.
   * If -1 this task doesn't have to be run more then once.
   */
  long period;

  /**
   * If true the next time this task should be run is relative to
   * the last scheduled time, otherwise it can drift in time.
   */
  boolean fixed;

  /**
   * Creates a TimerTask and marks it as not yet scheduled.
   */
  protected TimerTask()
  {
    this.scheduled = 0;
    this.lastExecutionTime = -1;
  }

  /**
   * Marks the task as canceled and prevents any further execution.
   * Returns true if the task was scheduled for any execution in the future
   * and this cancel operation prevents that execution from happening.
   * <p>
   * A task that has been canceled can never be scheduled again.
   * <p>
   * In this implementation the TimerTask it is possible that the Timer does
   * keep a reference to the TimerTask until the first time the TimerTask
   * is actually scheduled. But the reference will disappear immediatly when
   * cancel is called from within the TimerTask run method.
   */
  public boolean cancel()
  {
    boolean prevented_execution = (this.scheduled >= 0);
    this.scheduled = -1;
    return prevented_execution;
  }

  /**
   * Method that is called when this task is scheduled for execution.
   */
  public abstract void run();

  /**
   * Returns the last time this task was scheduled or (when called by the
   * task from the run method) the time the current execution of the task
   * was scheduled. When the task has not yet run the return value is
   * undefined.
   * <p>
   * Can be used (when the task is scheduled at fixed rate) to see the
   * difference between the requested schedule time and the actual time
   * that can be found with <code>System.currentTimeMillis()</code>.
   */
  public long scheduledExecutionTime()
  {
    return lastExecutionTime;
  }
}
