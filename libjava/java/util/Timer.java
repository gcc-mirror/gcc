/* Timer.java -- Timer that runs TimerTasks at a later time.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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
 * Timer that can run TimerTasks at a later time.
 * TimerTasks can be scheduled for one time execution at some time in the
 * future. They can be scheduled to be rescheduled at a time period after the
 * task was last executed. Or they can be scheduled to be executed repeatedly
 * at a fixed rate.
 * <p>
 * The normal scheduling will result in a more or less even delay in time
 * between successive executions, but the executions could drift in time if
 * the task (or other tasks) takes a long time to execute. Fixed delay
 * scheduling guarantees more or less that the task will be executed at a
 * specific time, but if there is ever a delay in execution then the period
 * between successive executions will be shorter. The first method of
 * repeated scheduling is preferred for repeated tasks in response to user
 * interaction, the second method of repeated scheduling is preferred for tasks
 * that act like alarms.
 * <p>
 * The Timer keeps a binary heap as a task priority queue which means that
 * scheduling and serving of a task in a queue of n tasks costs O(log n).
 *
 * @see TimerTask
 * @since 1.3
 * @author Mark Wielaard (mark@klomp.org)
 */
public class Timer
{
  /**
   * Priority Task Queue.
   * TimerTasks are kept in a binary heap.
   * The scheduler calls sleep() on the queue when it has nothing to do or
   * has to wait. A sleeping scheduler can be notified by calling interrupt()
   * which is automatically called by the enqueue(), cancel() and
   * timerFinalized() methods.
   */
  private static final class TaskQueue
  {
    /** Default size of this queue */
    private final int DEFAULT_SIZE = 32;

    /** Whether to return null when there is nothing in the queue */
    private boolean nullOnEmpty;

    /**
     * The heap containing all the scheduled TimerTasks
     * sorted by the TimerTask.scheduled field.
     * Null when the stop() method has been called.
     */
    private TimerTask heap[];

    /**
     * The actual number of elements in the heap
     * Can be less then heap.length.
     * Note that heap[0] is used as a sentinel.
     */
    private int elements;

    /**
     * Creates a TaskQueue of default size without any elements in it.
     */
    public TaskQueue()
    {
      heap = new TimerTask[DEFAULT_SIZE];
      elements = 0;
      nullOnEmpty = false;
    }

    /**
     * Adds a TimerTask at the end of the heap.
     * Grows the heap if necessary by doubling the heap in size.
     */
    private void add(TimerTask task)
    {
      elements++;
      if (elements == heap.length)
	{
	  TimerTask new_heap[] = new TimerTask[heap.length * 2];
	  System.arraycopy(heap, 0, new_heap, 0, heap.length);
	  heap = new_heap;
	}
      heap[elements] = task;
    }

    /**
     * Removes the last element from the heap.
     * Shrinks the heap in half if
     * elements+DEFAULT_SIZE/2 <= heap.length/4.
     */
    private void remove()
    {
      // clear the entry first
      heap[elements] = null;
      elements--;
      if (elements + DEFAULT_SIZE / 2 <= (heap.length / 4))
	{
	  TimerTask new_heap[] = new TimerTask[heap.length / 2];
	  System.arraycopy(heap, 0, new_heap, 0, elements + 1);
	  heap = new_heap;
	}
    }

    /**
     * Adds a task to the queue and puts it at the correct place
     * in the heap.
     */
    public synchronized void enqueue(TimerTask task)
    {
      // Check if it is legal to add another element
      if (heap == null)
	{
	  throw new IllegalStateException
	    ("cannot enqueue when stop() has been called on queue");
	}

      heap[0] = task;		// sentinel
      add(task);		// put the new task at the end
      // Now push the task up in the heap until it has reached its place
      int child = elements;
      int parent = child / 2;
      while (heap[parent].scheduled > task.scheduled)
	{
	  heap[child] = heap[parent];
	  child = parent;
	  parent = child / 2;
	}
      // This is the correct place for the new task
      heap[child] = task;
      heap[0] = null;		// clear sentinel
      // Maybe sched() is waiting for a new element
      this.notify();
    }

    /**
     * Returns the top element of the queue.
     * Can return null when no task is in the queue.
     */
    private TimerTask top()
    {
      if (elements == 0)
	{
	  return null;
	}
      else
	{
	  return heap[1];
	}
    }

    /**
     * Returns the top task in the Queue.
     * Removes the element from the heap and reorders the heap first.
     * Can return null when there is nothing in the queue.
     */
    public synchronized TimerTask serve()
    {
      // The task to return
      TimerTask task = null;

      while (task == null)
	{
	  // Get the next task
	  task = top();

	  // return null when asked to stop
	  // or if asked to return null when the queue is empty
	  if ((heap == null) || (task == null && nullOnEmpty))
	    {
	      return null;
	    }

	  // Do we have a task?
	  if (task != null)
	    {
	      // The time to wait until the task should be served
	      long time = task.scheduled - System.currentTimeMillis();
	      if (time > 0)
		{
		  // This task should not yet be served
		  // So wait until this task is ready
		  // or something else happens to the queue
		  task = null;	// set to null to make sure we call top()
		  try
		    {
		      this.wait(time);
		    }
		  catch (InterruptedException _)
		    {
		    }
		}
	    }
	  else
	    {
	      // wait until a task is added
	      // or something else happens to the queue
	      try
		{
		  this.wait();
		}
	      catch (InterruptedException _)
		{
		}
	    }
	}

      // reconstruct the heap
      TimerTask lastTask = heap[elements];
      remove();

      // drop lastTask at the beginning and move it down the heap
      int parent = 1;
      int child = 2;
      heap[1] = lastTask;
      while (child <= elements)
	{
	  if (child < elements)
	    {
	      if (heap[child].scheduled > heap[child + 1].scheduled)
		{
		  child++;
		}
	    }

	  if (lastTask.scheduled <= heap[child].scheduled)
	    break;		// found the correct place (the parent) - done

	  heap[parent] = heap[child];
	  parent = child;
	  child = parent * 2;
	}

      // this is the correct new place for the lastTask
      heap[parent] = lastTask;

      // return the task
      return task;
    }

    /**
     * When nullOnEmpty is true the serve() method will return null when
     * there are no tasks in the queue, otherwise it will wait until
     * a new element is added to the queue. It is used to indicate to
     * the scheduler that no new tasks will ever be added to the queue.
     */
    public synchronized void setNullOnEmpty(boolean nullOnEmpty)
    {
      this.nullOnEmpty = nullOnEmpty;
      this.notify();
    }

    /**
     * When this method is called the current and all future calls to
     * serve() will return null. It is used to indicate to the Scheduler
     * that it should stop executing since no more tasks will come.
     */
    public synchronized void stop()
    {
      this.heap = null;
      this.elements = 0;
      this.notify();
    }

  }				// TaskQueue

  /**
   * The scheduler that executes all the tasks on a particular TaskQueue,
   * reschedules any repeating tasks and that waits when no task has to be
   * executed immediatly. Stops running when canceled or when the parent
   * Timer has been finalized and no more tasks have to be executed.
   */
  private static final class Scheduler implements Runnable
  {
    // The priority queue containing all the TimerTasks.
    private TaskQueue queue;

    /**
     * Creates a new Scheduler that will schedule the tasks on the
     * given TaskQueue.
     */
    public Scheduler(TaskQueue queue)
    {
      this.queue = queue;
    }

    public void run()
    {
      TimerTask task;
      while ((task = queue.serve()) != null)
	{
	  // If this task has not been canceled
	  if (task.scheduled >= 0)
	    {

	      // Mark execution time
	      task.lastExecutionTime = task.scheduled;

	      // Repeatable task?
	      if (task.period < 0)
		{
		  // Last time this task is executed
		  task.scheduled = -1;
		}

	      // Run the task
	      try
		{
		  task.run();
		}
	      catch (Throwable t)
		{		
		  /* ignore all errors */
		}
	    }

	  // Calculate next time and possibly re-enqueue.
	  if (task.scheduled >= 0)
	    {
	      if (task.fixed)
		{
		  task.scheduled += task.period;
		}
	      else
		{
		  task.scheduled = task.period + System.currentTimeMillis();
		}

	      try
	        {
	          queue.enqueue(task);
		}
	      catch (IllegalStateException ise)
	        {
		  // Ignore. Apparently the Timer queue has been stopped.
		}
	    }
	}
    }
  }				// Scheduler

  // Number of Timers created.
  // Used for creating nice Thread names.
  private static int nr = 0;

  // The queue that all the tasks are put in.
  // Given to the scheduler
  private TaskQueue queue;

  // The Scheduler that does all the real work
  private Scheduler scheduler;

  // Used to run the scheduler.
  // Also used to checked if the Thread is still running by calling
  // thread.isAlive(). Sometimes a Thread is suddenly killed by the system
  // (if it belonged to an Applet).
  private Thread thread;

  // When cancelled we don't accept any more TimerTasks.
  private boolean canceled;

  /**
   * Creates a new Timer with a non daemon Thread as Scheduler, with normal
   * priority and a default name.
   */
  public Timer()
  {
    this(false);
  }

  /**
   * Creates a new Timer with a daemon Thread as scheduler if daemon is true,
   * with normal priority and a default name.
   */
  public Timer(boolean daemon)
  {
    this(daemon, Thread.NORM_PRIORITY);
  }

  /**
   * Creates a new Timer with a daemon Thread as scheduler if daemon is true,
   * with the priority given and a default name.
   */
  private Timer(boolean daemon, int priority)
  {
    this(daemon, priority, "Timer-" + (++nr));
  }

  /**
   * Creates a new Timer with a daemon Thread as scheduler if daemon is true,
   * with the priority and name given.E
   */
  private Timer(boolean daemon, int priority, String name)
  {
    canceled = false;
    queue = new TaskQueue();
    scheduler = new Scheduler(queue);
    thread = new Thread(scheduler, name);
    thread.setDaemon(daemon);
    thread.setPriority(priority);
    thread.start();
  }

  /**
   * Cancels the execution of the scheduler. If a task is executing it will
   * normally finish execution, but no other tasks will be executed and no
   * more tasks can be scheduled.
   */
  public void cancel()
  {
    canceled = true;
    queue.stop();
  }

  /**
   * Schedules the task at Time time, repeating every period
   * milliseconds if period is positive and at a fixed rate if fixed is true.
   *
   * @exception IllegalArgumentException if time is negative
   * @exception IllegalStateException if the task was already scheduled or
   * canceled or this Timer is canceled or the scheduler thread has died
   */
  private void schedule(TimerTask task, long time, long period, boolean fixed)
  {
    if (time < 0)
      throw new IllegalArgumentException("negative time");

    if (task.scheduled == 0 && task.lastExecutionTime == -1)
      {
	task.scheduled = time;
	task.period = period;
	task.fixed = fixed;
      }
    else
      {
	throw new IllegalStateException
	  ("task was already scheduled or canceled");
      }

    if (!this.canceled && this.thread != null)
      {
	queue.enqueue(task);
      }
    else
      {
	throw new IllegalStateException
	  ("timer was canceled or scheduler thread has died");
      }
  }

  private static void positiveDelay(long delay)
  {
    if (delay < 0)
      {
	throw new IllegalArgumentException("delay is negative");
      }
  }

  private static void positivePeriod(long period)
  {
    if (period < 0)
      {
	throw new IllegalArgumentException("period is negative");
      }
  }

  /**
   * Schedules the task at the specified data for one time execution.
   *
   * @exception IllegalArgumentException if date.getTime() is negative
   * @exception IllegalStateException if the task was already scheduled or
   * canceled or this Timer is canceled or the scheduler thread has died
   */
  public void schedule(TimerTask task, Date date)
  {
    long time = date.getTime();
    schedule(task, time, -1, false);
  }

  /**
   * Schedules the task at the specified date and reschedules the task every
   * period milliseconds after the last execution of the task finishes until
   * this timer or the task is canceled.
   *
   * @exception IllegalArgumentException if period or date.getTime() is
   * negative
   * @exception IllegalStateException if the task was already scheduled or
   * canceled or this Timer is canceled or the scheduler thread has died
   */
  public void schedule(TimerTask task, Date date, long period)
  {
    positivePeriod(period);
    long time = date.getTime();
    schedule(task, time, period, false);
  }

  /**
   * Schedules the task after the specified delay milliseconds for one time
   * execution.
   *
   * @exception IllegalArgumentException if delay or
   * System.currentTimeMillis + delay is negative
   * @exception IllegalStateException if the task was already scheduled or
   * canceled or this Timer is canceled or the scheduler thread has died
   */
  public void schedule(TimerTask task, long delay)
  {
    positiveDelay(delay);
    long time = System.currentTimeMillis() + delay;
    schedule(task, time, -1, false);
  }

  /**
   * Schedules the task after the delay milliseconds and reschedules the
   * task every period milliseconds after the last execution of the task
   * finishes until this timer or the task is canceled.
   *
   * @exception IllegalArgumentException if delay or period is negative
   * @exception IllegalStateException if the task was already scheduled or
   * canceled or this Timer is canceled or the scheduler thread has died
   */
  public void schedule(TimerTask task, long delay, long period)
  {
    positiveDelay(delay);
    positivePeriod(period);
    long time = System.currentTimeMillis() + delay;
    schedule(task, time, period, false);
  }

  /**
   * Schedules the task at the specified date and reschedules the task at a
   * fixed rate every period milliseconds until this timer or the task is
   * canceled.
   *
   * @exception IllegalArgumentException if period or date.getTime() is
   * negative
   * @exception IllegalStateException if the task was already scheduled or
   * canceled or this Timer is canceled or the scheduler thread has died
   */
  public void scheduleAtFixedRate(TimerTask task, Date date, long period)
  {
    positivePeriod(period);
    long time = date.getTime();
    schedule(task, time, period, true);
  }

  /**
   * Schedules the task after the delay milliseconds and reschedules the task
   * at a fixed rate every period milliseconds until this timer or the task
   * is canceled.
   *
   * @exception IllegalArgumentException if delay or
   * System.currentTimeMillis + delay is negative
   * @exception IllegalStateException if the task was already scheduled or
   * canceled or this Timer is canceled or the scheduler thread has died
   */
  public void scheduleAtFixedRate(TimerTask task, long delay, long period)
  {
    positiveDelay(delay);
    positivePeriod(period);
    long time = System.currentTimeMillis() + delay;
    schedule(task, time, period, true);
  }

  /**
   * Tells the scheduler that the Timer task died
   * so there will be no more new tasks scheduled.
   */
  protected void finalize()
  {
    queue.setNullOnEmpty(true);
  }
}
