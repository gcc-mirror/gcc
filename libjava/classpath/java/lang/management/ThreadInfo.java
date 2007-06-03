/* ThreadInfo.java - Information on a thread
   Copyright (C) 2006 Free Software Foundation

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

package java.lang.management;

import java.util.Arrays;

import javax.management.openmbean.ArrayType;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;

/**
 * <p>
 * A class which maintains information about a particular
 * thread.  This information includes:
 * </p>
 * <ul>
 * <li><strong>General Thread Information:</strong>
 * <ul>
 * <li>The identifier of the thread.</li>
 * <li>The name of the thread.</li>
 * </ul>
 * </li>
 * <li><strong>Execution Information:</strong>
 * <ul>
 * <li>The current state of the thread (e.g. blocked, runnable)</li>
 * <li>The object upon which the thread is blocked, either because
 * the thread is waiting to obtain the monitor of that object to enter
 * one of its synchronized monitor, or because
 * {@link java.lang.Object#wait()} has been called while the thread
 * was within a method of that object.</li>
 * <li>The thread identifier of the current thread holding an object's
 * monitor, upon which the thread described here is blocked.</li>
 * <li>The stack trace of the thread (if requested on creation
 * of this object</li>
 * <li>The current locks held on object monitors by the thread.</li>
 * <li>The current locks held on ownable synchronizers by the thread.</li>
 * </ul>
 * <li><strong>Synchronization Statistics</strong>
 * <ul>
 * <li>The number of times the thread has been blocked waiting for
 * an object's monitor or in a {@link java.lang.Object#wait()} call.</li>
 * <li>The accumulated time the thread has been blocked waiting for
 * an object's monitor on in a {@link java.lang.Object#wait()} call.
 * The availability of these statistics depends on the virtual machine's
 * support for thread contention monitoring (see
 * {@link ThreadMXBean#isThreadContentionMonitoringSupported()}.</li>
 * </ul>
 * </li>
 * </ul>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 * @see ThreadMXBean#isThreadContentionMonitoringSupported()
 */
public class ThreadInfo
{

  /**
   * The id of the thread which this instance concerns.
   */
  private long threadId;

  /**
   * The name of the thread which this instance concerns.
   */
  private String threadName;

  /**
   * The state of the thread which this instance concerns.
   */
  private Thread.State threadState;

  /**
   * The number of times the thread has been blocked.
   */
  private long blockedCount;

  /**
   * The accumulated number of milliseconds the thread has
   * been blocked (used only with thread contention monitoring
   * support).
   */
  private long blockedTime;

  /**
   * The name of the monitor lock on which this thread
   * is blocked (if any).
   */
  private String lockName;

  /**
   * The id of the thread which owns the monitor lock on
   * which this thread is blocked, or <code>-1</code>
   * if there is no owner.
   */
  private long lockOwnerId;

  /**
   * The name of the thread which owns the monitor lock on
   * which this thread is blocked, or <code>null</code>
   * if there is no owner.
   */
  private String lockOwnerName;

  /**
   * The number of times the thread has been in a waiting
   * state.
   */
  private long waitedCount;

  /**
   * The accumulated number of milliseconds the thread has
   * been waiting (used only with thread contention monitoring
   * support).
   */
  private long waitedTime;

  /**
   * True if the thread is in a native method.
   */
  private boolean isInNative;

  /**
   * True if the thread is suspended.
   */
  private boolean isSuspended;

  /**
   * The stack trace of the thread.
   */
  private StackTraceElement[] trace;

  /**
   * The array of information on monitors locked by the thread.
   */
  private MonitorInfo[] lockedMonitors;

  /**
   * The array of information on ownable synchronizers locked
   * by the thread.
   */
  private LockInfo[] lockedSynchronizers;

  /**
   * Cache a local reference to the thread management bean.
   */
  private static ThreadMXBean bean = null;

  /**
   * Cache the {@link javax.management.openmbean.CompositeType}
   * for the {@link StackTraceElement}.
   */
  private static CompositeType seType;

  /**
   * Constructs a new {@link ThreadInfo} corresponding
   * to the thread specified.
   *
   * @param thread the thread on which the new instance
   *               will be based.
   * @param blockedCount the number of times the thread
   *                     has been blocked.
   * @param blockedTime the accumulated number of milliseconds
   *                    the specified thread has been blocked
   *                    (only used with contention monitoring enabled)
   * @param lock the monitor lock the thread is waiting for
   *             (only used if blocked)
   * @param lockOwner the thread which owns the monitor lock, or
   *                  <code>null</code> if it doesn't have an owner
   *                  (only used if blocked)
   * @param waitedCount the number of times the thread has been in a
   *                    waiting state.
   * @param waitedTime the accumulated number of milliseconds the
   *                   specified thread has been waiting
   *                   (only used with contention monitoring enabled)
   * @param isInNative true if the thread is in a native method.
   * @param isSuspended true if the thread is suspended.
   * @param trace the stack trace of the thread to a pre-determined
   *              depth (see VMThreadMXBeanImpl)
   */
  private ThreadInfo(Thread thread, long blockedCount, long blockedTime,
		     Object lock, Thread lockOwner, long waitedCount,
		     long waitedTime, boolean isInNative, boolean isSuspended,
		     StackTraceElement[] trace)
  {
    this(thread, blockedCount, blockedTime, lock, lockOwner, waitedCount,
	 waitedTime, isInNative, isSuspended, trace, new MonitorInfo[]{},
	 new LockInfo[]{});
  }

  /**
   * Constructs a new {@link ThreadInfo} corresponding
   * to the thread specified.
   *
   * @param thread the thread on which the new instance
   *               will be based.
   * @param blockedCount the number of times the thread
   *                     has been blocked.
   * @param blockedTime the accumulated number of milliseconds
   *                    the specified thread has been blocked
   *                    (only used with contention monitoring enabled)
   * @param lock the monitor lock the thread is waiting for
   *             (only used if blocked)
   * @param lockOwner the thread which owns the monitor lock, or
   *                  <code>null</code> if it doesn't have an owner
   *                  (only used if blocked)
   * @param waitedCount the number of times the thread has been in a
   *                    waiting state.
   * @param waitedTime the accumulated number of milliseconds the
   *                   specified thread has been waiting
   *                   (only used with contention monitoring enabled)
   * @param isInNative true if the thread is in a native method.
   * @param isSuspended true if the thread is suspended.
   * @param trace the stack trace of the thread to a pre-determined
   *              depth (see VMThreadMXBeanImpl)
   * @param lockedMonitors an array of {@link MonitorInfo} objects
   *                       representing locks held on object monitors
   *                       by the thread.
   * @param lockedSynchronizers an array of {@link LockInfo} objects
   *                            representing locks held on ownable
   *                            synchronizers by the thread. 
   * @since 1.6
   */
  private ThreadInfo(Thread thread, long blockedCount, long blockedTime,
		     Object lock, Thread lockOwner, long waitedCount,
		     long waitedTime, boolean isInNative, boolean isSuspended,
		     StackTraceElement[] trace, MonitorInfo[] lockedMonitors,
		     LockInfo[] lockedSynchronizers)
  {
    this(thread.getId(), thread.getName(), thread.getState(), blockedCount, blockedTime,
         lock == null ? null : lock.getClass().getName() + "@" + 
	       Integer.toHexString(System.identityHashCode(lock)),
         lockOwner == null ? -1 : lockOwner.getId(),
         lockOwner == null ? null : lockOwner.getName(),
         waitedCount, waitedTime, isInNative, isSuspended,
	 trace, lockedMonitors, lockedSynchronizers);
  }

  /**
   * Constructs a new {@link ThreadInfo} corresponding
   * to the thread details specified.
   *
   * @param threadId the id of the thread on which this
   *                 new instance will be based.
   * @param threadName the name of the thread on which
   *                 this new instance will be based.
   * @param threadState the state of the thread on which
   *                 this new instance will be based.
   * @param blockedCount the number of times the thread
   *                     has been blocked.
   * @param blockedTime the accumulated number of milliseconds
   *                    the specified thread has been blocked
   *                    (only used with contention monitoring enabled)
   * @param lockName the name of the monitor lock the thread is waiting for
   *                 (only used if blocked)
   * @param lockOwnerId the id of the thread which owns the monitor
   *                  lock, or <code>-1</code> if it doesn't have an owner
   *                  (only used if blocked)
   * @param lockOwnerName the name of the thread which owns the monitor
   *                  lock, or <code>null</code> if it doesn't have an 
   *                  owner (only used if blocked)
   * @param waitedCount the number of times the thread has been in a
   *                    waiting state.
   * @param waitedTime the accumulated number of milliseconds the
   *                   specified thread has been waiting
   *                   (only used with contention monitoring enabled)
   * @param isInNative true if the thread is in a native method.
   * @param isSuspended true if the thread is suspended.
   * @param trace the stack trace of the thread to a pre-determined
   *              depth (see VMThreadMXBeanImpl)
   */
  private ThreadInfo(long threadId, String threadName, Thread.State threadState,
		     long blockedCount, long blockedTime, String lockName, 
		     long lockOwnerId, String lockOwnerName, long waitedCount,
		     long waitedTime, boolean isInNative, boolean isSuspended,
		     StackTraceElement[] trace)
  {
    this(threadId, threadName, threadState, blockedCount, blockedTime,
	 lockName, lockOwnerId, lockOwnerName, waitedCount, waitedTime,
	 isInNative, isSuspended, trace, new MonitorInfo[]{}, new LockInfo[]{});
  }

  /**
   * Constructs a new {@link ThreadInfo} corresponding
   * to the thread details specified.
   *
   * @param threadId the id of the thread on which this
   *                 new instance will be based.
   * @param threadName the name of the thread on which
   *                 this new instance will be based.
   * @param threadState the state of the thread on which
   *                 this new instance will be based.
   * @param blockedCount the number of times the thread
   *                     has been blocked.
   * @param blockedTime the accumulated number of milliseconds
   *                    the specified thread has been blocked
   *                    (only used with contention monitoring enabled)
   * @param lockName the name of the monitor lock the thread is waiting for
   *                 (only used if blocked)
   * @param lockOwnerId the id of the thread which owns the monitor
   *                  lock, or <code>-1</code> if it doesn't have an owner
   *                  (only used if blocked)
   * @param lockOwnerName the name of the thread which owns the monitor
   *                  lock, or <code>null</code> if it doesn't have an 
   *                  owner (only used if blocked)
   * @param waitedCount the number of times the thread has been in a
   *                    waiting state.
   * @param waitedTime the accumulated number of milliseconds the
   *                   specified thread has been waiting
   *                   (only used with contention monitoring enabled)
   * @param isInNative true if the thread is in a native method.
   * @param isSuspended true if the thread is suspended.
   * @param trace the stack trace of the thread to a pre-determined
   *              depth (see VMThreadMXBeanImpl)
   * @param lockedMonitors an array of {@link MonitorInfo} objects
   *                       representing locks held on object monitors
   *                       by the thread.
   * @param lockedSynchronizers an array of {@link LockInfo} objects
   *                            representing locks held on ownable
   *                            synchronizers by the thread. 
   *
   * @since 1.6
   */
  private ThreadInfo(long threadId, String threadName, Thread.State threadState,
		     long blockedCount, long blockedTime, String lockName, 
		     long lockOwnerId, String lockOwnerName, long waitedCount,
		     long waitedTime, boolean isInNative, boolean isSuspended,
		     StackTraceElement[] trace, MonitorInfo[] lockedMonitors,
		     LockInfo[] lockedSynchronizers)
  {
    this.threadId = threadId;
    this.threadName = threadName;
    this.threadState = threadState;
    this.blockedCount = blockedCount;
    this.blockedTime = blockedTime;
    this.lockName = lockName;
    this.lockOwnerId = lockOwnerId;
    this.lockOwnerName = lockOwnerName;
    this.waitedCount = waitedCount;
    this.waitedTime = waitedTime;
    this.isInNative = isInNative;
    this.isSuspended = isSuspended;
    this.trace = trace;
    this.lockedMonitors = lockedMonitors;
    this.lockedSynchronizers = lockedSynchronizers;
  }

  /**
   * Checks for an attribute in a {@link CompositeData} structure
   * with the correct type.
   *
   * @param ctype the composite data type to check.
   * @param name the name of the attribute.
   * @param type the type to check for.
   * @throws IllegalArgumentException if the attribute is absent
   *                                  or of the wrong type.
   */
  static void checkAttribute(CompositeType ctype, String name,
			     OpenType type)
    throws IllegalArgumentException
  {
    OpenType foundType = ctype.getType(name);
    if (foundType == null)
      throw new IllegalArgumentException("Could not find a field named " +
					 name);
    if (!(foundType.equals(type)))
      throw new IllegalArgumentException("Field " + name + " is not of " +
					 "type " + type.getClassName());
  }

  /**
   * Returns the {@link javax.management.openmbean.CompositeType} for
   * a {@link StackTraceElement}.
   *
   * @return the type for the stack trace element.
   */
  static CompositeType getStackTraceType()
  {
    if (seType == null)
      try
	{
	  seType = new CompositeType(StackTraceElement.class.getName(),
				     "An element of a stack trace",
				     new String[] { "className", "methodName",
						    "fileName", "lineNumber",
						    "nativeMethod" 
				     },
				     new String[] { "Name of the class",
						    "Name of the method",
						    "Name of the source code file",
						    "Line number",
						    "True if this is a native method" 
				     },
				     new OpenType[] {
				       SimpleType.STRING, SimpleType.STRING,
				       SimpleType.STRING, SimpleType.INTEGER,
				       SimpleType.BOOLEAN 
				     });
	}
      catch (OpenDataException e)
	{
	  throw new IllegalStateException("Something went wrong in creating " +
					  "the composite data type for the " +
					  "stack trace element.", e);
	}
    return seType;
  }

  /**
   * <p>
   * Returns a {@link ThreadInfo} instance using the values
   * given in the supplied
   * {@link javax.management.openmbean.CompositeData} object.
   * The composite data instance should contain the following
   * attributes with the specified types:
   * </p>
   * <table>
   * <th><td>Name</td><td>Type</td></th>
   * <tr><td>threadId</td><td>java.lang.Long</td></tr>
   * <tr><td>threadName</td><td>java.lang.String</td></tr>
   * <tr><td>threadState</td><td>java.lang.String</td></tr>
   * <tr><td>suspended</td><td>java.lang.Boolean</td></tr>
   * <tr><td>inNative</td><td>java.lang.Boolean</td></tr>
   * <tr><td>blockedCount</td><td>java.lang.Long</td></tr>
   * <tr><td>blockedTime</td><td>java.lang.Long</td></tr>
   * <tr><td>waitedCount</td><td>java.lang.Long</td></tr>
   * <tr><td>waitedTime</td><td>java.lang.Long</td></tr>
   * <tr><td>lockName</td><td>java.lang.String</td></tr>
   * <tr><td>lockOwnerId</td><td>java.lang.Long</td></tr>
   * <tr><td>lockOwnerName</td><td>java.lang.String</td></tr>
   * <tr><td>stackTrace</td><td>javax.management.openmbean.CompositeData[]
   * </td></tr>
   * </table>
   * <p>
   * The stack trace is further described as:
   * </p>
   * <table>
   * <th><td>Name</td><td>Type</td></th>
   * <tr><td>className</td><td>java.lang.String</td></tr>
   * <tr><td>methodName</td><td>java.lang.String</td></tr>
   * <tr><td>fileName</td><td>java.lang.String</td></tr>
   * <tr><td>lineNumber</td><td>java.lang.Integer</td></tr>
   * <tr><td>nativeMethod</td><td>java.lang.Boolean</td></tr>
   * </table>
   * 
   * @param data the composite data structure to take values from.
   * @return a new instance containing the values from the 
   *         composite data structure, or <code>null</code>
   *         if the data structure was also <code>null</code>.
   * @throws IllegalArgumentException if the composite data structure
   *                                  does not match the structure
   *                                  outlined above.
   */
  public static ThreadInfo from(CompositeData data)
  {
    if (data == null)
      return null;
    CompositeType type = data.getCompositeType();
    checkAttribute(type, "ThreadId", SimpleType.LONG);
    checkAttribute(type, "ThreadName", SimpleType.STRING);
    checkAttribute(type, "ThreadState", SimpleType.STRING);
    checkAttribute(type, "Suspended", SimpleType.BOOLEAN);
    checkAttribute(type, "InNative", SimpleType.BOOLEAN);
    checkAttribute(type, "BlockedCount", SimpleType.LONG);
    checkAttribute(type, "BlockedTime", SimpleType.LONG);
    checkAttribute(type, "WaitedCount", SimpleType.LONG);
    checkAttribute(type, "WaitedTime", SimpleType.LONG);
    checkAttribute(type, "LockName", SimpleType.STRING);
    checkAttribute(type, "LockOwnerId", SimpleType.LONG);
    checkAttribute(type, "LockOwnerName", SimpleType.STRING);
    try
      {
	checkAttribute(type, "StackTrace",
		       new ArrayType(1, getStackTraceType()));
      }
    catch (OpenDataException e)
      {
	throw new IllegalStateException("Something went wrong in creating " +
					"the array for the stack trace element.",
					e);
      }
    OpenType foundType = type.getType("LockedMonitors");
    if (foundType != null)
      try
	{
	  CompositeType mType = new CompositeType(MonitorInfo.class.getName(),
						  "Information on a object monitor lock",
						  new String[] { "ClassName",
								 "IdentityHashCode",
								 "LockedStackDepth",
								 "LockedStackFrame"
						  },
						  new String[] { "Name of the class",
								 "Identity hash code " +
								 "of the class",
								 "Stack depth at time " +
								 "of lock",
								 "Stack frame at time " +
								 "of lock",
						  },
						  new OpenType[] {
						    SimpleType.STRING, SimpleType.INTEGER,
						    SimpleType.INTEGER, getStackTraceType()
						  });
	  if (!(foundType.equals(new ArrayType(1, mType))))
	    throw new IllegalArgumentException("Field LockedMonitors is not of " +
					       "type " + mType.getClassName());
	}
    catch (OpenDataException e)
      {
	throw new IllegalStateException("Something went wrong in creating " +
					"the composite data type for the " +
					"object monitor information array.", e);
      }
    foundType = type.getType("LockedSynchronizers");
    if (foundType != null)
      try
	{
	  CompositeType lType = new CompositeType(LockInfo.class.getName(),
						  "Information on a lock",
						  new String[] { "ClassName",
								 "IdentityHashCode"
						  },
						  new String[] { "Name of the class",
								 "Identity hash code " +
								 "of the class"
						  },
						  new OpenType[] {
						    SimpleType.STRING, SimpleType.INTEGER
						  });
	  if (!(foundType.equals(new ArrayType(1, lType))))
	    throw new IllegalArgumentException("Field LockedSynchronizers is not of " +
					       "type " + lType.getClassName());
	}
    catch (OpenDataException e)
      {
	throw new IllegalStateException("Something went wrong in creating " +
					"the composite data type for the " +
					"ownable synchronizerinformation array.", e);
      }
    CompositeData[] dTraces = (CompositeData[]) data.get("StackTrace");
    StackTraceElement[] traces = new StackTraceElement[dTraces.length];
    for (int a = 0; a < dTraces.length; ++a)
	/* FIXME: We can't use the boolean as there is no available
	   constructor. */
      traces[a] = 
	new StackTraceElement((String) dTraces[a].get("ClassName"),
			      (String) dTraces[a].get("MethodName"),
			      (String) dTraces[a].get("FileName"),
			      ((Integer) 
			       dTraces[a].get("LineNumber")).intValue());
    MonitorInfo[] mInfo;
    if (data.containsKey("LockedMonitors"))
      {
	CompositeData[] dmInfos = (CompositeData[]) data.get("LockedMonitors");
	mInfo = new MonitorInfo[dmInfos.length];
	for (int a = 0; a < dmInfos.length; ++a)
	  mInfo[a] = MonitorInfo.from(dmInfos[a]);
      }
    else
      mInfo = new MonitorInfo[]{};
    LockInfo[] lInfo;
    if (data.containsKey("LockedSynchronizers"))
      {
	CompositeData[] dlInfos = (CompositeData[]) data.get("LockedSynchronizers");
	lInfo = new LockInfo[dlInfos.length];
	for (int a = 0; a < dlInfos.length; ++a)
	  lInfo[a] = new LockInfo((String) dlInfos[a].get("ClassName"),
				  (Integer) dlInfos[a].get("IdentityHashCode"));
      }
    else
      lInfo = new LockInfo[]{};
    return new ThreadInfo(((Long) data.get("ThreadId")).longValue(),
			  (String) data.get("ThreadName"),
			  Thread.State.valueOf((String) data.get("ThreadState")),
			  ((Long) data.get("BlockedCount")).longValue(),
			  ((Long) data.get("BlockedTime")).longValue(),
			  (String) data.get("LockName"),
			  ((Long) data.get("LockOwnerId")).longValue(),
			  (String) data.get("LockOwnerName"),  
			  ((Long) data.get("WaitedCount")).longValue(),
			  ((Long) data.get("WaitedTime")).longValue(),
			  ((Boolean) data.get("InNative")).booleanValue(),
			  ((Boolean) data.get("Suspended")).booleanValue(),
			  traces, mInfo, lInfo);
  }

  /**
   * Returns the number of times this thread has been
   * in the {@link java.lang.Thread.State#BLOCKED} state.
   * A thread enters this state when it is waiting to
   * obtain an object's monitor.  This may occur either
   * on entering a synchronized method for the first time,
   * or on re-entering it following a call to
   * {@link java.lang.Object#wait()}.
   *
   * @return the number of times this thread has been blocked.
   */
  public long getBlockedCount()
  {
    return blockedCount;
  }

  /**
   * <p>
   * Returns the accumulated number of milliseconds this
   * thread has been in the
   * {@link java.lang.Thread.State#BLOCKED} state
   * since thread contention monitoring was last enabled.
   * A thread enters this state when it is waiting to
   * obtain an object's monitor.  This may occur either
   * on entering a synchronized method for the first time,
   * or on re-entering it following a call to
   * {@link java.lang.Object#wait()}.
   * </p>
   * <p>
   * Use of this method requires virtual machine support
   * for thread contention monitoring and for this support
   * to be enabled.
   * </p>
   * 
   * @return the accumulated time (in milliseconds) that this
   *         thread has spent in the blocked state, since
   *         thread contention monitoring was enabled, or -1
   *         if thread contention monitoring is disabled.
   * @throws UnsupportedOperationException if the virtual
   *                                       machine does not
   *                                       support contention
   *                                       monitoring.
   * @see ThreadMXBean#isThreadContentionMonitoringEnabled()
   * @see ThreadMXBean#isThreadContentionMonitoringSupported()
   */
  public long getBlockedTime()
  {
    if (bean == null)
      bean = ManagementFactory.getThreadMXBean();
    // Will throw UnsupportedOperationException for us
    if (bean.isThreadContentionMonitoringEnabled())
      return blockedTime;
    else
      return -1;
  }

  /**
   * Returns an array of {@link MonitorInfo} objects representing
   * information on the locks on object monitors held by the thread.
   * If no locks are held, or such information was not requested
   * on creating this {@link ThreadInfo} object, a zero-length
   * array will be returned.
   *
   * @return information on object monitors locked by this thread.
   */
  public MonitorInfo[] getLockedMonitors()
  {
    return lockedMonitors;
  }

  /**
   * Returns an array of {@link LockInfo} objects representing
   * information on the locks on ownable synchronizers held by the thread.
   * If no locks are held, or such information was not requested
   * on creating this {@link ThreadInfo} object, a zero-length
   * array will be returned.
   *
   * @return information on ownable synchronizers locked by this thread.
   */
  public LockInfo[] getLockedSynchronizers()
  {
    return lockedSynchronizers;
  }

  /**
   * <p>
   * Returns a {@link LockInfo} object representing the
   * lock on which this thread is blocked.  If the thread
   * is not blocked, this method returns <code>null</code>.
   * </p>
   * <p>
   * The thread may be blocked due to one of three reasons:
   * </p>
   * <ol>
   * <li>The thread is in the <code>BLOCKED</code> state
   * waiting to acquire an object monitor in order to enter
   * a synchronized method or block.</li>
   * <li>The thread is in the <code>WAITING</code> or
   * <code>TIMED_WAITING</code> state due to a call to
   * {@link java.lang.Object#wait()}.</li>
   * <li>The thread is in the <code>WAITING</code> or
   * <code>TIMED_WAITING</code> state due to a call
   * to {@link java.util.concurrent.locks.LockSupport#park()}.
   * The lock is the return value of
   * {@link java.util.concurrent.locks.LockSupport#getBlocker()}.</li>
   * </ol>
   * 
   * @return a {@link LockInfo} object representing the lock on
   *         which the thread is blocked, or <code>null</code> if
   *         the thread isn't blocked.
   * @since 1.6
   * @see #getLockName()
   */
  public LockInfo getLockInfo()
  {
    String lockName = getLockName();
    int at = lockName.indexOf('@');
    return new LockInfo(lockName.substring(0, at),
			Integer.decode(lockName.substring(at + 1)));
  }

  /**
   * <p>
   * Returns a {@link java.lang.String} representation of
   * the lock on which this thread is blocked.  If
   * the thread is not blocked, this method returns
   * <code>null</code>.
   * </p>
   * <p>
   * The returned {@link java.lang.String} is constructed
   * using the class name and identity hashcode (usually
   * the memory address of the object) of the lock.  The
   * two are separated by the '@' character, and the identity
   * hashcode is represented in hexadecimal.  Thus, for a
   * lock, <code>l</code>, the returned value is
   * the result of concatenating
   * <code>l.getClass().getName()</code>, <code>"@"</code>
   * and
   * <code>Integer.toHexString(System.identityHashCode(l))</code>.
   * The value is only unique to the extent that the identity
   * hash code is also unique.  The value is the same as would
   * be returned by <code>getLockInfo().toString()</code>
   * </p>
   *
   * @return a string representing the lock on which this
   *         thread is blocked, or <code>null</code> if
   *         the thread is not blocked.
   */
  public String getLockName()
  {
    if (!isThreadBlocked())
      return null;
    return lockName;
  }

  /**
   * Returns the identifier of the thread which owns the
   * monitor lock this thread is waiting for.  -1 is returned
   * if either this thread is not blocked, or the lock is
   * not held by any other thread.
   * 
   * @return the thread identifier of thread holding the lock
   *         this thread is waiting for, or -1 if the thread
   *         is not blocked or the lock is not held by another
   *         thread.
   */
  public long getLockOwnerId()
  {
    if (!isThreadBlocked())
      return -1;
    return lockOwnerId;
  }

  /**
   * Returns the name of the thread which owns the
   * monitor lock this thread is waiting for.  <code>null</code>
   * is returned if either this thread is not blocked,
   * or the lock is not held by any other thread.
   * 
   * @return the thread identifier of thread holding the lock
   *         this thread is waiting for, or <code>null</code>
   *         if the thread is not blocked or the lock is not
   *         held by another thread.
   */
  public String getLockOwnerName()
  {
    if (!isThreadBlocked())
      return null;
    return lockOwnerName;
  }

  /**
   * <p>
   * Returns the stack trace of this thread to the depth
   * specified on creation of this {@link ThreadInfo}
   * object.  If the depth is zero, an empty array will
   * be returned.  For non-zero arrays, the elements
   * start with the most recent trace at position zero.
   * The bottom of the stack represents the oldest method
   * invocation which meets the depth requirements.
   * </p>
   * <p>
   * Some virtual machines may not be able to return
   * stack trace information for a thread.  In these
   * cases, an empty array will also be returned.
   * </p>
   * 
   * @return an array of {@link java.lang.StackTraceElement}s
   *         representing the trace of this thread.
   */
  public StackTraceElement[] getStackTrace()
  {
    return trace;
  }

  /**
   * Returns the identifier of the thread associated with
   * this instance of {@link ThreadInfo}.
   *
   * @return the thread's identifier.
   */
  public long getThreadId()
  {
    return threadId;
  }

  /**
   * Returns the name of the thread associated with
   * this instance of {@link ThreadInfo}.
   *
   * @return the thread's name.
   */
  public String getThreadName()
  {
    return threadName;
  }

  /**
   * Returns the state of the thread associated with
   * this instance of {@link ThreadInfo}.
   *
   * @return the thread's state.
   */
  public Thread.State getThreadState()
  {
    return threadState;
  }
    
  /**
   * Returns the number of times this thread has been
   * in the {@link java.lang.Thread.State#WAITING} 
   * or {@link java.lang.Thread.State#TIMED_WAITING} state.
   * A thread enters one of these states when it is waiting
   * due to a call to {@link java.lang.Object.wait()},
   * {@link java.lang.Object.join()} or
   * {@link java.lang.concurrent.locks.LockSupport.park()},
   * either with an infinite or timed delay, respectively. 
   *
   * @return the number of times this thread has been waiting.
   */
  public long getWaitedCount()
  {
    return waitedCount;
  }

  /**
   * <p>
   * Returns the accumulated number of milliseconds this
   * thread has been in the
   * {@link java.lang.Thread.State#WAITING} or
   * {@link java.lang.Thread.State#TIMED_WAITING} state,
   * since thread contention monitoring was last enabled.
   * A thread enters one of these states when it is waiting
   * due to a call to {@link java.lang.Object.wait()},
   * {@link java.lang.Object.join()} or
   * {@link java.lang.concurrent.locks.LockSupport.park()},
   * either with an infinite or timed delay, respectively. 
   * </p>
   * <p>
   * Use of this method requires virtual machine support
   * for thread contention monitoring and for this support
   * to be enabled.
   * </p>
   * 
   * @return the accumulated time (in milliseconds) that this
   *         thread has spent in one of the waiting states, since
   *         thread contention monitoring was enabled, or -1
   *         if thread contention monitoring is disabled.
   * @throws UnsupportedOperationException if the virtual
   *                                       machine does not
   *                                       support contention
   *                                       monitoring.
   * @see ThreadMXBean#isThreadContentionMonitoringEnabled()
   * @see ThreadMXBean#isThreadContentionMonitoringSupported()
   */
  public long getWaitedTime()
  {
    if (bean == null)
      bean = ManagementFactory.getThreadMXBean();
    // Will throw UnsupportedOperationException for us
    if (bean.isThreadContentionMonitoringEnabled())
      return waitedTime;
    else
      return -1;
  }

  /**
   * Returns true if the thread is in a native method.  This
   * excludes native code which forms part of the virtual
   * machine itself, or which results from Just-In-Time
   * compilation.
   *
   * @return true if the thread is in a native method, false
   *         otherwise.
   */
  public boolean isInNative()
  {
    return isInNative;
  }

  /**
   * Returns true if the thread has been suspended using
   * {@link java.lang.Thread#suspend()}.
   *
   * @return true if the thread is suspended, false otherwise.
   */
  public boolean isSuspended()
  {
    return isSuspended;
  }

  /**
   * Returns a {@link java.lang.String} representation of
   * this {@link ThreadInfo} object.  This takes the form
   * <code>java.lang.management.ThreadInfo[id=tid, name=n,
   * state=s, blockedCount=bc, waitedCount=wc, isInNative=iin,
   * isSuspended=is]</code>, where <code>tid</code> is
   * the thread identifier, <code>n</code> is the
   * thread name, <code>s</code> is the thread state,
   * <code>bc</code> is the blocked state count,
   * <code>wc</code> is the waiting state count and
   * <code>iin</code> and <code>is</code> are boolean
   * flags to indicate the thread is in native code or
   * suspended respectively.  If the thread is blocked,
   * <code>lock=l, lockOwner=lo</code> is also included,
   * where <code>l</code> is the lock waited for, and
   * <code>lo</code> is the thread which owns the lock
   * (or null if there is no owner).
   *
   * @return the string specified above.
   */
  public String toString()
  {
    return getClass().getName() +
      "[id=" + threadId + 
      ", name=" + threadName +
      ", state=" + threadState +
      ", blockedCount=" + blockedCount +
      ", waitedCount=" + waitedCount +
      ", isInNative=" + isInNative + 
      ", isSuspended=" + isSuspended +
      (isThreadBlocked() ? 
       ", lockOwnerId=" + lockOwnerId +
       ", lockOwnerName=" + lockOwnerName : "") +
      ", lockedMonitors=" + Arrays.toString(lockedMonitors) +
      ", lockedSynchronizers=" + Arrays.toString(lockedSynchronizers) +
      "]";
  }

  /**
   * <p>
   * Returns true if the thread is in a blocked state.
   * The thread is regarded as blocked if:
   * </p>
   * <ol>
   * <li>The thread is in the <code>BLOCKED</code> state
   * waiting to acquire an object monitor in order to enter
   * a synchronized method or block.</li>
   * <li>The thread is in the <code>WAITING</code> or
   * <code>TIMED_WAITING</code> state due to a call to
   * {@link java.lang.Object#wait()}.</li>
   * <li>The thread is in the <code>WAITING</code> or
   * <code>TIMED_WAITING</code> state due to a call
   * to {@link java.util.concurrent.locks.LockSupport#park()}.
   * The lock is the return value of
   * {@link java.util.concurrent.locks.LockSupport#getBlocker()}.</li>
   * </ol>
   *
   * @return true if the thread is blocked.
   */
  private boolean isThreadBlocked()
  {
    return (threadState == Thread.State.BLOCKED ||
	    threadState == Thread.State.WAITING ||
	    threadState == Thread.State.TIMED_WAITING);
  }
  
}
