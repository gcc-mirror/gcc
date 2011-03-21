/* java.lang.VMProcess -- VM implementation of java.lang.Process
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

package java.lang;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Represents one external process. Each instance of this class is in
 * one of three states: INITIAL, RUNNING, or TERMINATED. The instance
 * is {@link Object#notifyAll notifyAll()}'d each time the state changes.
 * The state of all instances is managed by a single dedicated thread
 * which does the actual fork()/exec() and wait() system calls. User
 * threads {@link Object#wait()} on the instance when creating the
 * process or waiting for it to terminate.
 *
 * <p>
 * See
 * <a href="http://gcc.gnu.org/bugzilla/show_bug.cgi?id=11801">GCC bug
 * #11801</a> for the motivation behind the design of this class.
 *
 * @author Archie Cobbs
 * @see Process
 * @see Runtime#exec(String)
 */
final class VMProcess extends Process
{

  // Possible states for a VMProcess
  private static final int INITIAL = 0;
  private static final int RUNNING = 1;
  private static final int TERMINATED = 2;

  // Dedicated thread that does all the fork()'ing and wait()'ing.
  static Thread processThread;

  // New processes waiting to be spawned by processThread.
  static final LinkedList workList = new LinkedList();

  // Return values set by nativeReap() when a child is reaped.
  // These are only accessed by processThread so no locking required.
  static long reapedPid;
  static int reapedExitValue;

  // Information about this process
  int state;                                   // current state of process
  final String[] cmd;                          // copied from Runtime.exec()
  final String[] env;                          // copied from Runtime.exec()
  final File dir;                              // copied from Runtime.exec()
  Throwable exception;                         // if process failed to start
  long pid;                                    // process id
  OutputStream stdin;                          // process input stream
  InputStream stdout;                          // process output stream
  InputStream stderr;                          // process error stream
  int exitValue;                               // process exit value
  boolean redirect;                            // redirect stderr -> stdout

  //
  // Dedicated thread that does all the fork()'ing and wait()'ing
  // for external processes. This is needed because some systems like
  // Linux use a process-per-thread model, which means the same thread
  // that did the fork()/exec() must also do the wait().
  //
  private static class ProcessThread extends Thread
  {

    // Max time (in ms) we'll delay before trying to reap another child.
    private static final int MAX_REAP_DELAY = 1000;

    // Processes created but not yet terminated; maps Long(pid) -> VMProcess
    // Only used in run() and spawn() method from this Thread, so no locking.
    private final HashMap activeMap = new HashMap();

    // We have an explicit constructor, because the default
    // constructor will be private, which means the compiler will have
    // to generate a second package-private constructor, which is
    // bogus.
    ProcessThread ()
    {
    }

    public void run()
    {
      final LinkedList workList = VMProcess.workList;
      while (true)
        {

          // Get the next process to spawn (if any) and spawn it. Spawn
          // at most one at a time before checking for reapable children.
          VMProcess process = null;
          synchronized (workList)
            {
              if (!workList.isEmpty())
                process = (VMProcess)workList.removeFirst();
            }

          if (process != null)
            spawn(process);


          // Check for termination of active child processes
          while (!activeMap.isEmpty() && VMProcess.nativeReap())
            {
              long pid = VMProcess.reapedPid;
              int exitValue = VMProcess.reapedExitValue;
              process = (VMProcess)activeMap.remove(new Long(pid));
              if (process != null)
                {
                  synchronized (process)
                    {
                      process.exitValue = exitValue;
                      process.state = TERMINATED;
                      process.notify();
                    }
                }
              else
                System.err.println("VMProcess WARNING reaped unknown process: "
                                   + pid);
            }


          // If there are more new processes to create, go do that now.
          // If there is nothing left to do, exit this thread. Otherwise,
          // sleep a little while, and then check again for reapable children.
          // We will get woken up immediately if there are new processes to
          // spawn, but not if there are new children to reap. So we only
          // sleep a short time, in effect polling while processes are active.
          synchronized (workList)
            {
              if (!workList.isEmpty())
                continue;
              if (activeMap.isEmpty())
                {
                  processThread = null;
                  break;
                }

              try
                {
                  workList.wait(MAX_REAP_DELAY);
                }
              catch (InterruptedException e)
                {
                  /* ignore */
                }
            }
        }
    }

    // Spawn a process
    private void spawn(VMProcess process)
    {

      // Spawn the process and put it in our active map indexed by pid.
      // If the spawn operation fails, store the exception with the process.
      // In either case, wake up thread that created the process.
      synchronized (process)
        {
          try
            {
              process.nativeSpawn(process.cmd, process.env, process.dir,
                                  process.redirect);
              process.state = RUNNING;
              activeMap.put(new Long(process.pid), process);
            }
          catch (ThreadDeath death)
            {
              throw death;
            }
          catch (Throwable t)
            {
              process.state = TERMINATED;
              process.exception = t;
            }
          process.notify();
        }
    }
  }

  // Constructor
  private VMProcess(String[] cmd, String[] env, File dir, boolean redirect)
    throws IOException
  {

    // Initialize this process
    this.state = INITIAL;
    this.cmd = cmd;
    this.env = env;
    this.dir = dir;
    this.redirect = redirect;

    // Add process to the new process work list and wakeup processThread
    synchronized (workList)
      {
        workList.add(this);
        if (processThread == null)
          {
            processThread = new ProcessThread();
            processThread.setDaemon(true);
            processThread.start();
          }
        else
          {
            workList.notify();
          }
      }

    // Wait for processThread to spawn this process and update its state
    synchronized (this)
      {
        while (state == INITIAL)
          {
            try
              {
                wait();
              }
            catch (InterruptedException e)
              {
                /* ignore */
              }
          }
      }

    // If spawning failed, rethrow the exception in this thread
    if (exception != null)
      {
        exception.fillInStackTrace();
        if (exception instanceof IOException)
          throw (IOException)exception;

        if (exception instanceof Error)
          throw (Error)exception;

        if (exception instanceof RuntimeException)
          throw (RuntimeException)exception;

        throw new RuntimeException(exception);
      }
  }

  // Invoked by native code (from nativeSpawn()) to record process info.
  private void setProcessInfo(OutputStream stdin,
                              InputStream stdout, InputStream stderr, long pid)
  {
    this.stdin = stdin;
    this.stdout = stdout;
    if (stderr == null)
      this.stderr = new InputStream()
        {
          public int read() throws IOException
          {
            return -1;
          }
        };
    else
      this.stderr = stderr;
    this.pid = pid;
  }

  /**
   * Entry point from Runtime.exec().
   */
  static Process exec(String[] cmd, String[] env, File dir) throws IOException
  {
    return new VMProcess(cmd, env, dir, false);
  }

  static Process exec(List cmd, Map env,
                      File dir, boolean redirect) throws IOException
  {
    String[] acmd = (String[]) cmd.toArray(new String[cmd.size()]);
    String[] aenv = new String[env.size()];

    int i = 0;
    Iterator iter = env.entrySet().iterator();
    while (iter.hasNext())
      {
        Map.Entry entry = (Map.Entry) iter.next();
        aenv[i++] = entry.getKey() + "=" + entry.getValue();
      }

    return new VMProcess(acmd, aenv, dir, redirect);
  }

  public OutputStream getOutputStream()
  {
    return stdin;
  }

  public InputStream getInputStream()
  {
    return stdout;
  }

  public InputStream getErrorStream()
  {
    return stderr;
  }

  public synchronized int waitFor() throws InterruptedException
  {
    while (state != TERMINATED)
      wait();
    return exitValue;
  }

  public synchronized int exitValue()
  {
    if (state != TERMINATED)
      throw new IllegalThreadStateException();
    return exitValue;
  }

  public synchronized void destroy()
  {
    if (state == TERMINATED)
      return;

    nativeKill(pid);

    while (state != TERMINATED)
      {
        try
          {
            wait();
          }
        catch (InterruptedException e)
          {
            /* ignore */
          }
      }
  }

  /**
   * Does the fork()/exec() thing to create the O/S process.
   * Must invoke setProcessInfo() before returning successfully.
   * This method is only invoked by processThread.
   *
   * @throws IOException if the O/S process could not be created.
   */
  native void nativeSpawn(String[] cmd, String[] env, File dir,
                          boolean redirect)
    throws IOException;

  /**
   * Test for a reapable child process, and reap if so. Does not block.
   * If a child was reaped, this method must set reapedPid and
   * reapedExitValue appropriately before returning.
   * This method is only invoked by processThread.
   *
   * @return true if a child was reaped, otherwise false
   */
  // This is not private as it is called from an inner class.
  static native boolean nativeReap();

  /**
   * Kill a process. This sends it a fatal signal but does not reap it.
   */
  private static native void nativeKill(long pid);
}
