// PosixProcess.java - Subclass of Process for POSIX systems.
/* Copyright (C) 1998, 1999, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;


/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date May 3, 1999
 * @author David Daney <ddaney@avtrex.com> Rewrote using
 * ProcessManager
 */

// This is entirely internal to our implementation.
// This file is copied to `ConcreteProcess.java' before compilation.
// Hence the class name apparently does not match the file name.
final class ConcreteProcess extends Process
{
  static class ProcessManager extends Thread
  {
    /**
     * A list of {@link ConcreteProcess ConcreteProcesses} to be
     * started.  The queueLock object is used as the lock Object
     * for all process related operations. To avoid dead lock
     * ensure queueLock is obtained before ConcreteProcess.
     */
    List queue = new LinkedList();
    private Map pidToProcess = new HashMap();
    private boolean ready = false;
    private long reaperPID;

    ProcessManager()
    {
      super("ProcessManager");
      // Don't keep the (main) process from exiting on our account.
      this.setDaemon(true);
    }

    /**
     * Get the ConcreteProcess object with the given pid and
     * remove it from the map.  This method is called from the
     * native code for {@link #reap()).  The mapping is removed so
     * the ConcreteProcesses can be GCed after they terminate.
     *
     * @param p The pid of the process.
     */
    private ConcreteProcess removeProcessFromMap(long p)
    {
      return (ConcreteProcess) pidToProcess.remove(new Long(p));
    }

    /**
     * Put the given ConcreteProcess in the map using the Long
     * value of its pid as the key.
     *
     * @param p The ConcreteProcess.
     */
    void addProcessToMap(ConcreteProcess p)
    {
      pidToProcess.put(new Long(p.pid), p);
    }

    /**
     * Queue up the ConcreteProcess and awake the ProcessManager.
     * The ProcessManager will start the ConcreteProcess from its
     * thread so it can be reaped when it terminates.
     *
     * @param p The ConcreteProcess.
     */
    void startExecuting(ConcreteProcess p)
    {
      synchronized (queueLock)
        {
	  queue.add(p);
	  signalReaper(); // If blocked in waitForSignal().
	  queueLock.notifyAll(); // If blocked in wait();
        }
    }

    /**
     * Block until the ProcessManager thread is ready to accept
     * commands.
     */
    void waitUntilReady()
    {
      synchronized (this)
        {
	  try
	    {
	      while (! ready)
		wait();
	    }
	  catch (InterruptedException ie)
	    {
	      // Ignore.
	    }
        }
    }

    /**
     * Main Process starting/reaping loop.
     */
    public void run()
    {
      init();
      // Now ready to accept requests.
      synchronized (this)
        {
	  ready = true;
	  this.notifyAll();
        }

      for (;;)
        {
	  try
	    {
	      synchronized (queueLock)
	        {
		  boolean haveMoreChildren = reap();
		  if (! haveMoreChildren && queue.size() == 0)
		    {
		      // This reaper thread could exit, but we
		      // keep it alive for a while in case
		      // someone wants to start more Processes.
		      try
		        {
			  queueLock.wait(1000L);
			  if (queue.size() == 0)
			    {
			      processManager = null;
			      return; // Timed out.
			    }
		        }
		      catch (InterruptedException ie)
		        {
			  // Ignore and exit the thread.
			  return;
		        }
		    }
		  while (queue.size() > 0)
		    {
		      ConcreteProcess p = (ConcreteProcess) queue.remove(0);
		      p.spawn(this);
		    }
	        }

	      // Wait for a SIGCHLD from either an exiting
	      // process or the startExecuting() method.  This
	      // is done outside of the synchronized block to
	      // allow other threads to enter and submit more
	      // jobs.
	      waitForSignal();
	    }
	  catch (Exception ex)
	    {
	      ex.printStackTrace(System.err);
	    }
        }
    }

    /**
     * Setup native signal handlers and other housekeeping things.
     *
     */
    private native void init();

    /**
     * Block waiting for SIGCHLD.
     *
     */
    private native void waitForSignal();

    /**
     * Try to reap as many children as possible without blocking.
     *
     * @return true if more live children exist.
     *
     */
    private native boolean reap();

    /**
     * Send SIGCHLD to the reaper thread.
     */
    private native void signalReaper();
  }

  public void destroy()
  {
    // Synchronized on the queueLock.  This ensures that the reaper
    // thread cannot be doing a wait() on the child.
    // Otherwise there would be a race where the OS could
    // create a process with the same pid between the wait()
    // and the update of the state which would cause a kill to
    // the wrong process.
    synchronized (queueLock)
      {
	synchronized (this)
	  {
	    // If there is no ProcessManager we cannot kill.
	    if (state != STATE_TERMINATED)
	      {
		if (processManager == null)
		  throw new InternalError();
		nativeDestroy();
	      }
	  }
      }
  }

  private native void nativeDestroy();

  public int exitValue()
  {
    synchronized (this)
      {
	if (state != STATE_TERMINATED)
	  throw new IllegalThreadStateException("Process has not exited");
      }
    return status;
  }

  /**
   * Called by native code when process exits.
   *
   * Already synchronized (this).  Close any streams that we can to
   * conserve file descriptors.
   *
   * The outputStream can be closed as any future writes will
   * generate an IOException due to EPIPE.
   *
   * The inputStream and errorStream can only be closed if the user
   * has not obtained a reference to them AND they have no bytes
   * available.  Since the process has terminated they will never have
   * any more data available and can safely be replaced by
   * EOFInputStreams.
   */
  void processTerminationCleanup()
  {
    try
      {
        outputStream.close();
      }
    catch (IOException ioe)
      {
        // Ignore.
      }
    try
      {
        if (returnedErrorStream == null && errorStream.available() == 0)
          {
            errorStream.close();
            errorStream = null;
          }
      }
    catch (IOException ioe)
      {
        // Ignore.
      }
    try
      {
        if (returnedInputStream == null && inputStream.available() == 0)
          {
            inputStream.close();
            inputStream = null;
          }
      }
    catch (IOException ioe)
      {
        // Ignore.
      }
  }

  public synchronized InputStream getErrorStream()
  {
    if (returnedErrorStream != null)
      return returnedErrorStream;

    if (errorStream == null)
      returnedErrorStream = EOFInputStream.instance;
    else
      returnedErrorStream = errorStream;
            
    return returnedErrorStream;
  }

  public synchronized InputStream getInputStream()
  {
    if (returnedInputStream != null)
      return returnedInputStream;

    if (inputStream == null)
      returnedInputStream = EOFInputStream.instance;
    else
      returnedInputStream = inputStream;
            
    return returnedInputStream;
  }

  public OutputStream getOutputStream()
  {
    return outputStream;
  }

  public int waitFor() throws InterruptedException
  {
    synchronized (this)
      {
	while (state != STATE_TERMINATED)
	  wait();
      }
    return status;
  }

  /**
   * Start this process running.  This should only be called by the
   * ProcessManager.
   *
   * @param pm The ProcessManager that made the call.
   */
  void spawn(ProcessManager pm)
  {
    synchronized (this)
      {
	// Do the fork/exec magic.
	nativeSpawn();
	// There is no race with reap() in the pidToProcess map
	// because this is always called from the same thread
	// doing the reaping.
	pm.addProcessToMap(this);
	state = STATE_RUNNING;
	// Notify anybody waiting on state change.
	this.notifyAll();
      }
  }

  /**
   * Do the fork and exec.
   */
  private native void nativeSpawn();

  // This file is copied to `ConcreteProcess.java' before
  // compilation.  Hence the constructor name apparently does not
  // match the file name.
  ConcreteProcess(String[] progarray, String[] envp, File dir)
           throws IOException
  {
    // Check to ensure there is something to run, and avoid
    // dereferencing null pointers in native code.
    if (progarray[0] == null)
      throw new NullPointerException();

    this.progarray = progarray;
    this.envp = envp;
    this.dir = dir;

    // Start a ProcessManager if there is not one already running.
    synchronized (queueLock)
      {
	if (processManager == null)
	  {
	    processManager = new ProcessManager();
	    processManager.start();
	    processManager.waitUntilReady();
	  }

	// Queue this ConcreteProcess for starting by the ProcessManager.
	processManager.startExecuting(this);
      }

    // Wait until ProcessManager has started us.
    synchronized (this)
      {
	while (state == STATE_WAITING_TO_START)
	  {
	    try
	      {
		wait();
	      }
	    catch (InterruptedException ie)
	      {
		// FIXME: What to do when interrupted while blocking in a constructor?
		// Ignore.
	      }
	  }
      }

    // If there was a problem, re-throw it.
    if (exception != null)
      {
	if (exception instanceof IOException)
	  {
	    IOException ioe = new IOException(exception.toString());
	    ioe.initCause(exception);
	    throw ioe;
	  }

	// Not an IOException.  Something bad happened.
	InternalError ie = new InternalError(exception.toString());
	ie.initCause(exception);
	throw ie;
      }

    // If we get here, all is well, the Process has started.
  }

  private String[] progarray;
  private String[] envp;
  private File dir;

  /** Set by the ProcessManager on problems starting. */
  private Throwable exception;

  /** The process id.  This is cast to a pid_t on the native side. */
  private long pid;

  // FIXME: Why doesn't the friend declaration in ConcreteProcess.h
  // allow ConcreteProcess$ProcessManager native code access these
  // when they are private?

  /** Before the process is forked. */
  static final int STATE_WAITING_TO_START = 0;

  /** After the fork. */
  static final int STATE_RUNNING = 1;

  /** After exit code has been collected. */
  static final int STATE_TERMINATED = 2;

  /** One of STATE_WAITING_TO_START, STATE_RUNNING, STATE_TERMINATED. */
  int state;

  /** The exit status, if the child has exited. */
  int status;

  /** The streams. */
  private InputStream errorStream;
  private InputStream inputStream;
  private OutputStream outputStream;

  /** InputStreams obtained by the user.  Not null indicates that the
   *  user has obtained the stream.
   */
  private InputStream returnedErrorStream;
  private InputStream returnedInputStream;

  /**
   * Lock Object for all processManager related locking.
   */
  private static Object queueLock = new Object();
  private static ProcessManager processManager;

  static class EOFInputStream extends InputStream
  {
    static EOFInputStream instance = new EOFInputStream();
    public int read()
    {
      return -1;
    }
  }
}
