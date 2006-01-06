/* Runtime.java -- access to the VM process
   Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006 Free Software Foundation

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

import gnu.classpath.SystemProperties;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;

/**
 * Runtime represents the Virtual Machine.
 *
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Jeroen Frijters
 */
// No idea why this class isn't final, since you can't build a subclass!
public class Runtime
{
  /**
   * The library path, to search when loading libraries. We can also safely use
   * this as a lock for synchronization.
   */
  private final String[] libpath;

  static
  {
    init();
  }

  /**
   * The thread that started the exit sequence. Access to this field must
   * be thread-safe; lock on libpath to avoid deadlock with user code.
   * <code>runFinalization()</code> may want to look at this to see if ALL
   * finalizers should be run, because the virtual machine is about to halt.
   */
  private Thread exitSequence;

  /**
   * All shutdown hooks. This is initialized lazily, and set to null once all
   * shutdown hooks have run. Access to this field must be thread-safe; lock
   * on libpath to avoid deadlock with user code.
   */
  private Set shutdownHooks;

  /** True if we should finalize on exit.  */
  private boolean finalizeOnExit;

  /**
   * The one and only runtime instance.
   */
  private static final Runtime current = new Runtime();

  /**
   * Not instantiable by a user, this should only create one instance.
   */
  private Runtime()
  {
    if (current != null)
      throw new InternalError("Attempt to recreate Runtime");
    
    // We don't use libpath in the libgcj implementation.  We still
    // set it to something to allow the various synchronizations to
    // work.
    libpath = new String[0];
  }

  /**
   * Get the current Runtime object for this JVM. This is necessary to access
   * the many instance methods of this class.
   *
   * @return the current Runtime object
   */
  public static Runtime getRuntime()
  {
    return current;
  }

  /**
   * Exit the Java runtime. This method will either throw a SecurityException
   * or it will never return. The status code is returned to the system; often
   * a non-zero status code indicates an abnormal exit. Of course, there is a
   * security check, <code>checkExit(status)</code>.
   *
   * <p>First, all shutdown hooks are run, in unspecified order, and
   * concurrently. Next, if finalization on exit has been enabled, all pending
   * finalizers are run. Finally, the system calls <code>halt</code>.</p>
   *
   * <p>If this is run a second time after shutdown has already started, there
   * are two actions. If shutdown hooks are still executing, it blocks
   * indefinitely. Otherwise, if the status is nonzero it halts immediately;
   * if it is zero, it blocks indefinitely. This is typically called by
   * <code>System.exit</code>.</p>
   *
   * @param status the status to exit with
   * @throws SecurityException if permission is denied
   * @see #addShutdownHook(Thread)
   * @see #runFinalizersOnExit(boolean)
   * @see #runFinalization()
   * @see #halt(int)
   */
  public void exit(int status)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkExit(status);

    if (runShutdownHooks())
      halt(status);

    // Someone else already called runShutdownHooks().
    // Make sure we are not/no longer in the shutdownHooks set.
    // And wait till the thread that is calling runShutdownHooks() finishes.
    synchronized (libpath)
      {
        if (shutdownHooks != null)
          {
            shutdownHooks.remove(Thread.currentThread());
            // Interrupt the exit sequence thread, in case it was waiting
            // inside a join on our thread.
            exitSequence.interrupt();
            // Shutdown hooks are still running, so we clear status to
	    // make sure we don't halt.
	    status = 0;
          }
      }

    // If exit() is called again after the shutdown hooks have run, but
    // while finalization for exit is going on and the status is non-zero
    // we halt immediately.
    if (status != 0)
      halt(status);

    while (true)
      try
        {
          exitSequence.join();
        }
      catch (InterruptedException e)
        {
          // Ignore, we've suspended indefinitely to let all shutdown
          // hooks complete, and to let any non-zero exits through, because
          // this is a duplicate call to exit(0).
        }
  }

  /**
   * On first invocation, run all the shutdown hooks and return true.
   * Any subsequent invocations will simply return false.
   * Note that it is package accessible so that VMRuntime can call it
   * when VM exit is not triggered by a call to Runtime.exit().
   * 
   * @return was the current thread the first one to call this method?
   */
  boolean runShutdownHooks()
  {
    boolean first = false;
    synchronized (libpath) // Synch on libpath, not this, to avoid deadlock.
      {
        if (exitSequence == null)
          {
            first = true;
            exitSequence = Thread.currentThread();
            if (shutdownHooks != null)
              {
                Iterator i = shutdownHooks.iterator();
                while (i.hasNext()) // Start all shutdown hooks.
                  try
                    {
                      ((Thread) i.next()).start();
                    }
                  catch (IllegalThreadStateException e)
                    {
                      i.remove();
                    }
              }
          }
      }
    if (first)
      {
        if (shutdownHooks != null)
          {
            // Check progress of all shutdown hooks. As a hook completes,
            // remove it from the set. If a hook calls exit, it removes
            // itself from the set, then waits indefinitely on the
            // exitSequence thread. Once the set is empty, set it to null to
            // signal all finalizer threads that halt may be called.
            while (true)
              {
                Thread[] hooks;
                synchronized (libpath)
                  {
                    hooks = new Thread[shutdownHooks.size()];
                    shutdownHooks.toArray(hooks);
                  }
                if (hooks.length == 0)
                  break;
                for (int i = 0; i < hooks.length; i++)
                  {
                    try
                      {
                        synchronized (libpath)
                          {
                            if (!shutdownHooks.contains(hooks[i]))
                              continue;
                          }
                        hooks[i].join();
                        synchronized (libpath)
                          {
                            shutdownHooks.remove(hooks[i]);
                          }
                      }
                    catch (InterruptedException x)
                      {
                        // continue waiting on the next thread
                      }
                  }
              }
            synchronized (libpath)
              {
                shutdownHooks = null;
              }
          }
	// Run finalization on all finalizable objects (even if they are
	// still reachable).
        runFinalizationForExit();
      }
    return first;
  }

  /**
   * Register a new shutdown hook. This is invoked when the program exits
   * normally (because all non-daemon threads ended, or because
   * <code>System.exit</code> was invoked), or when the user terminates
   * the virtual machine (such as by typing ^C, or logging off). There is
   * a security check to add hooks,
   * <code>RuntimePermission("shutdownHooks")</code>.
   *
   * <p>The hook must be an initialized, but unstarted Thread. The threads
   * are run concurrently, and started in an arbitrary order; and user
   * threads or daemons may still be running. Once shutdown hooks have
   * started, they must all complete, or else you must use <code>halt</code>,
   * to actually finish the shutdown sequence. Attempts to modify hooks
   * after shutdown has started result in IllegalStateExceptions.</p>
   *
   * <p>It is imperative that you code shutdown hooks defensively, as you
   * do not want to deadlock, and have no idea what other hooks will be
   * running concurrently. It is also a good idea to finish quickly, as the
   * virtual machine really wants to shut down!</p>
   *
   * <p>There are no guarantees that such hooks will run, as there are ways
   * to forcibly kill a process. But in such a drastic case, shutdown hooks
   * would do little for you in the first place.</p>
   *
   * @param hook an initialized, unstarted Thread
   * @throws IllegalArgumentException if the hook is already registered or run
   * @throws IllegalStateException if the virtual machine is already in
   *         the shutdown sequence
   * @throws SecurityException if permission is denied
   * @since 1.3
   * @see #removeShutdownHook(Thread)
   * @see #exit(int)
   * @see #halt(int)
   */
  public void addShutdownHook(Thread hook)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkPermission(new RuntimePermission("shutdownHooks"));
    if (hook.isAlive() || hook.getThreadGroup() == null)
      throw new IllegalArgumentException("The hook thread " + hook + " must not have been already run or started");
    synchronized (libpath)
      {
        if (exitSequence != null)
          throw new IllegalStateException("The Virtual Machine is exiting. It is not possible anymore to add any hooks");
        if (shutdownHooks == null)
          shutdownHooks = new HashSet(); // Lazy initialization.
        if (! shutdownHooks.add(hook))
          throw new IllegalArgumentException(hook.toString() + " had already been inserted");
      }
  }

  /**
   * De-register a shutdown hook. As when you registered it, there is a
   * security check to remove hooks,
   * <code>RuntimePermission("shutdownHooks")</code>.
   *
   * @param hook the hook to remove
   * @return true if the hook was successfully removed, false if it was not
   *         registered in the first place
   * @throws IllegalStateException if the virtual machine is already in
   *         the shutdown sequence
   * @throws SecurityException if permission is denied
   * @since 1.3
   * @see #addShutdownHook(Thread)
   * @see #exit(int)
   * @see #halt(int)
   */
  public boolean removeShutdownHook(Thread hook)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkPermission(new RuntimePermission("shutdownHooks"));
    synchronized (libpath)
      {
        if (exitSequence != null)
          throw new IllegalStateException();
        if (shutdownHooks != null)
          return shutdownHooks.remove(hook);
      }
    return false;
  }

  /**
   * Forcibly terminate the virtual machine. This call never returns. It is
   * much more severe than <code>exit</code>, as it bypasses all shutdown
   * hooks and initializers. Use caution in calling this! Of course, there is
   * a security check, <code>checkExit(status)</code>.
   *
   * @param status the status to exit with
   * @throws SecurityException if permission is denied
   * @since 1.3
   * @see #exit(int)
   * @see #addShutdownHook(Thread)
   */
  public void halt(int status)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkExit(status);
    exitInternal(status);
  }

  /**
   * Tell the VM to run the finalize() method on every single Object before
   * it exits.  Note that the JVM may still exit abnormally and not perform
   * this, so you still don't have a guarantee. And besides that, this is
   * inherently unsafe in multi-threaded code, as it may result in deadlock
   * as multiple threads compete to manipulate objects. This value defaults to
   * <code>false</code>. There is a security check, <code>checkExit(0)</code>.
   *
   * @param finalizeOnExit whether to finalize all Objects on exit
   * @throws SecurityException if permission is denied
   * @see #exit(int)
   * @see #gc()
   * @since 1.1
   * @deprecated never rely on finalizers to do a clean, thread-safe,
   *             mop-up from your code
   */
  public static void runFinalizersOnExit(boolean finalizeOnExit)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkExit(0);
    current.finalizeOnExit = finalizeOnExit;
  }

  /**
   * Create a new subprocess with the specified command line. Calls
   * <code>exec(cmdline, null, null)</code>. A security check is performed,
   * <code>checkExec</code>.
   *
   * @param cmdline the command to call
   * @return the Process object
   * @throws SecurityException if permission is denied
   * @throws IOException if an I/O error occurs
   * @throws NullPointerException if cmdline is null
   * @throws IndexOutOfBoundsException if cmdline is ""
   */
  public Process exec(String cmdline) throws IOException
  {
    return exec(cmdline, null, null);
  }

  /**
   * Create a new subprocess with the specified command line and environment.
   * If the environment is null, the process inherits the environment of
   * this process. Calls <code>exec(cmdline, env, null)</code>. A security
   * check is performed, <code>checkExec</code>.
   *
   * @param cmdline the command to call
   * @param env the environment to use, in the format name=value
   * @return the Process object
   * @throws SecurityException if permission is denied
   * @throws IOException if an I/O error occurs
   * @throws NullPointerException if cmdline is null, or env has null entries
   * @throws IndexOutOfBoundsException if cmdline is ""
   */
  public Process exec(String cmdline, String[] env) throws IOException
  {
    return exec(cmdline, env, null);
  }

  /**
   * Create a new subprocess with the specified command line, environment, and
   * working directory. If the environment is null, the process inherits the
   * environment of this process. If the directory is null, the process uses
   * the current working directory. This splits cmdline into an array, using
   * the default StringTokenizer, then calls
   * <code>exec(cmdArray, env, dir)</code>. A security check is performed,
   * <code>checkExec</code>.
   *
   * @param cmdline the command to call
   * @param env the environment to use, in the format name=value
   * @param dir the working directory to use
   * @return the Process object
   * @throws SecurityException if permission is denied
   * @throws IOException if an I/O error occurs
   * @throws NullPointerException if cmdline is null, or env has null entries
   * @throws IndexOutOfBoundsException if cmdline is ""
   * @since 1.3
   */
  public Process exec(String cmdline, String[] env, File dir)
    throws IOException
  {
    StringTokenizer t = new StringTokenizer(cmdline);
    String[] cmd = new String[t.countTokens()];
    for (int i = 0; i < cmd.length; i++)
      cmd[i] = t.nextToken();
    return exec(cmd, env, dir);
  }

  /**
   * Create a new subprocess with the specified command line, already
   * tokenized. Calls <code>exec(cmd, null, null)</code>. A security check
   * is performed, <code>checkExec</code>.
   *
   * @param cmd the command to call
   * @return the Process object
   * @throws SecurityException if permission is denied
   * @throws IOException if an I/O error occurs
   * @throws NullPointerException if cmd is null, or has null entries
   * @throws IndexOutOfBoundsException if cmd is length 0
   */
  public Process exec(String[] cmd) throws IOException
  {
    return exec(cmd, null, null);
  }

  /**
   * Create a new subprocess with the specified command line, already
   * tokenized, and specified environment. If the environment is null, the
   * process inherits the environment of this process. Calls
   * <code>exec(cmd, env, null)</code>. A security check is performed,
   * <code>checkExec</code>.
   *
   * @param cmd the command to call
   * @param env the environment to use, in the format name=value
   * @return the Process object
   * @throws SecurityException if permission is denied
   * @throws IOException if an I/O error occurs
   * @throws NullPointerException if cmd is null, or cmd or env has null
   *         entries
   * @throws IndexOutOfBoundsException if cmd is length 0
   */
  public Process exec(String[] cmd, String[] env) throws IOException
  {
    return exec(cmd, env, null);
  }

  /**
   * Create a new subprocess with the specified command line, already
   * tokenized, and the specified environment and working directory. If the
   * environment is null, the process inherits the environment of this
   * process. If the directory is null, the process uses the current working
   * directory. A security check is performed, <code>checkExec</code>.
   *
   * @param cmd the command to call
   * @param env the environment to use, in the format name=value
   * @param dir the working directory to use
   * @return the Process object
   * @throws SecurityException if permission is denied
   * @throws IOException if an I/O error occurs
   * @throws NullPointerException if cmd is null, or cmd or env has null
   *         entries
   * @throws IndexOutOfBoundsException if cmd is length 0
   * @since 1.3
   */
  public Process exec(String[] cmd, String[] env, File dir)
    throws IOException
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkExec(cmd[0]);
    return execInternal(cmd, env, dir);
  }

  /**
   * Returns the number of available processors currently available to the
   * virtual machine. This number may change over time; so a multi-processor
   * program want to poll this to determine maximal resource usage.
   *
   * @return the number of processors available, at least 1
   */
  public native int availableProcessors();

  /**
   * Find out how much memory is still free for allocating Objects on the heap.
   *
   * @return the number of bytes of free memory for more Objects
   */
  public native long freeMemory();

  /**
   * Find out how much memory total is available on the heap for allocating
   * Objects.
   *
   * @return the total number of bytes of memory for Objects
   */
  public native long totalMemory();

  /**
   * Returns the maximum amount of memory the virtual machine can attempt to
   * use. This may be <code>Long.MAX_VALUE</code> if there is no inherent
   * limit (or if you really do have a 8 exabyte memory!).
   *
   * @return the maximum number of bytes the virtual machine will attempt
   *         to allocate
   */
  public native long maxMemory();

  /**
   * Run the garbage collector. This method is more of a suggestion than
   * anything. All this method guarantees is that the garbage collector will
   * have "done its best" by the time it returns. Notice that garbage
   * collection takes place even without calling this method.
   */
  public native void gc();

  /**
   * Run finalization on all Objects that are waiting to be finalized. Again,
   * a suggestion, though a stronger one than {@link #gc()}. This calls the
   * <code>finalize</code> method of all objects waiting to be collected.
   *
   * @see #finalize()
   */
  public native void runFinalization();

  /**
   * Tell the VM to trace every bytecode instruction that executes (print out
   * a trace of it).  No guarantees are made as to where it will be printed,
   * and the VM is allowed to ignore this request.
   *
   * @param on whether to turn instruction tracing on
   */
  public native void traceInstructions(boolean on);

  /**
   * Tell the VM to trace every method call that executes (print out a trace
   * of it).  No guarantees are made as to where it will be printed, and the
   * VM is allowed to ignore this request.
   *
   * @param on whether to turn method tracing on
   */
  public native void traceMethodCalls(boolean on);

  /**
   * Load a native library using the system-dependent filename. This is similar
   * to loadLibrary, except the only name mangling done is inserting "_g"
   * before the final ".so" if the VM was invoked by the name "java_g". There
   * may be a security check, of <code>checkLink</code>.
   *
   * @param filename the file to load
   * @throws SecurityException if permission is denied
   * @throws UnsatisfiedLinkError if the library is not found
   */
  public void load(String filename)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkLink(filename);
    _load(filename, false);
  }

  /**
   * Load a native library using a system-independent "short name" for the
   * library.  It will be transformed to a correct filename in a
   * system-dependent manner (for example, in Windows, "mylib" will be turned
   * into "mylib.dll").  This is done as follows: if the context that called
   * load has a ClassLoader cl, then <code>cl.findLibrary(libpath)</code> is
   * used to convert the name. If that result was null, or there was no class
   * loader, this searches each directory of the system property
   * <code>java.library.path</code> for a file named
   * <code>System.mapLibraryName(libname)</code>. There may be a security
   * check, of <code>checkLink</code>.
   *
   * @param libname the library to load
   *
   * @throws SecurityException if permission is denied
   * @throws UnsatisfiedLinkError if the library is not found
   *
   * @see System#mapLibraryName(String)
   * @see ClassLoader#findLibrary(String)
   */
  public void loadLibrary(String libname)
  {
    // This is different from the Classpath implementation, but I
    // believe it is more correct.
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkLink(libname);
    _load(libname, true);
  }

  /**
   * Return a localized version of this InputStream, meaning all characters
   * are localized before they come out the other end.
   *
   * @param in the stream to localize
   * @return the localized stream
   * @deprecated <code>InputStreamReader</code> is the preferred way to read
   *             local encodings
   */
  public InputStream getLocalizedInputStream(InputStream in)
  {
    return in;
  }

  /**
   * Return a localized version of this OutputStream, meaning all characters
   * are localized before they are sent to the other end.
   *
   * @param out the stream to localize
   * @return the localized stream
   * @deprecated <code>OutputStreamWriter</code> is the preferred way to write
   *             local encodings
   */
  public OutputStream getLocalizedOutputStream(OutputStream out)
  {
    return out;
  }

  /**
   * Native method that actually shuts down the virtual machine.
   *
   * @param status the status to end the process with
   */
  native void exitInternal(int status);

  /**
   * Load a file. If it has already been loaded, do nothing. The name has
   * already been mapped to a true filename.
   *
   * @param filename the file to load
   * @param do_search True if we should search the load path for the file
   */
  native void _load(String filename, boolean do_search);

  /**
   *This is a helper function for the ClassLoader which can load
   * compiled libraries.  Returns true if library (which is just the
   * base name -- path searching is done by this function) was loaded,
   * false otherwise.
   */
  native boolean loadLibraryInternal(String libname);

  /**
   * A helper for Runtime static initializer which does some internal native
   * initialization.
   */
  private static native void init ();

  /**
   * Run finalizers when exiting.
   */
  private native void runFinalizationForExit();

  /**
   * Map a system-independent "short name" to the full file name, and append
   * it to the path.
   * XXX This method is being replaced by System.mapLibraryName.
   *
   * @param pathname the path
   * @param libname the short version of the library name
   * @return the full filename
   */
  static native String nativeGetLibname(String pathname, String libname);

  /**
   * Execute a process. The command line has already been tokenized, and
   * the environment should contain name=value mappings. If directory is null,
   * use the current working directory; otherwise start the process in that
   * directory.
   *
   * @param cmd the non-null command tokens
   * @param env the non-null environment setup
   * @param dir the directory to use, may be null
   * @return the newly created process
   * @throws NullPointerException if cmd or env have null elements
   * @throws IOException if the exec fails
   */
  native Process execInternal(String[] cmd, String[] env, File dir)
    throws IOException;
} // class Runtime
