/* Process.java - Represent spawned system process
   Copyright (C) 1998, 1999, 2001, 2002, 2003 Free Software Foundation, Inc.

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


package java.lang;

import java.io.OutputStream;
import java.io.InputStream;

/**
 * An instance of a subclass of <code>Process</code> is created by the
 * <code>Runtime.exec</code> methods.  Methods in <code>Process</code>
 * provide a means to send input to a process, obtain the output from a
 * subprocess, destroy a subprocess, obtain the exit value from a
 * subprocess, and wait for a subprocess to complete.
 *
 * <p>This is dependent on the platform, and some processes (like native
 * windowing processes, 16-bit processes in Windows, or shell scripts) may
 * be limited in functionality. Because some platforms have limited buffers
 * between processes, you may need to provide input and read output to prevent
 * the process from blocking, or even deadlocking.
 *
 * <p>Even if all references to this object disapper, the process continues
 * to execute to completion. There are no guarantees that the
 * subprocess execute asynchronously or concurrently with the process which
 * owns this object.
 *
 * @author Brian Jones
 * @author Tom Tromey <tromey@cygnus.com>
 * @see Runtime#exec(String[], String[], File)
 * @since 1.0
 * @status updated to 1.4
 */
public abstract class Process
{
  /**
   * Empty constructor does nothing.
   */
  public Process()
  {
  }

  /**
   * Obtain the output stream that sends data to the subprocess. This is
   * the STDIN of the subprocess. When implementing, you should probably
   * use a buffered stream.
   *
   * @return the output stream that pipes to the process input
   */
  public abstract OutputStream getOutputStream();

  /**
   * Obtain the input stream that receives data from the subprocess. This is
   * the STDOUT of the subprocess. When implementing, you should probably
   * use a buffered stream.
   *
   * @return the input stream that pipes data from the process output
   */
  public abstract InputStream getInputStream();

  /**
   * Obtain the input stream that receives data from the subprocess. This is
   * the STDERR of the subprocess. When implementing, you should probably
   * use a buffered stream.
   *
   * @return the input stream that pipes data from the process error output
   */
  public abstract InputStream getErrorStream();

  /**
   * The thread calling <code>waitFor</code> will block until the subprocess
   * has terminated. If the process has already terminated then the method
   * immediately returns with the exit value of the subprocess.
   *
   * @return the subprocess exit value; 0 conventionally denotes success
   * @throws InterruptedException if another thread interrupts the blocked one
   */
  public abstract int waitFor() throws InterruptedException;

  /**
   * When a process terminates there is associated with that termination
   * an exit value for the process to indicate why it terminated. A return
   * of <code>0</code> denotes normal process termination by convention.
   *
   * @return the exit value of the subprocess
   * @throws IllegalThreadStateException if the subprocess has not terminated
   */
  public abstract int exitValue();

  /**
   * Kills the subprocess and all of its children forcibly.
   */
  public abstract void destroy();
} // class Process
