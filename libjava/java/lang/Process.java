/* Process.java - Represent spawned system process.
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.lang;

import java.io.OutputStream;
import java.io.InputStream;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 */

/**
 * An instance of a subclass of <code>Process</code> is created by the
 * <code>Runtime.exec</code> methods.  Methods in <code>Process</code>
 * provide a means to send input to a process, obtain the output from a 
 * subprocess, destroy a subprocess, obtain the exit value from a 
 * subprocess, and wait for a subprocess to complete.
 * 
 * @since JDK 1.0
 * 
 * @author Brian Jones
 * @author Tom Tromey <tromey@cygnus.com>
 */
public abstract class Process
{
  /**
   * Empty constructor does nothing.
   */
  public Process() { }

  /**
   * Obtain the output stream of the subprocess.  It may help to 
   * associate this stream as the redirected STDIN file descriptor of
   * the subprocess.
   */
  public abstract OutputStream getOutputStream();

  /**
   * Obtain the input stream of the subprocess.  It may help to 
   * associate this stream as the redirected STDOUT file descriptor of
   * the subprocess.
   */
  public abstract InputStream getInputStream();

  /**
   * Obtain the error input stream of the subprocess.  It may help to 
   * associate this stream as the redirected STDERR file descriptor of
   * the subprocess.
   */
  public abstract InputStream getErrorStream();

  /**
   * The thread calling <code>waitFor</code> will block until the subprocess
   * has terminated.  If the process has already terminated then the method
   * immediately returns with the exit value of the subprocess.
   * 
   * @returns the exit value of the subprocess.  A return of <code>0</code> 
   * denotes normal process termination by convention.
   *
   * @throws InterruptedException is thrown if another thread interrupts 
   * the waiting thread.  The waiting thread stops waiting.
   */
  public abstract int waitFor()
    throws InterruptedException;

  /**
   * When a process terminates there is associated with that termination
   * an exit value for the process to indicate why it terminated.  A return
   * of <code>0</code> denotes normal process termination by convention.
   *
   * @returns the exit value of the subprocess.
   * @throws IllegalThreadStateException is thrown if the subprocess 
   * represented by the subclass of this class has not yet terminated.
   */
  public abstract int exitValue();

  /**
   * Kills the subprocess and all of its children forcibly.
   */
  public abstract void destroy();

}
