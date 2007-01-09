/* ProcessBuilder.java - Represent spawned system process
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * This class is used to construct new operating system processes.
 * A <code>ProcessBuilder</code> instance basically represent a
 * template for a new process.  Actual processes are generated from
 * this template via use of the <code>start()</code> method, which
 * may be invoked multiple times, with each invocation spawning a
 * new process with the current attributes of the
 * <code>ProcessBuilder</code> object.  Each spawned process is
 * independent of the <code>ProcessBuilder</code> object, and is
 * unaffected by changes in its attributes.
 * </p>
 * <p>
 * The following attributes define a process:
 * </p>
 * <ul>
 * <li>The <emphasis>working directory</emphasis>; the activities of a
 * process begin with the current directory set to this.  By default,
 * this is the working directory of the current process, as defined
 * by the <code>user.dir</code> property.</li>
 * <li>The <emphasis>command</emphasis> which invokes the process.  This
 * usually consists of the name of the program binary followed by an
 * arbitrary number of arguments.  For example, <code>find -type f</code>
 * invokes the <code>find</code> binary with the arguments "-type" and "f".
 * The command is provided a list, the elements of which are defined in a
 * system dependent manner; the layout is affected by expected operating
 * system conventions.  A common method is to split the command on each
 * space within the string.  Thus, <code>find -type f</code> forms a
 * three element list.  However, in some cases, the expectation is that
 * this split is performed by the program itself; thus, the list consists
 * of only two elements (the program name and its arguments).</li>
 * <li>The <emphasis>environment map</emphasis>, which links environment
 * variables to their corresponding values.  The initial contents of the map
 * are the current environment values i.e. it contains the contents of the
 * map returned by <code>System.getenv()</code>.</li>
 * <li>The <emphasis>redirection flag</emphasis>, which specifies whether
 * or not the contents of the error stream should be redirected to standard
 * output.  By default, this is false, and there are two output streams, one
 * for normal data ({@link Process#getOutputStream()}) and one for error data
 * ({@link Process#getErrorStream()}).  When set to true, the two are merged,
 * which simplifies the interleaving of the two streams.  Data is read using
 * the stream returned by {@link Process#getOutputStream()}, and the
 * stream returned by {@link Process#getErrorStream()} throws an immediate
 * end-of-file exception.</li>
 * </ul>
 * <p>
 * All checks on attribute validity are delayed until <code>start()</code>
 * is called. <code>ProcessBuilder</code> objects are <strong>not
 * synchronized</strong>; the user must provide external synchronization
 * where multiple threads may interact with the same
 * <code>ProcessBuilder</code> object.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @see Process
 * @see System#getenv()
 * @since 1.5
 */
public final class ProcessBuilder
{

  /**
   * The working directory of the process.
   */
  private File directory = new File(System.getProperty("user.dir"));

  /**
   * The command line syntax for invoking the process.
   */
  private List<String> command;

  /**
   * The mapping of environment variables to values.
   */
  private Map<String, String> environment =
    new System.EnvironmentMap(System.getenv());

  /**
   * A flag indicating whether to redirect the error stream to standard
   * output.
   */
  private boolean redirect = false;

  /**
   * Constructs a new <code>ProcessBuilder</code> with the specified
   * command being used to invoke the process.  The list is used directly;
   * external changes are reflected in the <code>ProcessBuilder</code>.
   *
   * @param command the name of the program followed by its arguments.
   */
  public ProcessBuilder(List<String> command)
  {
    this.command = command;
  }

  /**
   * Constructs a new <code>ProcessBuilder</code> with the specified
   * command being used to invoke the process.  This constructor
   * simplifies creating a new <code>ProcessBuilder</code> by
   * converting the provided series of constructor arguments into a
   * list of command-line arguments.
   *
   * @param command the name of the program followed by its arguments.
   */
  public ProcessBuilder(String... command)
  {
    this.command = Arrays.asList(command);
  }

  /**
   * Returns the current command line, used to invoke the process.
   * The return value is simply a reference to the list of command
   * line arguments used by the <code>ProcessBuilder</code> object;
   * any changes made to it will be reflected in the operation of
   * the <code>ProcessBuilder</code>.
   *
   * @return the list of command-line arguments.
   */
  public List<String> command()
  {
    return command;
  }

  /**
   * Sets the command-line arguments to those specified.  The list is
   * used directly; external changes are reflected in the
   * <code>ProcessBuilder</code>.
   *
   * @param command the name of the program followed by its arguments.
   * @return a reference to this process builder.
   */
  public ProcessBuilder command(List<String> command)
  {
    this.command = command;
    return this;
  }

  /**
   * Sets the command-line arguments to those specified.
   * This simplifies modifying the arguments by converting
   * the provided series of constructor arguments into a
   * list of command-line arguments.
   *
   * @param command the name of the program followed by its arguments.
   * @return a reference to this process builder.
   */
  public ProcessBuilder command(String... command)
  {
    this.command = Arrays.asList(command);
    return this;
  }

  /**
   * Returns the working directory of the process.  The
   * returned value may be <code>null</code>; this
   * indicates that the default behaviour of using the
   * working directory of the current process should
   * be adopted.
   * 
   * @return the working directory.
   */
  public File directory()
  {
    return directory;
  }

  /**
   * Sets the working directory to that specified.
   * The supplied argument may be <code>null</code>,
   * which indicates the default value should be used.
   * The default is the working directory of the current
   * process.
   * 
   * @param directory the new working directory.
   * @return a reference to this process builder.
   */
  public ProcessBuilder directory(File directory)
  {
    this.directory = directory;
    return this;
  }

  /**
   * <p>
   * Returns the system environment variables of the process.
   * If the underlying system does not support environment variables,
   * an empty map is returned.
   * </p>
   * <p>
   * The returned map does not accept queries using
   * null keys or values, or those of a type other than
   * <code>String</code>.  Attempts to pass in a null value will
   * throw a <code>NullPointerException</code>.  Types other than
   * <code>String</code> throw a <code>ClassCastException</code>.
   * </p>
   * <p>
   * As the returned map is generated using data from the underlying
   * platform, it may not comply with the <code>equals()</code>
   * and <code>hashCode()</code> contracts.  It is also likely that
   * the keys of this map will be case-sensitive.
   * </p>
   * <p>
   * Modification of the map is reliant on the underlying platform;
   * some may not allow any changes to the environment variables or
   * may prevent certain values being used.  Attempts to do so will
   * throw an <code>UnsupportedOperationException</code> or
   * <code>IllegalArgumentException</code>, respectively. 
   * </p>
   * <p>
   * Use of this method may require a security check for the
   * RuntimePermission "getenv.*".
   * </p>
   *
   * @return a map of the system environment variables for the process.
   * @throws SecurityException if the checkPermission method of
   *         an installed security manager prevents access to
   *         the system environment variables.
   * @since 1.5
   */
  public Map<String, String> environment()
  {
    return environment;
  }

  /**
   * Returns true if the output stream and error stream of the
   * process will be merged to form one composite stream.  The
   * default return value is <code>false</code>.
   *
   * @return true if the output stream and error stream are to
   *         be merged.
   */
  public boolean redirectErrorStream()
  {
    return redirect;
  }

  /**
   * Sets the error stream redirection flag.  If set, the output
   * and error streams are merged to form one composite stream.
   *
   * @param redirect the new value of the redirection flag.
   * @return a reference to this process builder.
   */
  public ProcessBuilder redirectErrorStream(boolean redirect)
  {
    this.redirect = redirect;
    return this;
  }

  /**
   * <p>
   * Starts execution of a new process, based on the attributes of
   * this <code>ProcessBuilder</code> object.  This is the point
   * at which the command-line arguments are checked.  The list
   * must be non-empty and contain only non-null string objects.
   * The other attributes have default values which are used in
   * cases where their values are not explicitly specified.
   * </p>
   * <p>
   * If a security manager is in place, then the
   * {@link SecurityManager#checkExec()} method is called to
   * ensure that permission is given to execute the process.
   * </p>
   * <p>
   * The execution of the process is system-dependent.  Various
   * exceptions may result, due to problems at the operating system
   * level.  These are all returned as a form of {@link IOException}.
   * </p>
   *
   * @return a <code>Process</code> object, representing the spawned
   *         subprocess.
   * @throws IOException if a problem occurs with executing the process
   *                     at the operating system level.
   * @throws IndexOutOfBoundsException if the command to execute is
   *                                   actually an empty list.
   * @throws NullPointerException if the command to execute is null
   *                              or the list contains null elements.
   * @throws SecurityException if a security manager exists and prevents
   *                           execution of the subprocess.
   */
  public Process start() throws IOException
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe!
    if (sm != null)
      sm.checkExec(command.get(0));
    return VMProcess.exec(command, environment, directory, redirect);
  }
}
