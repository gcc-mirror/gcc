/* java.lang.Throwable -- Reference implementation of root class for
   all Exceptions and Errors
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.io.PrintWriter;
import java.io.PrintStream;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 30, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status: Sufficient for compiled code, but methods applicable to
 * bytecode not implemented.  JDK 1.1.
 */

/**
 * Throwable is the superclass of all exceptions that can be raised.
 *
 * <p>There are two special cases: {@link Error} and {@link RuntimeException}:
 * these two classes (and their subclasses) are considered unchecked
 * exceptions, and are either frequent enough or catastrophic enough that you
 * do not need to declare them in <code>throws</code> clauses.  Everything
 * else is a checked exception, and is ususally a subclass of
 * {@link Exception}; these exceptions have to be handled or declared.
 *
 * <p>Instances of this class are usually created with knowledge of the
 * execution context, so that you can get a stack trace of the problem spot
 * in the code.  Also, since JDK 1.4, Throwables participate in "exception
 * chaining."  This means that one exception can be caused by another, and
 * preserve the information of the original.
 *
 * <p>One reason this is useful is to wrap exceptions to conform to an
 * interface.  For example, it would be bad design to require all levels
 * of a program interface to be aware of the low-level exceptions thrown
 * at one level of abstraction. Another example is wrapping a checked
 * exception in an unchecked one, to communicate that failure occured
 * while still obeying the method throws clause of a superclass.
 *
 * <p>A cause is assigned in one of two ways; but can only be assigned once
 * in the lifetime of the Throwable.  There are new constructors added to
 * several classes in the exception hierarchy that directly initialize the
 * cause, or you can use the <code>initCause</code> method. This second
 * method is especially useful if the superclass has not been retrofitted
 * with new constructors:<br>
 * <pre>
 * try
 *   {
 *     lowLevelOp();
 *   }
 * catch (LowLevelException lle)
 *   {
 *     throw (HighLevelException) new HighLevelException().initCause(lle);
 *   }
 * </pre>
 * Notice the cast in the above example; without it, your method would need
 * a throws clase that declared Throwable, defeating the purpose of chainig
 * your exceptions.
 *
 * <p>By convention, exception classes have two constructors: one with no
 * arguments, and one that takes a String for a detail message.  Further,
 * classes which are likely to be used in an exception chain also provide
 * a constructor that takes a Throwable, with or without a detail message
 * string.
 *
 * <p>Another 1.4 feature is the StackTrace, a means of reflection that
 * allows the program to inspect the context of the exception, and which is
 * serialized, so that remote procedure calls can correctly pass exceptions.
 *
 * @author Brian Jones
 * @author John Keiser
 * @author Mark Wielaard
 * @author Tom Tromey
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.0
 * @status still missing 1.4 functionality
 */
public class Throwable implements Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -3042686055658047285L;

  /**
   * The detail message.
   *
   * @serial specific details about the exception, may be null
   */
  private String detailMessage;

  /**
   * The cause of the throwable, including null for an unknown or non-chained
   * cause. This may only be set once; so the field is set to
   * <code>this</code> until initialized.
   *
   * @serial the cause, or null if unknown, or this if not yet set
   * @since 1.4
   */
  private Throwable cause = this;

  /**
   * The stack trace, in a serialized form.
   *
   * @serial the elements of the stack trace; this is non-null, and has
   *         no null entries
   * @since 1.4
   */
  private StackTraceElement[] stackTrace;

  /**
   * Instantiate this Throwable with an empty message. The cause remains
   * uninitialized.  {@link #fillInStackTrace()} will be called to set
   * up the stack trace.
   */
  public Throwable()
  {
    this((String) null);
  }

  /**
   * Instantiate this Throwable with the given message. The cause remains
   * uninitialized.  {@link #fillInStackTrace()} will be called to set
   * up the stack trace.
   *
   * @param message the message to associate with the Throwable
   */
  public Throwable(String message)
  {
    fillInStackTrace();
    detailMessage = message;  
  }

  /**
   * Instantiate this Throwable with the given message and cause. Note that
   * the message is unrelated to the message of the cause.
   * {@link #fillInStackTrace()} will be called to set up the stack trace.
   *
   * @param message the message to associate with the Throwable
   * @param cause the cause, may be null
   * @since 1.4
   */
  public Throwable(String message, Throwable cause)
  {
    this(message);
    initCause(cause);
  }

  /**
   * Instantiate this Throwable with the given cause. The message is then
   * built as <code>cause == null ? null : cause.toString()</code>.
   * {@link #fillInStackTrace()} will be called to set up the stack trace.
   *
   * @param cause the cause, may be null
   * @since 1.4
   */
  public Throwable(Throwable cause)
  {
    this(cause == null ? null : cause.toString(), cause);
  }

  /**
   * Get the message associated with this Throwable.
   *
   * @return the error message associated with this Throwable, may be null
   */
  public String getMessage()
  {
    return detailMessage;
  }

  /**
   * Get a localized version of this Throwable's error message.
   * This method must be overridden in a subclass of Throwable
   * to actually produce locale-specific methods.  The Throwable
   * implementation just returns getMessage().
   *
   * @return a localized version of this error message
   * @see #getMessage()
   * @since 1.1
   */
  public String getLocalizedMessage()
  {
    return getMessage();
  }

  /**
   * Returns the cause of this exception, or null if the cause is not known
   * or non-existant. This cause is initialized by the new constructors,
   * or by calling initCause.
   *
   * @return the cause of this Throwable
   * @since 1.4
   */
  public Throwable getCause()
  {
    return cause == this ? null : cause;
  }

  /**
   * Initialize the cause of this Throwable.  This may only be called once
   * during the object lifetime, including implicitly by chaining
   * constructors.
   *
   * @param cause the cause of this Throwable, may be null
   * @return this
   * @throws IllegalArgumentException if cause is this (a Throwable can't be
   *         its own cause!)
   * @throws IllegalStateException if the cause has already been set
   * @since 1.4
   */
  public Throwable initCause(Throwable cause)
  {
    if (cause == this)
      throw new IllegalArgumentException();
    if (this.cause != this)
      throw new IllegalStateException();
    this.cause = cause;
    return this;
  }

  /**
   * Get a human-readable representation of this Throwable. The detail message
   * is retrieved by getLocalizedMessage().  Then, with a null detail
   * message, this string is simply the object's class name; otherwise
   * the string is <code>getClass().getName() + ": " + message</code>.
   *
   * @return a human-readable String represting this Throwable
   */
  public String toString()
  {
    String msg = getLocalizedMessage();
    return getClass().getName() + (msg == null ? "" : ": " + msg);
  }

  /**
   * Print a stack trace to the standard error stream. This stream is the
   * current contents of <code>System.err</code>. The first line of output
   * is the result of {@link #toString()}, and the remaining lines represent
   * the data created by {@link #fillInStackTrace()}. While the format is
   * unspecified, this implementation uses the suggested format, demonstrated
   * by this example:<br>
   * <pre>
   * public class Junk
   * {
   *   public static void main(String args[])
   *   {
   *     try
   *       {
   *         a();
   *       }
   *     catch(HighLevelException e)
   *       {
   *         e.printStackTrace();
   *       }
   *   }
   *   static void a() throws HighLevelException
   *   {
   *     try
   *       {
   *         b();
   *       }
   *     catch(MidLevelException e)
   *       {
   *         throw new HighLevelException(e);
   *       }
   *   }
   *   static void b() throws MidLevelException
   *   {
   *     c();
   *   }
   *   static void c() throws MidLevelException
   *   {
   *     try
   *       {
   *         d();
   *       }
   *     catch(LowLevelException e)
   *       {
   *         throw new MidLevelException(e);
   *       }
   *   }
   *   static void d() throws LowLevelException
   *   {
   *     e();
   *   }
   *   static void e() throws LowLevelException
   *   {
   *     throw new LowLevelException();
   *   }
   * }
   * class HighLevelException extends Exception
   * {
   *   HighLevelException(Throwable cause) { super(cause); }
   * }
   * class MidLevelException extends Exception
   * {
   *   MidLevelException(Throwable cause)  { super(cause); }
   * }
   * class LowLevelException extends Exception
   * {
   * }
   * </pre>
   * <p>
   * <pre>
   *  HighLevelException: MidLevelException: LowLevelException
   *          at Junk.a(Junk.java:13)
   *          at Junk.main(Junk.java:4)
   *  Caused by: MidLevelException: LowLevelException
   *          at Junk.c(Junk.java:23)
   *          at Junk.b(Junk.java:17)
   *          at Junk.a(Junk.java:11)
   *          ... 1 more
   *  Caused by: LowLevelException
   *          at Junk.e(Junk.java:30)
   *          at Junk.d(Junk.java:27)
   *          at Junk.c(Junk.java:21)
   *          ... 3 more
   * </pre>
   */
  public void printStackTrace()
  {
    printStackTrace(System.err);
  }

  /**
   * Print a stack trace to the specified PrintStream. See
   * {@link #printStackTrace()} for the sample format.
   *
   * @param s the PrintStream to write the trace to
   */
  public void printStackTrace(PrintStream s)
  {
    printStackTrace(new PrintWriter(s));
  }

  /**
   * <p>Prints the exception, the detailed message and the stack trace
   * associated with this Throwable to the given <code>PrintWriter</code>.
   * The actual output written is implemention specific. Use the result of
   * <code>getStackTrace()</code> when more precise information is needed.
   *
   * <p>This implementation first prints a line with the result of this
   * object's <code>toString()</code> method.
   * <br>
   * Then for all elements given by <code>getStackTrace</code> it prints
   * a line containing three spaces, the string "at " and the result of calling
   * the <code>toString()</code> method on the <code>StackTraceElement</code>
   * object. If <code>getStackTrace()</code> returns an empty array it prints
   * a line containing three spaces and the string
   * "&lt;&lt;No stacktrace available&gt;&gt;".
   * <br>
   * Then if <code>getCause()</code> doesn't return null it adds a line
   * starting with "Caused by: " and the result of calling
   * <code>toString()</code> on the cause.
   * <br>
   * Then for every cause (of a cause, etc) the stacktrace is printed the
   * same as for the top level <code>Throwable</code> except that as soon
   * as all the remaining stack frames of the cause are the same as the
   * the last stack frames of the throwable that the cause is wrapped in
   * then a line starting with three spaces and the string "... X more" is
   * printed, where X is the number of remaining stackframes.
   *
   * @param w the PrintWriter to write the trace to
   * @since 1.1
   */
  public void printStackTrace (PrintWriter pw)
  {
    // First line
    pw.println(toString());

    // The stacktrace
    StackTraceElement[] stack = getStackTrace();
    if (stack == null || stack.length == 0)
      {
	pw.println("   <<No stacktrace available>>");
	return;
      }
    else
      {
	for (int i = 0; i < stack.length; i++)
	  pw.println("   at " + stack[i]);
      }

    // The cause(s)
    Throwable cause = getCause();
    while (cause != null)
      {
        // Cause first line
        pw.println("Caused by: " + cause);

        // Cause stacktrace
        StackTraceElement[] parentStack = stack;
        stack = cause.getStackTrace();
	if (stack == null || stack.length == 0)
	  {
	    pw.println("   <<No stacktrace available>>");
	  }
	else if (parentStack == null || parentStack.length == 0)
	  {
	    for (int i = 0; i < stack.length; i++)
	      pw.println("   at " + stack[i]);
	  }
	else
	  {
	    boolean equal = false; // Is rest of stack equal to parent frame?
	    for (int i = 0; i < stack.length && ! equal; i++)
	      {
		// Check if we already printed the rest of the stack
		// since it was the tail of the parent stack
		int remaining = stack.length - i;
		int element = i;
		int parentElement = parentStack.length - remaining;
		equal = parentElement >= 0
		      && parentElement < parentStack.length; // be optimistic
		while (equal && element < stack.length)
		  {
		    if (stack[element].equals(parentStack[parentElement]))
		      {
			element++;
			parentElement++;
		      }
		    else
		      equal = false;
		  }
		// Print stacktrace element or indicate the rest is equal 
		if (! equal)
		  pw.println("   at " + stack[i]);
		else
		  pw.println("   ..." + remaining + " more");
	      }
	  }
        cause = cause.getCause();
      }
  }

  /**
   * Fill in the stack trace with the current execution stack.
   *
   * @return this same throwable
   * @see #printStackTrace()
   */
  public native Throwable fillInStackTrace();

  /**
   * Provides access to the information printed in {@link #printStackTrace()}.
   * The array is non-null, with no null entries, although the virtual
   * machine is allowed to skip stack frames.  If the array is not 0-length,
   * then slot 0 holds the information on the stack frame where the Throwable
   * was created (or at least where <code>fillInStackTrace()</code> was
   * called).
   *
   * @return an array of stack trace information, as available from the VM
   * @since 1.4
   */
  public StackTraceElement[] getStackTrace()
  {
    if (stackTrace == null)
      stackTrace = getStackTrace0();

    return stackTrace;
  }

  /**
   * Change the stack trace manually. This method is designed for remote
   * procedure calls, which intend to alter the stack trace before or after
   * serialization according to the context of the remote call.
   *
   * @param stackTrace the new trace to use
   * @throws NullPointerException if stackTrace is null or has null elements
   * @since 1.4
   */
  public void setStackTrace(StackTraceElement[] stackTrace)
  {
    for (int i = stackTrace.length; --i >= 0; )
      if (stackTrace[i] == null)
        throw new NullPointerException();
    this.stackTrace = stackTrace;
  }

  private native final StackTraceElement[] getStackTrace0 ();

  // Setting this flag to false prevents fillInStackTrace() from running.
  static boolean trace_enabled = true;
  private transient byte stackTraceBytes[];
}
