/* NamingException.java -- Superclass of all naming Exceptions
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

package javax.naming;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * Superclass of all naming Exceptions.
 * Can contain extra information about the root cause of this exception
 * (for example when the original exception was not a subclass of
 * <code>NamingException</code>), the part of the <code>Name</code> that
 * could be resolved (including the <code>Object</code> it resolved to)
 * and the part of the <code>Name</code> that could not be resolved when
 * the exception occured.
 *
 * @since 1.3
 * @author Anthony Green (green@redhat.com)
 * @author Mark Wielaard (mark@klomp.org)
 */
public class NamingException extends Exception
{
  private static final long serialVersionUID = -1299181962103167177L;

  /**
   * The root cause of this exception. Might be null. Set by calling
   * <code>setRootCause()</code>, can be accessed by calling
   * <code>getRootCause()</code>.
   */
  protected Throwable rootException;

  /**
   * If the exception was caused while resolving a <code>Name</code> then
   * this field contains that part of the name that could be resolved.
   * Field might be null. Set by calling <code>setResolvedName()</code>.
   * Can be accessed by calling <code>getResolvedName</code>.
   */
  protected Name resolvedName;

  /**
   * If the exception was caused while resolving a <code>Name</code> then
   * this field contains the object that part of the name could be resolved to.
   * Field might be null. Set by calling <code>setResolvedObj()</code>.
   * Can be accessed by calling <code>getResolvedObj</code>.
   */
  protected Object resolvedObj;

  /**
   * If the exception was caused while resolving a <code>Name</code> then
   * this field contains that part of the name that could not be resolved.
   * Field might be null. Set by calling <code>setRemainingName()</code>.
   * The field can be extended by calling <code>appendRemainingName()</code>
   * or <code>appendRemainingComponent()</code>.
   * Can be accessed by calling <code>getRemainingName</code>.
   */
  protected Name remainingName;

  /**
   * Creates a new NamingException without a message. Does not set any of the
   * <code>rootException</code>, <code>resolvedName</code>,
   * <code>resolvedObj</code> or <code>remainingObject</code> fields.
   * These fields can be set later.
   */
  public NamingException ()
  {
    super();
  }

  /**
   * Creates a new NamingException with a detailed message. Does not set
   * the <code>rootException</code>, <code>resolvedName</code>,
   * <code>resolvedObj</code> or <code>remainingObject,</code> fields.
   * These fields can be set later.
   */
  public NamingException (String msg)
  {
    super(msg);
  }

  /**
   * Gets the root cause field <code>rootException</code> of this Exception.
   */
  public Throwable getRootCause ()
  {
    return rootException;
  }

  /**
   * Sets the root cause field <code>rootException</code> of this Exception.
   */
  public void setRootCause (Throwable e)
  {
    rootException = e;
  }

  /**
   * Gets the part of the name that could be resolved before this exception
   * happend. Returns the <code>resolvedName</code> field of this Exception.
   */
  public Name getResolvedName ()
  {
    return resolvedName;
  }

  /**
   * Sets the part of the name that could be resolved before this exception
   * happend. Sets the <code>resolvedName</code> field of this Exception.
   */
  public void setResolvedName (Name name)
  {
    resolvedName = name;
  }

  /**
   * Gets the Object to which (part of) the name could be resolved before this
   * exception happend. Returns the <code>resolvedObj</code> field of this
   * Exception.
   */
  public Object getResolvedObj ()
  {
    return resolvedObj;
  }

  /**
   * Sets the Object to which (part of) the name could be resolved before this
   * exception happend. Sets the <code>resolvedObj</code> field of this
   * Exception.
   */
  public void setResolvedObj (Object o)
  {
    resolvedObj = o;
  }

  /**
   * Gets the part of the name that could not be resolved before this exception
   * happend. Returns the <code>remainingName</code> field of this Exception.
   */
  public Name getRemainingName ()
  {
    return remainingName;
  }

  /**
   * Sets the part of the name that could be resolved before this exception
   * happend. Sets the <code>resolvedName</code> field of this Exception.
   * The field can be extended by calling <code>appendRemainingName()</code>
   * or <code>appendRemainingComponent()</code>.
   */
  public void setRemainingName (Name name)
  {
    remainingName = name;
  }

  /**
   * Adds the given <code>Name</code> to the <code>remainingName</code> field.
   * Does nothing when <code>name</code> is null or when a
   * <code>InvalidNameException</code> is thrown when adding the name.
   *
   * @see Name#addAll(Name)
   */
  public void appendRemainingName (Name name)
  {
    if (name != null)
      try
      	{
	  remainingName.addAll(name);
	}
      catch(InvalidNameException ine) { /* ignored */ }
  }

  /**
   * Adds the given <code>String</code> to the <code>remainingName</code> field.
   * Does nothing when <code>name</code> is null or when a
   * <code>InvalidNameException</code> is thrown when adding the component.
   *
   * @see Name#add(String)
   */
  public void appendRemainingComponent (String name)
  {
    if (name != null)
      try
      	{
	  remainingName.add(name);
	}
      catch(InvalidNameException ine) { /* ignored */ }
  }

  /**
   * Gets the message given to the constructor or null if no message was given.
   *
   * @see Throwable#getMessage();
   */
  public String getExplanation()
  {
    return getMessage();
  }

  /**
   * Returns a String representation of this exception and possibly including
   * the part object that could be resolved if the given flag is set to true.
   * Always includes the root cause and the remaining name if not null.
   */
  public String toString(boolean objectInfo)
  {
    StringBuffer sb = new StringBuffer(super.toString());
    Throwable cause = getRootCause();
    if (cause != null)
      {
	sb.append(" caused by ");
	sb.append(cause);
      }
    Name remaining = getRemainingName();
    if (remaining != null)
      {
	sb.append(" [remainingName: ");
	sb.append(remaining);
      }
    Object resolved = getResolvedObj();
    if (objectInfo && resolved != null)
      {
	if (remainingName == null)
	  sb.append(" [");
	else
	  sb.append(", ");
	sb.append("resolvedObj: ");
	sb.append(resolved);
      }
    if ((remaining != null) || (objectInfo && resolved != null))
      sb.append(']');

    return sb.toString();
  }

  /**
   * Returns a string representation of this exception.
   * Calls <code>toString(false)</code>.
   */
  public String toString()
  {
    return toString(false);
  }
  /**
   * Prints the stacktrace of this exception or of the root cause if not null.
   */
  public void printStackTrace()
  {
    Throwable cause = getRootCause();
    if (cause != null)
      cause.printStackTrace();
    else
      super.printStackTrace();
  }

  /**
   * Prints the stacktrace of this exception or of the root cause if not null
   * to the given <code>PrintStream</code>.
   */
  public void printStackTrace(PrintStream ps)
  {
    Throwable cause = getRootCause();
    if (cause != null)
      cause.printStackTrace(ps);
    else
      super.printStackTrace(ps);
  }

  /**
   * Prints the stacktrace of this exception or of the root cause if not null
   * to the given <code>PrintWriter</code>.
   */
  public void printStackTrace(PrintWriter pw)
  {
    Throwable cause = getRootCause();
    if (cause != null)
      cause.printStackTrace(pw);
    else
      super.printStackTrace(pw);
  }
}

