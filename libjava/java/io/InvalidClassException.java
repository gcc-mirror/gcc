/* InvalidClassException.java -- deserializing a class failed
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

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


package java.io;

/**
 * This exception is thrown when there is some sort of problem with a
 * class during a serialization operation.  This could be:<br><ul>
 * <li>the serial version of the class doesn't match</li>
 * <li>the class contains unknown datatypes</li>
 * <li>the class does not have an accessible no-arg constructor</li>
 * </ul>.
 *
 * <p>The field <code>classname</code> will contain the name of the
 * class that caused the problem if known.  The getMessage() method
 * for this exception will always include the name of that class
 * if known.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @since 1.1
 * @status updated to 1.4
 */
public class InvalidClassException extends ObjectStreamException
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -4333316296251054416L;

  /**
   * The name of the class which encountered the error.
   *
   * @serial the classname causing the error
   */
  public String classname;

  /**
   * Create an exception with a descriptive error message, but a null
   * classname.
   *
   * @param message the descriptive error message
   */
  public InvalidClassException(String message)
  {
    super(message);
  }

  /**
   * Create an exception with a descriptive error message, and the name of
   * the class that caused the problem.
   *
   * @param classname the name of the faulty class
   * @param message the descriptive error message
   */
  public InvalidClassException(String classname, String message)
  {
    super(message);
    this.classname = classname;
  }

  /**
   * Returns the descriptive error message for this exception. It will
   * include the class name that caused the problem if known, in the format:
   * <code>[classname][; ][super.getMessage()]</code>.
   *
   * @return A descriptive error message, may be null
   */
  public String getMessage()
  {
    String msg = super.getMessage();
    if (msg == null)
      return classname;
    return (classname == null ? "" : classname + "; ") + msg;
  }
}

