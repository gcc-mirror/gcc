/* DNW.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA;


/**
 * Contains the static method to throw an error in the case
 * when the execution should never get into the current point.
 *
 * The error message contains the text, suggesting to check
 * the user code first and then report a bug.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Unexpected
  extends InternalError
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The default message for the CORBA assertion error.
   */
  public static final String SHARED_MESSAGE =
    "CORBA assertion error. Please check your code. " +
    "If you think it is Classpath problem, please report " +
    "this bug providing as much information as possible.";

  /**
   * Create an instance with explaining message and enclosing
   * exception.
   */
  public Unexpected(String msg, Exception why)
  {
    super(msg + ". " + SHARED_MESSAGE);
    if (why != null)
      initCause(why);
  }

  /**
   * Create an instance with enclosing exception.
   */
  public Unexpected(Exception why)
  {
    super(SHARED_MESSAGE);
    if (why != null)
      initCause(why);
  }

  /**
   * Create an instance.
   */
  public Unexpected()
  {
    super(SHARED_MESSAGE);
  }

  /**
   * Throws an error with the custom explaining message and
   * the appended share message.
   *
   * @param msg the error message
   * @param why the enclosing exception.
   */
  public static void error(String msg, Exception why)
  {
    throw new Unexpected(msg, why);
  }

  /**
   * Throws an error with the shared explaining message.
   *
   * @param why the enclosing exception.
   * @throws Error, always.
   */
  public static void error(Exception why)
  {
    throw new Unexpected(why);
  }

  /**
   * Throws an error with the shared explaining message.
   *
   * @throws Error, always.
   */
  public static void error()
  {
    throw new Unexpected();
  }
}
