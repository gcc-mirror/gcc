/* AnnotationFormatError.java - Thrown when an binary annotation is malformed
   Copyright (C) 2004, 2005 Free Software Foundation

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

package java.lang.annotation;

/**
 * Thrown when an annotation found in a class file is
 * malformed.  When the virtual machine finds a class file
 * containing annotations, it attempts to parse them.
 * This error is thrown if this operation fails.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class AnnotationFormatError extends Error
{
  private static final long serialVersionUID = -4256701562333669892L;

  /**
   * Constructs a new <code>AnnotationFormatError</code>
   * using the specified message to give details of the error.
   *
   * @param message the message to use in the error output.
   */
  public AnnotationFormatError(String message)
  {
    super(message);
  }

  /**
   * <p>
   * Constructs a new <code>AnnotationFormatError</code>
   * using the specified message to give details of the error.
   * The supplied cause <code>Throwable</code> is used to
   * provide additional history, with regards to the root
   * of the problem.  It is perfectly valid for this to be null, if
   * the cause is unknown.
   * </p>
   * <p>
   * <strong>Note</strong>: if a cause is supplied, the error
   * message from this cause is not automatically included in the
   * error message given by this error.
   * </p>
   *
   * @param message the message to use in the error output
   * @param cause the cause of this error, or null if the cause
   *              is unknown.
   */
  public AnnotationFormatError(String message, Throwable cause)
  {
    super(message, cause);
  }

  /**
   * Constructs a new <code>AnnotationFormatError</code> using
   * the supplied cause <code>Throwable</code> to
   * provide additional history, with regards to the root
   * of the problem.  It is perfectly valid for this to be null, if
   * the cause is unknown.  If the cause is not null, the error
   * message from this cause will also be used as the message
   * for this error.
   *
   * @param cause the cause of the error.
   */
  public AnnotationFormatError(Throwable cause)
  {
    super(cause);
  }

}
