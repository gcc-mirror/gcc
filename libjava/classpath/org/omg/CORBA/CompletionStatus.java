/* CompletionStatus.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.CORBA;

import gnu.CORBA.Minor;

import java.io.Serializable;

import org.omg.CORBA.portable.IDLEntity;

/**
 * Defines the method completion status, usually for the time moment,
 * when the exception has been thrown.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public final class CompletionStatus
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID for interoperability.
   * Using the version 1.4 UID.
   */
  private static final long serialVersionUID = -9047319660881406859L;

  /**
   * The method was completed when the exception was thrown.
   */
  public static final int _COMPLETED_YES = 0;

  /**
   * The method was running when the exception was thrown.
   */
  public static final int _COMPLETED_NO = 1;

  /**
   * The method was either running or complete (no exact information availabe)
   * when the exception was thrown.
   */
  public static final int _COMPLETED_MAYBE = 2;

  /**
   * An instance of CompletionStatus, initialized to {@link #COMPLETED_YES }
   */
  public static final CompletionStatus COMPLETED_YES =
    new CompletionStatus(_COMPLETED_YES);

  /**
   * An instance of CompletionStatus, initialized to {@link #COMPLETED_NO }
   */
  public static final CompletionStatus COMPLETED_NO =
    new CompletionStatus(_COMPLETED_NO);

  /**
   * An instance of CompletionStatus, initialized to {@link #COMPLETED_MAYBE }
   */
  public static final CompletionStatus COMPLETED_MAYBE =
    new CompletionStatus(_COMPLETED_MAYBE);

  /**
   * The private array of all states. As long as the states form the uniform
   * sequence, from_int can find the needed value directly indexing this array.
   */
  private static final CompletionStatus[] states =
    new CompletionStatus[] { COMPLETED_YES, COMPLETED_NO, COMPLETED_MAYBE };
  private final int _value;

  /**
   * No other instances can be created.
   */
  private CompletionStatus(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the CompletionStatus, matching the given integer constant
   * 
   * @param completion one of COMPLETED_YES, COMPLETED_NO or COMPLEED_MAYBE.
   * @return one of COMPLETED_YES, COMPLETED_NO or COMPLEED_MAYBE.
   * @throws BAD_PARAM if the parameter is not one of these three valid values.
   */
  public static CompletionStatus from_int(int completion)
  {
    try
      {
        return states[completion];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Invalid completion status "
          + completion);
        bad.minor = Minor.Enumeration;
        throw bad;
      }
  }

  /**
   * Returns a short string representation.
   * @return a string, defining the completion status.
   */
  public String toString()
  {
    switch (value())
      {
        case _COMPLETED_YES :
          return "completed";

        case _COMPLETED_NO :
          return "not completed";

        case _COMPLETED_MAYBE :
          return "maybe completed";

        default :
          return "invalid completion status instance";
      }
  }

  /**
   * Returns the value, representing the completion
   * status of this object.
   * @return one of COMPLETED_MAYBE, COMPLETED_YES or COMPLETED_NO
   */
  public int value()
  {
    return _value;
  }
}
