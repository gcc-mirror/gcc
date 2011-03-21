/* CompletionStatusHelper.java --
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


package org.omg.CORBA;

import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * Provides static helper methods for working with
 * {@link CompletionStatus}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class CompletionStatusHelper
{
  /**
   * Extract the {@link CompletionStatus} from the
   * given {@link Any}. This implementation expects
   * the integer (Corba long) value, stating the completion
   * status.
   *
   * @param a an Any to extract the completion status from.
   *
   * @return completion status
   */
  public static CompletionStatus extract(Any a)
  {
    return CompletionStatus.from_int(a.extract_long());
  }

  /**
   * Returns the agreed Id.
   * @return <code>IDL:omg.org/CORBA/CompletionStatus:1.0</code>, always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/CompletionStatus:1.0";
  }

  /**
  * Insert into the given {@link CompletionStatus} into the
  * given {@link Any}. This implementation inserts it as an
  * integer (CORBA long).
  *
  * @param into the target Any.
  * @param that the {@link CompletionStatus} to insert.
  */
  public static void insert(Any into, CompletionStatus that)
  {
    into.insert_long(that.value());
  }

  /**
   * Reads the {@link CompletionStatus} from the CORBA input stream.
   * This implementation reads an an integer (CORBA long).
   *
   * @param input the CORBA (not java.io) stream to read from.
   * @return the value from the stream.
   */
  public static CompletionStatus read(InputStream input)
  {
    return CompletionStatus.from_int(input.read_long());
  }

  /**
   * Writes the {@link CompletionStatus} into the given stream.
   * This implementation writes an int (CORBA long), corresponding
   * the status of completion.
   *
   * @param output the CORBA (not java.io) output stream to write.
   * @param status the value that must be written.
   */
  public static void write(OutputStream output, CompletionStatus status)
  {
    output.write_long(status.value());
  }

  /**
   * Get the parameter mode typecode (enumeration, named "CompletionStatus").
   * The typecode states that the enumeration can obtain one of
   * the following values:  COMPLETED_YES ,COMPLETED_NO or COMPLETED_MAYBE .
   */
  public static TypeCode type()
  {
    String[] members =
      new String[] { "COMPLETED_YES", "COMPLETED_NO", "COMPLETED_MAYBE" };

    return
      OrbRestricted.Singleton.create_enum_tc(id(), "CompletionStatus",
                                             members
      );
  }
}
