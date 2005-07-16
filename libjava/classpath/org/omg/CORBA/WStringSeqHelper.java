/* WStringSeqHelper.java --
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

import gnu.CORBA.primitiveArrayTypeCode;

import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * Provides static helper methods for working with
 * CORBA <code>wstring</code> array.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class WStringSeqHelper
{
  /**
   * Extract the <code>String[]</code> from the
   * given {@link Any}. This implementation expects the
   * {@link Any} to hold the instance of {@link WStringSeqHolder}
   * that is returned by {@link Any#extract_Streamable() }.
   *
   * @param a an Any to extract the array from.
   *
   * @return the extracted array.
   *
   * @throws ClassCastException if the Any contains something other than the
   * the {@link WStringSeqHolder}.
   */
  public static String[] extract(Any a)
  {
    WStringSeqHolder h = (WStringSeqHolder) a.extract_Streamable();
    return h.value;
  }

  /**
   * Returns the agreed Id, delegating functionality to
   * the {@link #type()}.id().
   */
  public static String id()
  {
    try
      {
        return type().id();
      }
    catch (BadKind ex)
      {
        // Should never happen under correct work.
        throw new Error("Please report this bug.", ex);
      }
  }

  /**
  * Insert into the given <code>String[]</code> into the
  * given {@link Any}. This implementation first creates
  * a {@link WStringSeqHolder} and then calls
  * {@link Any#insert_Streamable(Streamable)}.
  *
  * @param into the target Any.
  * @param that the array to insert.
  */
  public static void insert(Any into, String[] that)
  {
    WStringSeqHolder holder = new WStringSeqHolder(that);
    into.insert_Streamable(holder);
  }

  /**
   * Reads the <code>String[]</code> from the CORBA input stream.
   *
   * @param input the CORBA (not java.io) stream to read from.
   * @return the value from the stream.
   */
  public static String[] read(InputStream input)
  {
    String[] value = new String[ input.read_long() ];
    for (int i = 0; i < value.length; i++)
      {
        value [ i ] = input.read_wstring();
      }
    return value;
  }

  /**
   * Creates and returns a new instance of the TypeCode,
   * corresponding the CORBA <code>wstring[]</code>.
   * The length of the sequence is left with the initial
   * value 0.
   */
  public static TypeCode type()
  {
    return new primitiveArrayTypeCode(TCKind.tk_string);
  }

  /**
   * Writes the <code>String[]</code> into the given stream.
   *
   * @param output the CORBA (not java.io) output stream to write.
   * @param value the value that must be written.
   */
  public static void write(OutputStream output, String[] value)
  {
    output.write_long(value.length);

    for (int i = 0; i < value.length; i++)
      {
        output.write_wstring(value [ i ]);
      }
  }
}
