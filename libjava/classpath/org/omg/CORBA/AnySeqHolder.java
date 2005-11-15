/* AnySeqHolder.java --
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

import gnu.CORBA.typecodes.ArrayTypeCode;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

/**
 * A sequence holder for CORBA <code>AnySeq</code> that is mapped into
 * java <code>Any[]</code>.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public final class AnySeqHolder
  implements Streamable
{
  /**
   * The <code>Any[]</code> (CORBA <code>AnySeq</code>) value,
   * held by this AnySeqHolder.
   */
  public Any[] value;

  /**
   * The type code for this holder. Each holder has a different instance.
   */
  private final ArrayTypeCode typecode =
    new ArrayTypeCode(TCKind.tk_any);

  /**
   * Constructs an instance of AnySeqHolder,
   * initializing {@link #value} to <code>null</code>.
   */
  public AnySeqHolder()
  {
  }

  /**
   * Constructs an instance of AnySeqHolder,  
   * initializing {@link #value} to the given array
   */
  public AnySeqHolder(Any [] a_value)
  {
    value = a_value;
  }

  /**
   * Fill in the {@link value } field by reading the required data
   * from the given stream. This method first reads the array size
   * (as CORBA <code>long</code>and then all Any's.
   *
   * @param input the input stream to read from.
   */
  public void _read(InputStream input)
  {
    value = new Any[ input.read_long() ];
    for (int i = 0; i < value.length; i++)
      {
        value [ i ] = input.read_any();
      }
    typecode.setLength(value.length);
  }

  /**
   * Returns the TypeCode, corresponding the CORBA type that is stored
   * using this holder.
   */
  public TypeCode _type()
  {
    return typecode;
  }

  /**
   * Write the {@link value } field to the given stream.
   * This method first writes the array size
   * (as CORBA <code>long</code> and then all Any's.
   *
   * @param output the output stream to write into.
   */
  public void _write(OutputStream output)
  {
    output.write_long(value.length);

    for (int i = 0; i < value.length; i++)
      {
        output.write_any(value [ i ]);
      }
  }

}
