/* PolicyListHelper.java --
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
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
* The helper operations for the
* CORBA object {@link Policy}[].
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class PolicyListHelper
{
  /**
   * Get the type code of the {@link Policy}[].
   */
  public static TypeCode type()
  {
     return OrbRestricted.Singleton.create_interface_tc(id(), "Policy[]");
  }

  /**
   * Insert the Policy[] into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the Policy[] to insert.
   */
  public static void insert(Any any, Policy[] that)
  {
    any.insert_Streamable(new PolicyListHolder(that));
  }

  /**
   * Extract the Policy[] from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain Policy[].
   */
  public static Policy[] extract(Any any)
  {
    try
      {
        PolicyListHolder holds = (PolicyListHolder) any.extract_Streamable();
        return holds.value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Policy[] expected");
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Get the Policy[] repository id.
   *
   * @return "IDL:omg.org/CORBA/PolicyList:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/PolicyList:1.0";
  }

  /**
   * Read the sequence of policies from the CDR intput stream.
   * The method follows usual CDR standards (an array length as
   * CORBA long, followed by the array members, if any).
   * The array members are read using {@link PolicyHelper}.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static Policy[] read(InputStream input)
  {
    Policy[] p = new Policy[ input.read_long() ];
    for (int i = 0; i < p.length; i++)
      {
        p [ i ] = PolicyHelper.read(input);
      }
    return p;
  }

  /**
   * Write the sequence of policies from the CDR intput stream.
   * The method follows usual CDR standards (an array length as
   * CORBA long, followed by the array members, if any).
   * The array members are written using {@link PolicyHelper}.
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a policy array to write.
   */
  public static void write(OutputStream output, Policy[] value)
  {
    output.write_long(value.length);
    for (int i = 0; i < value.length; i++)
      {
        PolicyHelper.write(output, value [ i ]);
      }
  }
}
