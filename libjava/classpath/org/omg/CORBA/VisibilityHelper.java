/* VisibilityHelper.java --
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
 * A helper operations for a Visibility that is mapped into java and CORBA
 * <code>short</code>. The Visibility normally takes one of the two values,
 * {@link PUBLIC_MEMBER#value} or {@link PRIVATE_MEMBER#value}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class VisibilityHelper
{
  /**
   * Insert the Visibility into the given Any.
   * Uses {@link Any#insert_short}.
   */
  public static void insert(Any any, short that)
  {
    any.insert_short(that);
  }

  /**
   * Extract the Visibility from the given Any.
   * Uses {@link Any#extract_short}.
   */
  public static short extract(Any any)
  {
    return any.extract_short();
  }

  /**
   * Return an alias of short, named "Visibility".
   */
  public static TypeCode type()
  {
        TypeCode tshort =
          OrbRestricted.Singleton.get_primitive_tc(TCKind.tk_short);
        return
          OrbRestricted.Singleton.create_alias_tc(id(), "Visibility", tshort);
  }

  /**
   * Get the Visibility repository id.
   *
   * @return "IDL:omg.org/CORBA/Visibility:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/Visibility:1.0";
  }

  /**
   * Read the visibility value (as short) from the CDR intput stream.
   *
   * Uses {@link InputStream#read_short()}.
   *
   * @param istream a stream to read from.
   */
  public static short read(InputStream istream)
  {
    return istream.read_short();
  }

  /**
   * Write the visibility value (as short) to the CDR output stream.
   *
   * USes {@link OutputStream#write_short(short)}.
   *
   * @param ostream a stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream ostream, short value)
  {
    ostream.write_short(value);
  }
}