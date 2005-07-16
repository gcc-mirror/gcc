/* Streamable.java --
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


package org.omg.CORBA.portable;

import org.omg.CORBA.TypeCode;

/**
 * The base class for the Holder classess of IDL types.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public interface Streamable
{
  /**
   * Fill in the value field for this holder by reading the required data
   * from the given stream.
   *
   * @param input the input stream to read from.
   */
  void _read(InputStream input);

  /**
   * Returns the TypeCode, corresponding the CORBA type that is stored
   * using this holder.
   */
  TypeCode _type();

  /**
   * Write the value field to the given stream.
   *
   * @param output the output stream to write into.
   */
  void _write(OutputStream output);
}
