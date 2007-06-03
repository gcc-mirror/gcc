/* DynFixed.java --
   Copyright (C) 2005, 2007 Free Software Foundation, Inc.

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

import org.omg.CORBA.DynAnyPackage.InvalidValue;

/**
 * Represents a CORBA <code>fixed</code>, allowing to get and set its value
 * in the form of the binary representation.
 *
 * The format, described in CORBA specification, requires to store
 * data in hexadecimal format, two digits per byte (oceted), most
 * significant digit first. The last half-byte in the representation
 * stores the sign, being 0xD for negative numbers and 0xC for
 * zero and positive numbers. To have the even number of half bytes,
 * 0x0 is appended to the beginning, if required. The position of the
 * decimal point is not stored.
 *
 * @see gnu.CORBA.BigDecimalHelper
 *
 * @deprecated by {@link org.omg.DynamicAny.DynFixed}
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynFixed
  extends DynAny
{
  /**
   * Get the value of this DynFixed in the binary form.
   *
   * @return the binary representation, defined in the header comment.
   */
  byte[] get_value();

  /**
   * Sets the value of this DynFixed from the binary representation.
   *
   * @param a_value the byte array, representing a CORBA <code>fixed</code>,
   * as defined in the header comment.
   */
  void set_value(byte[] a_value)
          throws InvalidValue;
}
