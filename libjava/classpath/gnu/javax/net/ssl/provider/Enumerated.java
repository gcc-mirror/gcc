/* Enumerated.java -- Interface to enumerated types.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

/**
 * An enumerated type in the SSL protocols. Enumerated values take on
 * one of a set of possible numeric values, which are not specifically
 * ordered, and may be extensible to a maximum value.
 *
 * <pre>enum { e1(v1), e2(v2), ... [[, (n) ]] }</pre>
 *
 * <p>Enumerated types are encoded as big-endian multibyte integers,
 * which take up the least possible number of bytes. Thus, an
 * enumeration with up to 255 values will be encoded in a single byte,
 * and so on.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
interface Enumerated
{

  /**
   * Returns the encoded value of this enumerated value, which is
   * appropriate to send over-the-wire.
   *
   * @return The encoded value.
   */
  byte[] getEncoded();

  /**
   * Returns the numeric value of this enumerated value.
   *
   * @return The numeric value.
   */
  int getValue();

  /**
   * Returns a string representation of this enumerated value.
   *
   * @return The string.
   */
  String toString();
}
