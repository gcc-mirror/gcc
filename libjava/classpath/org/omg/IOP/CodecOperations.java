/* CodecOperations.java --
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


package org.omg.IOP;

import org.omg.CORBA.Any;
import org.omg.CORBA.TypeCode;
import org.omg.IOP.CodecPackage.FormatMismatch;
import org.omg.IOP.CodecPackage.InvalidTypeForEncoding;
import org.omg.IOP.CodecPackage.TypeMismatch;

/**
 * Defines the operations, applicable to
 * the Codec.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface CodecOperations
{
  /**
   * Encode the value, stored inside the given {@link Any}, into array of
   * bytes. The returned byte array contains the data structure typecode,
   * followed by the data structure itself.
   *
   * @param that the {@link Any}, containing the data structure, required to
   * encode.
   *
   * @return the array of bytes, containing the encoded data structure.
   *
   * @throws InvalidTypeForEncoding if the data structure is not supported
   * by this {@link Codec} (wide char and wide string are not supported
   * by ENCODING_CDR_ENCAPS v 1.0).
   *
   * @see #decode(byte[])
   */
  byte[] encode(Any that)
         throws InvalidTypeForEncoding;

  /**
   * Decode the given array of bytes and return the decoded value, inserted
   * into {@link Any}. This methods expects that the byte array contains
   * the CDR-encoded data structure {@link TypeCode}, followed by the data
   * structure itself.
   *
   * @param them an array of bytes to decode.
   * @return the {@link Any}, containing the decoded structure. The structure
   * can be extracted from the Any with the appropriate helper.
   *
   * @throws FormatMismatch on the invalid structure of the byte array.
   *
   * @see #encode(Any)
   */
  Any decode(byte[] them)
      throws FormatMismatch;

  /**
  * Encode the value (without the typecode), stored in the passed {@link Any},
  * into the given byte array.
  *
  * @param that_value the {@link Any}, holding the value to encode.
  * @return the array, containing the encoded value alone (no preceeding
  * typecode).
  *
  * @see #decode_value(byte[], TypeCode)
  */
  byte[] encode_value(Any that_value)
               throws InvalidTypeForEncoding;

  /**
   * Decode the given array of bytes, supposing that they contain the
   * given data structure, and return the decoded value.
   *
   * @param them the array of bytes to decode. Should contain the data type,
   * matching the structure, defined in the <code>type</code> parameter.
   * Does not contain the typecode itself.
   *
   * @param type the typecode of the data structure, stored in the byte
   * array.
   *
   * @return the {@link Any}, containing the decoded structure. The
   * structure can be extracted from the Any with the appropriate helper.
   *
   * @throws FormatMismatch on the invalid structure of the byte array.
   * @throws TypeMismatch if discovered that the the byte array defines a
   * different structure.
   *
   * @see #encode_value(Any)
   */
  Any decode_value(byte[] them, TypeCode type)
            throws FormatMismatch, TypeMismatch;
}
