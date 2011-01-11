/* DataOutputStream.java --
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

import org.omg.CORBA.portable.ValueBase;

/**
 * An interface for writing the custom value types. A value type, providing
 * its own mechanism for writing the content, must implement
 * the {@link CustomValue} that uses this interface.
 *
 * @see CustomValue
 * @see CustomMarshal
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DataOutputStream
  extends ValueBase
{
  /**
   * Write {@link Any} to the output stream.
   *
   * @param value a value to write.
   */
  void write_any(Any value);

  /**
   * Write boolean to the output stream.
   *
   * @param value a value to write.
   */
  void write_boolean(boolean value);

  /**
   * Write narrow (usually 8 bit) char to the output stream.
   *
   * @param value a value to write.
   */
  void write_char(char value);

  /**
   * Write wide (usually 16 bit) char to the output stream.
   *
   * @param value a value to write.
   */
  void write_wchar(char value);

  /**
   * Write octet (byte) to the output stream.
   *
   * @param value a value to write.
   */
  void write_octet(byte value);

  /**
   * Write short (16 bit signed integer) to the output stream.
   *
   * @param value a value to write.
   */
  void write_short(short value);

  /**
   * Write unsigned short to the output stream.
   *
   * @param value a value to write.
   */
  void write_ushort(short value);

  /**
   * Write CORBA long (32 bits, java int) to the output stream.
   *
   * @param value a value to write.
   */
  void write_long(int value);

  /**
   * Write unsigned CORBA long (32 bits, java int) to the output stream.
   *
   * @param value a value to write.
   */
  void write_ulong(int value);

  /**
   * Write CORBA long long (64 bits, java long) to the output stream.
   *
   * @param value a value to write.
   */
  void write_longlong(long value);

  /**
   * Write unsigned CORBA long long (64 bits, java long) to the output stream.
   *
   * @param value a value to write.
   */
  void write_ulonglong(long value);

  /**
   * Write float to the output stream.
   *
   * @param value a value to write.
   */
  void write_float(float value);

  /**
   * Write double to the output stream.
   *
   * @param value a value to write.
   */
  void write_double(double value);

  /**
   * Write narrow (usually 8 bits per character) string to the output stream.
   *
   * @param value a value to write.
   */
  void write_string(String value);

  /**
   * Write wide (usually 16 bits per character) string to the output stream.
   *
   * @param value a value to write.
   */
  void write_wstring(String value);

  /**
   * Write CORBA object reference to the output stream.
   *
   * @param value a value to write, null should be supported.
   */
  void write_Object(org.omg.CORBA.Object value);

  /**
   * Write abstract interface to the output stream.
   *
   * @param value a value to write, can be either CORBA object or
   * CORBA value type.
   */
  void write_Abstract(java.lang.Object value);

  /**
   * Write value type to the output stream.
   *
   * @param value a value to write.
   */
  void write_Value(java.io.Serializable value);

  /**
   * Write typecode to the output stream.
   *
   * @param value a value to write.
   */
  void write_TypeCode(TypeCode value);

  /**
   * Write array of Any's to the output stream.
   *
   * @param seq a value to write.
   */
  void write_any_array(Any[] seq, int offset, int length);

  /**
   * Write array of boolean's to the output stream.
   *
   * @param seq a value to write.
   */
  void write_boolean_array(boolean[] seq, int offset, int length);

  /**
   * Write array of narrow chars to the output stream.
   *
   * @param seq a value to write.
   */
  void write_char_array(char[] seq, int offset, int length);

  /**
   * Write array of wide chars to the output stream.
   *
   * @param seq a value to write.
   */
  void write_wchar_array(char[] seq, int offset, int length);

  /**
   * Write array of octets (bytes) to the output stream.
   *
   * @param seq a value to write.
   */
  void write_octet_array(byte[] seq, int offset, int length);

  /**
   * Write array of shorts (16 bit integers) to the output stream.
   *
   * @param seq a value to write.
   */
  void write_short_array(short[] seq, int offset, int length);

  /**
   * Write array of unsigned shorts (16 bit integers) to the output stream.
   *
   * @param seq a value to write.
   */
  void write_ushort_array(short[] seq, int offset, int length);

  /**
   * Write array of CORBA longs (java ints) to the output stream.
   *
   * @param seq a value to write.
   */
  void write_long_array(int[] seq, int offset, int length);

  /**
   * Write array of unsigned CORBA longs (java ints) to the output stream.
   *
   * @param seq a value to write.
   */
  void write_ulong_array(int[] seq, int offset, int length);

  /**
   * Write array of unsigned CORBA long longs (java longs)
   * to the output stream.
   *
   * @param seq a value to write.
   */
  void write_ulonglong_array(long[] seq, int offset, int length);

  /**
   * Write arrayo fo CORBA long longs (java ints) to the output stream.
   *
   * @param seq a value to write.
   */
  void write_longlong_array(long[] seq, int offset, int length);

  /**
   * Write array of floats to the output stream.
   *
   * @param seq a value to write.
   */
  void write_float_array(float[] seq, int offset, int length);

  /**
   * Write array of doubles to the output stream.
   *
   * @param seq a value to write.
   */
  void write_double_array(double[] seq, int offset, int length);
}
