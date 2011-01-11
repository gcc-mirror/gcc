/* DataInputStream.java --
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

import org.omg.CORBA.portable.ValueBase;

/**
 * An interface for reading the custom value types. A value type, providing
 * its own mechanism for reading the content, must implement
 * the {@link CustomValue} that uses this interface.
 *
 * @see CustomValue
 * @see CustomMarshal
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DataInputStream
  extends ValueBase
{
  /**
   * Read {@link Any}.
   * @return a value, extracted from the stream.
   */
  Any read_any();

  /**
   * Read boolean.
   * @return a value, extracted from the stream.
   */
  boolean read_boolean();

  /**
   * Read narrow (usually 8 bit) char.
   * @return a value, extracted from the stream.
   */
  char read_char();

  /**
   * Read wide (usually 16 bit) char.
   * @return a value, extracted from the stream.
   */
  char read_wchar();

  /**
   * Read octet (byte).
   * @return a value, extracted from the stream.
   */
  byte read_octet();

  /**
   * Read short (16 bit int).
   * @return a value, extracted from the stream.
   */
  short read_short();

  /**
   * Read unsigned short.
   * @return a value, extracted from the stream.
   */
  short read_ushort();

  /**
   * Read CORBA long (java int, 32 bits).
   * @return a value, extracted from the stream.
   */
  int read_long();

  /**
   * Read CORBA unsigned long (java int).
   * @return a value, extracted from the stream.
   */
  int read_ulong();

  /**
   * Read CORBA long long (java long, 64 bits).
   * @return a value, extracted from the stream.
   */
  long read_longlong();

  /**
   * Read unsigned CORBA long long (java long, 64 bits).
   * @return a value, extracted from the stream.
   */
  long read_ulonglong();

  /**
   * Read float.
   * @return a value, extracted from the stream.
   */
  float read_float();

  /**
   * Read dobule.
   * @return a value, extracted from the stream.
   */
  double read_double();

  /**
   * Read narrow string (usually 8 bits per character).
   * @return a value, extracted from the stream.
   */
  String read_string();

  /**
   * Read wide string (usually 16 bits per character).
   * @return a value, extracted from the stream.
   */
  String read_wstring();

  /**
   * Read CORBA object.
   *
   * @return a value, extracted from the stream. May be null
   * if the null was previously written by {@link DataOutputStream#write_Object}.
   */
  org.omg.CORBA.Object read_Object();

  /**
   * Read abstract interface.
   *
   * @return a value, extracted from the stream. May be either CORBA Object or
   * CORBA value type.
   */
  java.lang.Object read_Abstract();

  /**
   * Read the CORBA value type.
   * @return a value, extracted from the stream.
   */
  java.io.Serializable read_Value();

  /**
   * Read typecode.
   * @return a value, extracted from the stream.
   */
  TypeCode read_TypeCode();

  /**
   * Read array of Any's.
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_any_array(AnySeqHolder seq, int offset, int length);

  /**
   * Read boolean array.
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_boolean_array(BooleanSeqHolder seq, int offset, int length);

  /**
   * Read array of narrow (usually 8 bit) chars.
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_char_array(CharSeqHolder seq, int offset, int length);

  /**
   * Read array of wide (usually 16 bit) chars.
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_wchar_array(WCharSeqHolder seq, int offset, int length);

  /**
   * Read array of bytes.
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_octet_array(OctetSeqHolder seq, int offset, int length);

  /**
   * Read array of shorts (16 bit ints).
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_short_array(ShortSeqHolder seq, int offset, int length);

  /**
   * Read array of unsigned shorts (16 bit ints).
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_ushort_array(UShortSeqHolder seq, int offset, int length);

  /**
   * Read array of CORBA longs (java ints).
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_long_array(LongSeqHolder seq, int offset, int length);

  /**
   * Read array of CORBA unsigned longs (java ints).
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_ulong_array(ULongSeqHolder seq, int offset, int length);

  /**
   * Read array of CORBA unsigned long longs (java longs).
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_ulonglong_array(ULongLongSeqHolder seq, int offset, int length);

  /**
   * Read array of CORBA long longs (java longs).
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_longlong_array(LongLongSeqHolder seq, int offset, int length);

  /**
   * Read array of floats.
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_float_array(FloatSeqHolder seq, int offset, int length);

  /**
   * Read array of doubles.
   *
   * The value, extracted from the stream, is returned in the
   * .value field of the passed holder.
   */
  void read_double_array(DoubleSeqHolder seq, int offset, int length);
}
