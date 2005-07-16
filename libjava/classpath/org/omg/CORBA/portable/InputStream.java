/* InputStream.java --
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

import java.math.BigDecimal;

import org.omg.CORBA.Any;
import org.omg.CORBA.Context;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Principal;
import org.omg.CORBA.TypeCode;

/**
 * This class is used to read CORBA IDL data types.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class InputStream
  extends java.io.InputStream
{
  /**
   * Return the Object Request Broker that has created this stream.
   * @return the ORB. This must be overridden, as the default
   * method always returns null.
   */
  public ORB orb()
  {
    return null;
  }

  /**
   * This should read the CORBA context, but following the 1.4 API
   * specification, it must not be implemented.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Context read_Context()
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Read a CORBA (not java) object
   * @return an object.
   */
  public abstract org.omg.CORBA.Object read_Object();

  /**
   * Read a CORBA <code>char</code>.
   * @return a value, corresponding the value of the CORBA <code>char</code>.
   */
  public abstract char read_char();

  /**
   * Read a CORBA <code>double</code>.
   * @return a value, corresponding the value of the CORBA <code>double</code>.
   */
  public abstract double read_double();

  /**
   * Read a CORBA <code>float</code>.
   * @return a value, corresponding the value of the CORBA <code>float</code>.
   */
  public abstract float read_float();

  /**
   * Read a and array of CORBA float.
   */
  public abstract void read_float_array(float[] value, int offset, int length);

  /**
   * Read a CORBA <code>long</code> that is mapped into java <code>int</code>.
   * @return an integer, corresponding the CORBA <code>long</code>.
   */
  public abstract int read_long();

  /**
   * Read a CORBA <code>long long</code> that is mapped into java <code>
   * long</code>.
   * @return a value, corresponding the CORBA <code>long long</code>
   */
  public abstract long read_longlong();

  /**
   * Read an array of CORBA <code>long long</code>
   */
  public abstract void read_longlong_array(long[] value, int offset, int length);

  /**
   * Read a CORBA <code>octed</code> that is mapped into java <code>byte</code>.
   * @return a byte, corresponding the CORBA <code>octet</code>.
   */
  public abstract byte read_octet();

  /**
   * Read a and array of CORBA octets that are mapped into java array of
   * bytes.
   */
  public abstract void read_octet_array(byte[] value, int offset, int length);

  /**
   * Read a CORBA <code>short</code>.
   * @return a value, corresponding the value of the CORBA <code>short</code>.
   */
  public abstract short read_short();

  public abstract void read_short_array(short[] value, int offset, int length);

  /**
   * Read a CORBA unsigned long that is mapped into java <code>int</code>.
   * @return an integer, matching the CORBA unsigned <code>long</code>.
   */
  public abstract int read_ulong();

  /**
   * Read an array of CORBA unsigned long.
   */
  public abstract void read_ulong_array(int[] value, int offset, int length);

  /**
   * Should read from the stream, but following specification it does not.
   * @throws java.io.IOException
   * @throws NO_IMPLEMENT, always.
   */
  public int read()
           throws java.io.IOException
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Read a TypeCode.
   * @return a TypeCode.
   */
  public abstract TypeCode read_TypeCode();

  /**
   * Read a CORBA <code>boolean</code>.
   * @return a value, corresponding the value of the CORBA <code>boolean</code>.
   */
  public abstract boolean read_boolean();

  /**
   * Read a and array of CORBA boolean values.
   */
  public abstract void read_boolean_array(boolean[] value, int offset,
                                          int length
                                         );

  /**
   * Read a and array of CORBA characters.
   */
  public abstract void read_char_array(char[] value, int offset, int length);

  /**
   * Read a and array of CORBA doubles.
   */
  public abstract void read_double_array(double[] value, int offset, int length);

  /**
   * Read a and array of CORBA longs.
   */
  public abstract void read_long_array(int[] value, int offset, int length);

  /**
   * Read a CORBA <code>string</code> that is mapped into java
   * <code>String</code>.
   * @return a value, corresponding the value of the CORBA
   * <code>string</code>.
   */
  public abstract String read_string();

  /**
   * Read a and array of CORBA unsigned longs.
   */
  public abstract long read_ulonglong();

  /**
   * Read a and array of CORBA unsigned longs.
   */
  public abstract void read_ulonglong_array(long[] value, int offset, int length);

  /**
   * Read a CORBA unsigned short that is mapped into java <code>short</code>.
   * @return a value of the CORBA unsigned <code>short</code>.
   */
  public abstract short read_ushort();

  /**
   * Read a and array of CORBA unsigned shorts.
   */
  public abstract void read_ushort_array(short[] value, int offset, int length);

  /**
   * Read a CORBA object that is an instance of the class, passed
   * as an argument. This method is not abstract, but following the
   * specifications it must not be implemented.
   *
   * @param klass a CORBA class
   * @throws NO_IMPLEMENT, always.
   */
  public Object read_Object(Class klass)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Read a CORBA <code>Any</code>.
   * @return an <code>Any</code>.
   */
  public abstract Any read_any();

  /**
   * Read a CORBA <code>fixed</code>. This method is not abstract,
   * but following the specifications it must not be implemented.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public BigDecimal read_fixed()
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Read a CORBA <code>wchar</code> that is mapped into java <code>char</code>.
   * @return a value, corresponding the value of the CORBA <code>wchar</code>.
   */
  public abstract char read_wchar();

  public abstract void read_wchar_array(char[] value, int offset, int length);

  /**
   * Read a CORBA <code>wstring</code> that is mapped into
   * java <code>String</code>.
   * @return a string, corresponding CORBA <code>wstring</code>.
   */
  public abstract String read_wstring();

  /**
   * Read a CORBA <code>Principal</code>.
   * @deprecated by CORBA 2.2
   * @return a Principal.
   */
  public Principal read_Principal()
  {
    throw new NO_IMPLEMENT();
  }
}
