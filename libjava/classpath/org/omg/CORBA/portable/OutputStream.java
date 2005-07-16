/* OutputStream.java --
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

import org.omg.CORBA.Any;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Principal;
import org.omg.CORBA.TypeCode;

import java.io.IOException;

import java.math.BigDecimal;

/**
 * This class is used to write CORBA IDL data types.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class OutputStream
  extends java.io.OutputStream
{
  /**
   * Returns an input stream with the same buffer.
   * @return an input stream
   */
  public abstract InputStream create_input_stream();

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
   * Should write an byte (lower 8 bits) to the output stream, but,
   * following specification, it does not and
   * must be overridden to get a functionality.
   *
   * @param n an integer to write.
   *
   * @throws NO_IMPLEMENT, always.
   * @throws IOException in overriden methods.
   */
  public void write(int n)
             throws IOException
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Should write a CORBA context to the output stream, but,
   * following the 1.4 specification, it does not and
   * must be overridden to get a functionality.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public void write_Context(Context context, ContextList contexts)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Write CORBA (not java) Object.
   */
  public abstract void write_Object(org.omg.CORBA.Object x);

  /**
   * Should write a principal to the output stream, but,
   * following specification, it does not and
   * must be overridden to get a functionality.
   *
   * @deprecated by CORBA 2.2
   *
   * @param principal a Principal to write
   *
   * @throws NO_IMPLEMENT, always.
   */
  public void write_Principal(Principal principal)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Write TypeCode.
   */
  public abstract void write_TypeCode(TypeCode x);

  /**
   * Write CORBA <code>Any</code>.
   */
  public abstract void write_any(Any x);

  /**
   * Write CORBA <code>boolean</code>.
   */
  public abstract void write_boolean(boolean x);

  /**
   * Write CORBA <code>booelan[]</code>.
   */
  public abstract void write_boolean_array(boolean[] x, int ofs, int len);

  /**
   * Write CORBA <code>char</code>.
   */
  public abstract void write_char(char x);

  /**
   * Write CORBA <code>char[]</code>.
   */
  public abstract void write_char_array(char[] chars, int offset, int length);

  /**
   * Write CORBA <code>double</code>.
   */
  public abstract void write_double(double x);

  /**
   * Write CORBA <code>double[]</code>.
   */
  public abstract void write_double_array(double[] x, int ofs, int len);

  /**
   * Should write a BigDecimal number, but, following specification,
   * it does not and must be overridden to get a functionality.
   *
   * @param fixed a number to write
   * @throws NO_IMPLEMENT, always.
   */
  public void write_fixed(BigDecimal fixed)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Write CORBA <code>float</code>.
   */
  public abstract void write_float(float x);

  /**
   * Write CORBA <code>float[]</code>.
   */
  public abstract void write_float_array(float[] x, int ofs, int len);

  /**
  * Write CORBA <code>long</code> that is mapped into java <code>int</code>.
  */
  public abstract void write_long(int x);

  /**
   * Write CORBA <code>long[]</code>.
   */
  public abstract void write_long_array(int[] x, int ofs, int len);

  /**
   * Write CORBA <code>long long</code> that is mapped into
   * java <code>long</code>.
   */
  public abstract void write_longlong(long x);

  /**
   * Write CORBA <code>long long []</code>.
   */
  public abstract void write_longlong_array(long[] x, int ofs, int len);

  /**
   * Write CORBA <code>octed</code> that is mapped into java <code>byte</code>
   */
  public abstract void write_octet(byte x);

  /**
   * Write CORBA <code>octet[]</code>.
   */
  public abstract void write_octet_array(byte[] x, int ofs, int len);

  /**
   * Write CORBA <code>short</code>.
   */
  public abstract void write_short(short x);

  /**
   * Write CORBA <code>short[]</code>.
   */
  public abstract void write_short_array(short[] x, int ofs, int len);

  /**
   * Write CORBA <code>string</code>.
   */
  public abstract void write_string(String x);

  /**
   * Write unsigned CORBA <code>long</code> that is mapped into
   * java <code>int</code>.
   */
  public abstract void write_ulong(int x);

  /**
   * Write array of CORBA unsigned longs.
   */
  public abstract void write_ulong_array(int[] x, int ofs, int len);

  /**
   * Write unsigned CORBA <code>long long </code> that is mapped into
   * java <code>long</code>.
   */
  public abstract void write_ulonglong(long x);

  /**
   * Write array of unsigned CORBA long-longs.
   */
  public abstract void write_ulonglong_array(long[] x, int ofs, int len);

  /**
   * Write unsigned CORBA <code>short</code> that is mapped into
   * java <code>short</code>.
   */
  public abstract void write_ushort(short x);

  /**
   * Write array of unsigned CORBA shorts.
   */
  public abstract void write_ushort_array(short[] x, int ofs, int len);

  /**
   * Write CORBA <code>wchar</code> that is mapped into
   * java <code>char</code>.
   */
  public abstract void write_wchar(char x);

  /**
   * Write array of CORBA wchars.
   */
  public abstract void write_wchar_array(char[] chars, int offset, int length);

  /**
   * Write CORBA <code>wstring</code> that is mapped into
   * java <code>string</code>.
   */
  public abstract void write_wstring(String x);
}
