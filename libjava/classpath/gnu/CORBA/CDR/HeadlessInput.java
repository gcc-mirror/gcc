/* HeadlessInput.java --
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


package gnu.CORBA.CDR;

import gnu.CORBA.Minor;

import org.omg.CORBA.Any;
import org.omg.CORBA.AnySeqHolder;
import org.omg.CORBA.BooleanSeqHolder;
import org.omg.CORBA.CharSeqHolder;
import org.omg.CORBA.Context;
import org.omg.CORBA.DataInputStream;
import org.omg.CORBA.DoubleSeqHolder;
import org.omg.CORBA.FloatSeqHolder;
import org.omg.CORBA.LongLongSeqHolder;
import org.omg.CORBA.LongSeqHolder;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.OctetSeqHolder;
import org.omg.CORBA.Principal;
import org.omg.CORBA.ShortSeqHolder;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.ULongLongSeqHolder;
import org.omg.CORBA.ULongSeqHolder;
import org.omg.CORBA.UShortSeqHolder;
import org.omg.CORBA.WCharSeqHolder;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.InputStream;

import java.io.IOException;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * Substitutes the main stream in factories when the header is already behind.
 * Overrides methods that may be invoked from the factory, forcing not to read
 * the header if called first time on this stream.
 * 
 * This stream reverts to default behavior if one or more call are made (reading
 * value types that are nested fields of the value type).
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class HeadlessInput
  extends org.omg.CORBA_2_3.portable.InputStream
  implements DataInputStream, gnuValueStream
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Indicates that no positional information is available.
   */
  static final int NONE = -1;

  /**
   * If true, this is not the first call.
   */
  public boolean subsequentCalls;

  /**
   * The enclosed stream.
   */
  final BufferredCdrInput stream;

  /**
   * Create an instance, reading from the given buffer.
   * 
   * @param a_stream a stream from where the data will be read.
   * @param inheritSettings a stream from that endian and other settings are
   * inherited.
   */
  public HeadlessInput(BufferredCdrInput a_stream, InputStream inheritSettings)
  {
    stream = a_stream;

    if (inheritSettings instanceof AbstractCdrInput)
      {
        AbstractCdrInput t = (AbstractCdrInput) inheritSettings;
        t.cloneSettings(stream);
      }
    else if (stream.orb() == null)
      stream.setOrb(inheritSettings.orb());

    if (inheritSettings instanceof gnuValueStream
      && stream.getRunTime() == null)
      {
        stream.setRunTime(((gnuValueStream) inheritSettings).getRunTime());
      }
  }

  /**
   * Tries to read using boxed value helper.
   */
  public Serializable read_value(BoxedValueHelper helper)
  {
    if (subsequentCalls)
      return stream.read_value(helper);
    else
      {
        subsequentCalls = true;
        return helper.read_value(this);
      }
  }

  /**
   * Tries to locate a factory using repository id.
   */
  public Serializable read_value(String repository_id)
  {
    if (subsequentCalls)
      return stream.read_value(repository_id);
    else
      {
        subsequentCalls = true;
        Serializable value = Vio.readValue(this, NONE, null,
          null, repository_id, null, null);
        return value;
      }
  }

  /**
   * Try to read when having an unitialised value.
   */
  public Serializable read_value(Serializable value)
  {
    if (subsequentCalls)
      return stream.read_value(value);
    else
      {
        subsequentCalls = true;
        value = Vio.readValue(this, NONE, value, null, null,
          null, null);
        return value;
      }
  }

  /**
   * Try to read when having an unitialised value.
   */
  public Serializable read_value(Class clz)
  {
    if (subsequentCalls)
      return stream.read_value(clz);
    else
      {
        try
          {
            subsequentCalls = true;
            Serializable value = (Serializable) Vio.instantiateAnyWay(clz);
            value = Vio.readValue(this, NONE, value, null, null,
              null, null);
            return value;
          }
        catch (Exception ex)
          {
            MARSHAL m = new MARSHAL("Can't read an instance of "
              + clz.getName());
            m.minor = Minor.Value;
            m.initCause(ex);
            throw m;
          }
      }
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public int available()
    throws IOException
  {
    return stream.available();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void close()
    throws IOException
  {
    stream.close();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void mark(int readlimit)
  {
    stream.mark(readlimit);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public boolean markSupported()
  {
    return stream.markSupported();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public ORB orb()
  {
    return stream.orb();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public Object read_abstract_interface()
  {
    return stream.read_abstract_interface();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public Object read_abstract_interface(Class clz)
  {
    return stream.read_abstract_interface(clz);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public Any read_any()
  {
    return stream.read_any();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_boolean_array(boolean[] value, int offset, int length)
  {
    stream.read_boolean_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public boolean read_boolean()
  {
    return stream.read_boolean();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_char_array(char[] value, int offset, int length)
  {
    stream.read_char_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public char read_char()
  {
    return stream.read_char();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public Context read_Context()
  {
    return stream.read_Context();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_double_array(double[] value, int offset, int length)
  {
    stream.read_double_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public double read_double()
  {
    return stream.read_double();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public BigDecimal read_fixed()
  {
    return stream.read_fixed();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_float_array(float[] value, int offset, int length)
  {
    stream.read_float_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public float read_float()
  {
    return stream.read_float();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_long_array(int[] value, int offset, int length)
  {
    stream.read_long_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public int read_long()
  {
    return stream.read_long();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_longlong_array(long[] value, int offset, int length)
  {
    stream.read_longlong_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public long read_longlong()
  {
    return stream.read_longlong();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public org.omg.CORBA.Object read_Object()
  {
    return stream.read_Object();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public org.omg.CORBA.Object read_Object(Class klass)
  {
    return stream.read_Object(klass);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_octet_array(byte[] value, int offset, int length)
  {
    stream.read_octet_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public byte read_octet()
  {
    return stream.read_octet();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public Principal read_Principal()
  {
    return stream.read_Principal();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_short_array(short[] value, int offset, int length)
  {
    stream.read_short_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public short read_short()
  {
    return stream.read_short();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public String read_string()
  {
    return stream.read_string();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public TypeCode read_TypeCode()
  {
    return stream.read_TypeCode();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_ulong_array(int[] value, int offset, int length)
  {
    stream.read_ulong_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public int read_ulong()
  {
    return stream.read_ulong();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_ulonglong_array(long[] value, int offset, int length)
  {
    stream.read_ulonglong_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public long read_ulonglong()
  {
    return stream.read_ulonglong();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_ushort_array(short[] value, int offset, int length)
  {
    stream.read_ushort_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public short read_ushort()
  {
    return stream.read_ushort();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public Serializable read_value()
  {
    return read_value((Serializable) null);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_wchar_array(char[] value, int offset, int length)
  {
    stream.read_wchar_array(value, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public char read_wchar()
  {
    return stream.read_wchar();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public String read_wstring()
  {
    return stream.read_wstring();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public int read()
    throws IOException
  {
    return stream.read();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public int read(byte[] b, int off, int len)
    throws IOException
  {
    return stream.read(b, off, len);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public int read(byte[] b)
    throws IOException
  {
    return stream.read(b);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void reset()
    throws IOException
  {
    stream.reset();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public long skip(long n)
    throws IOException
  {
    return stream.skip(n);
  }

  /**
   * Get a string representation.
   */
  public String toString()
  {
    return "HeadlessInput+" + stream.toString();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public String[] _truncatable_ids()
  {
    return stream._truncatable_ids();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public Object read_Abstract()
  {
    return stream.read_Abstract();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_any_array(AnySeqHolder holder, int offset, int length)
  {
    stream.read_any_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_boolean_array(BooleanSeqHolder holder, int offset, int length)
  {
    stream.read_boolean_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_char_array(CharSeqHolder holder, int offset, int length)
  {
    stream.read_char_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_double_array(DoubleSeqHolder holder, int offset, int length)
  {
    stream.read_double_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_float_array(FloatSeqHolder holder, int offset, int length)
  {
    stream.read_float_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_long_array(LongSeqHolder holder, int offset, int length)
  {
    stream.read_long_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_longlong_array(LongLongSeqHolder holder, int offset,
    int length)
  {
    stream.read_longlong_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_octet_array(OctetSeqHolder holder, int offset, int length)
  {
    stream.read_octet_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_short_array(ShortSeqHolder holder, int offset, int length)
  {
    stream.read_short_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_ulong_array(ULongSeqHolder holder, int offset, int length)
  {
    stream.read_ulong_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_ulonglong_array(ULongLongSeqHolder holder, int offset,
    int length)
  {
    stream.read_ulonglong_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_ushort_array(UShortSeqHolder holder, int offset, int length)
  {
    stream.read_ushort_array(holder, offset, length);
  }

  /**
   * Delegates functionality to read_value.
   */
  public Serializable read_Value()
  {
    return read_value();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void read_wchar_array(WCharSeqHolder holder, int offset, int length)
  {
    stream.read_wchar_array(holder, offset, length);
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public int getPosition()
  {
    return stream.getPosition();
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public gnuRuntime getRunTime()
  {
    return stream.runtime;
  }

  /**
   * Replace the instance of RunTime.
   */
  public void setRunTime(gnuRuntime a_runtime)
  {
    stream.runtime = a_runtime;
  }

  /**
   * Delegates functionality to the underlying stream.
   */
  public void seek(int position)
  {
    stream.seek(position);
  }

}