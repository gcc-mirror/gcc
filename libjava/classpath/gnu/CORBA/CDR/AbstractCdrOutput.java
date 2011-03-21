/* AbstractCdrOutput.java --
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

import gnu.CORBA.BigDecimalHelper;
import gnu.CORBA.IOR;
import gnu.CORBA.IorProvider;
import gnu.CORBA.Minor;
import gnu.CORBA.TypeCodeHelper;
import gnu.CORBA.Unexpected;
import gnu.CORBA.Version;
import gnu.CORBA.GIOP.CharSets_OSF;
import gnu.CORBA.GIOP.CodeSetServiceContext;
import gnu.CORBA.typecodes.PrimitiveTypeCode;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DataInputStream;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.UserException;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * A simple CORBA CDR (common data representation)
 * output stream, writing data into the
 * given {@link java.io.OutputStream}.
 *
 * The same class also implements the {@link DataInputStream},
 * providing support for writing the value type objects
 * in a user defined way.
 *
 * TODO This class uses 16 bits per Unicode character only, as it was until
 * jdk 1.4 inclusive.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class AbstractCdrOutput
  extends org.omg.CORBA_2_3.portable.OutputStream
  implements org.omg.CORBA.DataOutputStream
{
  /**
   * The runtime, associated with this stream. This field is only used when
   * reading and writing value types and filled-in in gnu.CORBA.CDR.Vio.
   */
  public transient gnuRuntime runtime;

  /**
   * This instance is used to convert primitive data types into the
   * byte sequences.
   */
  protected AbstractDataOutput b;

  /**
   * The associated orb, if any.
   */
  protected ORB orb;

  /**
   * The GIOP version.
   */
  protected Version giop = new Version(1, 2);

  /**
   * The code set information.
   */
  protected CodeSetServiceContext codeset;

  /**
   * The name of the currently used narrow charset.
   */
  private String narrow_charset;

  /**
   * The name of the currently used wide charset, null if
   * the native wide charset is used.
   */
  private String wide_charset;

  /**
   * True if the native code set is used for narrow characters.
   * If the set is native, no the intermediate Reader object
   * is instantiated when writing characters.
   */
  private boolean narrow_native;

  /**
   * True if the native code set is used for wide characters.
   * If the set is native, no the intermediate Reader object
   * is instantiated when writing characters.
   */
  private boolean wide_native;

  /**
   * If true, the Little Endian encoding is used to write the
   * data. Otherwise, the Big Endian encoding is used.
   */
  private boolean little_endian;

  /**
   * The stream whre the data are actually written.
   */
  private java.io.OutputStream actual_stream;

  /**
   * Creates the stream.
   *
   * @param writeTo a stream to write CORBA output to.
   */
  public AbstractCdrOutput(java.io.OutputStream writeTo)
  {
    setOutputStream(writeTo);
    setCodeSet(CodeSetServiceContext.STANDARD);
  }

  /**
   * Creates the stream, requiring the subsequent call
   * of {@link #setOutputStream(java.io.OutputStream)}.
   */
  public AbstractCdrOutput()
  {
    setCodeSet(CodeSetServiceContext.STANDARD);
  }

  /**
   * Set the alignment offset, if the index of the first byte in the
   * stream is different from 0.
   */
  public abstract void setOffset(int an_offset);

  /**
   * Clone all important settings to another stream.
   */
  public void cloneSettings(AbstractCdrOutput stream)
  {
    stream.setBigEndian(!little_endian);
    stream.setCodeSet(getCodeSet());
    stream.setVersion(giop);
    stream.setOrb(orb);
  }

  /**
   * Set the current code set context.
   */
  public void setCodeSet(CodeSetServiceContext a_codeset)
  {
    this.codeset = a_codeset;
    narrow_charset = CharSets_OSF.getName(codeset.char_data);
    wide_charset = CharSets_OSF.getName(codeset.wide_char_data);

    narrow_native = CharSets_OSF.NATIVE_CHARACTER == codeset.char_data;
    wide_native = CharSets_OSF.NATIVE_WIDE_CHARACTER == codeset.wide_char_data;
  }

  /**
   * Get the current code set context.
   */
  public CodeSetServiceContext getCodeSet()
  {
    return codeset;
  }

  /**
   * Set the orb, associated with this stream.
   * @param an_orb
   */
  public void setOrb(ORB an_orb)
  {
    orb = an_orb;
  }

  /**
   * Set the output stream that receives the CORBA output.
   *
   * @param writeTo the stream.
   */
  public void setOutputStream(java.io.OutputStream writeTo)
  {
    if (little_endian)
      b = new LittleEndianOutputStream(writeTo);
    else
      b = new BigEndianOutputStream(writeTo);

    actual_stream = writeTo;
  }

  /**
   * Set the GIOP version. Some data types are written differently
   * for the different versions. The default version is 1.0 .
   */
  public void setVersion(Version giop_version)
  {
    giop = giop_version;
  }

  /**
   * Specify if the stream should use the Big Endian (usual for java)
   * or Little Encoding. The default is Big Endian.
   *
   * @param use_big_endian if true, use Big Endian, if false,
   * use Little Endian.
   */
  public void setBigEndian(boolean use_big_endian)
  {
    little_endian = !use_big_endian;
    setOutputStream(actual_stream);
  }

  /**
   * Align the curretn position at the given natural boundary.
   */
  public abstract void align(int boundary);

  /**
   * Create the encapsulation stream, associated with the current
   * stream. The encapsulated stream must be closed. When being
   * closed, the encapsulation stream writes its buffer into
   * this stream using the CORBA CDR encapsulation rules.
   *
   * It is not allowed to write to the current stream directly
   * before the encapsulation stream is closed.
   *
   * The encoding (Big/Little Endian) inside the encapsulated
   * sequence is the same as used into the parent stream.
   *
   * @return the encapsulated stream.
   */
  public AbstractCdrOutput createEncapsulation()
  {
    return new EncapsulationStream(this, !little_endian);
  }

  /**
   * Return the associated {@link ORB}.
   * @return the associated {@link ORB} or null is no such is set.
   */
  public ORB orb()
  {
    return orb;
  }

  /**
   * Write a single byte.
   * @param n byte to write (low 8 bits are written).
   */
  public void write(int n)
  {
    try
      {
        b.write(n);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Write bytes directly into the underlying stream.
   */
  public void write(byte[] x)
             throws java.io.IOException
  {
    b.write(x);
  }

  /**
   * Write bytes directly into the underlying stream.
   */
  public void write(byte[] x, int ofs, int len)
             throws java.io.IOException
  {
    b.write(x, ofs, len);
  }

  /**
   * Following the specification, this is not implemented.
   * Override to get the functionality.
   */
  public void write_Context(Context context, ContextList contexts)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Read the CORBA object. The object is written form of the plain (not a
   * string-encoded) IOR profile without the heading endian indicator. The
   * responsible method for reading such data is {@link IOR.write_no_endian}.
   *
   * The null value is written as defined in OMG specification (zero length
   * string, followed by an empty set of profiles).
   */
  public void write_Object(org.omg.CORBA.Object x)
  {
    ORB w_orb = orb;
    if (x instanceof IorProvider)
      {
        ((IorProvider) x).getIor()._write_no_endian(this);
        return;
      }
    else if (x == null)
      {
        IOR.write_null(this);
        return;
      }
    else if (x instanceof ObjectImpl)
      {
        Delegate d = ((ObjectImpl) x)._get_delegate();

        if (d instanceof IorProvider)
          {
            ((IorProvider) d).getIor()._write_no_endian(this);
            return;
          }
        else
          {
            ORB d_orb = d.orb(x);
            if (d_orb != null)
              w_orb = d_orb;
          }
      }

    // Either this is not an ObjectImpl or it has the
    // unexpected delegate. Try to convert via ORBs
    // object_to_string().
    if (w_orb != null)
      {
        IOR ior = IOR.parse(w_orb.object_to_string(x));
        ior._write_no_endian(this);
        return;
      }
    else
      throw new BAD_OPERATION(
        "Please set the ORB for this stream, cannot write "
          + x.getClass().getName());
  }

  /**
   * Write the TypeCode. This implementation delegates functionality
   * to {@link cdrTypeCode}.
   *
   * @param x a TypeCode to write.
   */
  public void write_TypeCode(TypeCode x)
  {
    try
      {
        TypeCodeHelper.write(this, x);
      }
    catch (UserException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes an instance of the CORBA {@link Any}.
   * This method writes the typecode, followed
   * by value itself. In Any contains null
   * (value not set), the {@link TCKind#tk_null}
   * is written.
   *
   * @param x the {@link Any} to write.
   */
  public void write_any(Any x)
  {
    Streamable value = x.extract_Streamable();
    if (value != null)
      {
        write_TypeCode(x.type());
        value._write(this);
      }
    else
      {
        PrimitiveTypeCode p = new PrimitiveTypeCode(TCKind.tk_null);
        write_TypeCode(p);
      }
  }

  /**
   * Writes a single byte, 0 for <code>false</code>,
   * 1 for <code>true</code>.
   *
   * @param x the value to write
   */
  public void write_boolean(boolean x)
  {
    try
      {
        b.write(x ? 1 : 0);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the boolean array.
   *
   * @param x array
   * @param ofs offset
   * @param len length.
   */
  public void write_boolean_array(boolean[] x, int ofs, int len)
  {
    try
      {
        for (int i = ofs; i < ofs + len; i++)
          {
            b.write(x [ i ] ? 1 : 0);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the lower byte of the passed parameter.
   * @param x the char to write
   *
   * It is effective to write more characters at once.
   */
  public void write_char(char x)
  {
    try
      {
        if (narrow_native)
          b.write(x);
        else
          {
            OutputStreamWriter ow =
              new OutputStreamWriter((OutputStream) b, narrow_charset);
            ow.write(x);
            ow.flush();
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the lower bytes of the passed array members.
   *
   * @param chars an array
   * @param offset offset
   * @param length length
   */
  public void write_char_array(char[] chars, int offset, int length)
  {
    try
      {
        if (narrow_native)
          {
            for (int i = offset; i < offset + length; i++)
              {
                b.write(chars [ i ]);
              }
          }
        else
          {
            OutputStreamWriter ow =
              new OutputStreamWriter((OutputStream) b, narrow_charset);
            ow.write(chars, offset, length);
            ow.flush();
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the double value (IEEE 754 format).
   */
  public void write_double(double x)
  {
    try
      {
        align(8);
        b.writeDouble(x);
      }
    catch (Exception ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the array of double values.
   */
  public void write_double_array(double[] x, int ofs, int len)
  {
    try
      {
        align(8);
        for (int i = ofs; i < ofs + len; i++)
          {
            b.writeDouble(x [ i ]);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes CORBA fixed, storing all digits but not the scale.
   * The end of the record on <code>fixed</code> can
   * be determined from its last byte.
   */
  public void write_fixed(BigDecimal fixed)
  {
    try
      {
        BigDecimalHelper.write(this, fixed);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
    catch (BadKind ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Write the float value (IEEE 754 format).
   */
  public void write_float(float x)
  {
    try
      {
        align(4);
        b.writeFloat(x);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   *  Writes an array of the float values.
   */
  public void write_float_array(float[] x, int ofs, int len)
  {
    try
      {
        align(4);
        for (int i = ofs; i < ofs + len; i++)
          {
            b.writeFloat(x [ i ]);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the integer value (CORBA long, four bytes, high byte first).
   * @param x the value to write.
   */
  public void write_long(int x)
  {
    try
      {
        align(4);
        b.writeInt(x);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the array of integer (CORBA long) values.
   *
   * @param x value
   * @param ofs offset
   * @param len length
   */
  public void write_long_array(int[] x, int ofs, int len)
  {
    try
      {
        align(4);
        for (int i = ofs; i < ofs + len; i++)
          {
            b.writeInt(x [ i ]);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the long (CORBA long long) value, 8 bytes,
   * high byte first.
   *
   * @param x the value to write.
   */
  public void write_longlong(long x)
  {
    try
      {
        align(8);
        b.writeLong(x);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the array of longs (CORBA long longs) values.
   *
   * @param x value
   * @param ofs offset
   * @param len length
   */
  public void write_longlong_array(long[] x, int ofs, int len)
  {
    try
      {
        align(8);
        for (int i = ofs; i < ofs + len; i++)
          {
            b.writeLong(x [ i ]);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes this byte.
   * @param x
   */
  public void write_octet(byte x)
  {
    try
      {
        b.writeByte(x);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the array of bytes (CORBA octets) values.
   *
   * @param x value
   * @param ofs offset
   * @param len length
   */
  public void write_octet_array(byte[] x, int ofs, int len)
  {
    try
      {
        b.write(x, ofs, len);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes first the size of array, and then the byte array using
   * the {@link java.io.OutputStream#write(byte[]) }. The sequence
   * being written is preceeded by the int, representing the array
   * length.
   */
  public void write_sequence(byte[] buf)
  {
    try
      {
        write_long(buf.length);
        write(buf);
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.CDR;
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Writes the contents of the provided stream.
   * The sequence being written is preceeded by the int,
   * representing the stream buffer length (the number of
   * bytes being subsequently written).
   */
  public void write_sequence(BufferedCdrOutput from)
  {
    try
      {
        write_long(from.buffer.size());
        from.buffer.writeTo(this);
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.CDR;
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Writes the two byte integer (short), high byte first.
   *
   * @param x the integer to write.
   */
  public void write_short(short x)
  {
    try
      {
        align(2);
        b.writeShort(x);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the array of short (two byte integer) values.
   *
   * @param x value
   * @param ofs offset
   * @param len length
   */
  public void write_short_array(short[] x, int ofs, int len)
  {
    try
      {
        align(2);
        for (int i = ofs; i < ofs + len; i++)
          {
            b.writeShort(x [ i ]);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the string. This implementation first calls
   * String.getBytes() and then writes the length of the returned
   * array (as CORBA ulong) and the returned array itself.
   *
   * The encoding information, if previously set, is taken
   * into consideration.
   *
   * @param x the string to write.
   */
  public void write_string(String x)
  {
    try
      {
        byte[] ab = x.getBytes(narrow_charset);
        write_long(ab.length + 1);
        write(ab);

        // write null terminator.
        write(0);
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the CORBA unsigned long in the same way as CORBA long.
   */
  public void write_ulong(int x)
  {
    write_long(x);
  }

  /**
   * Writes the array of CORBA unsigned longs in the same way as
   * array of ordinary longs.
   */
  public void write_ulong_array(int[] x, int ofs, int len)
  {
    write_long_array(x, ofs, len);
  }

  /**
   * Write the unsigned long long in the same way as an ordinary long long.
   *
   * @param x a value to write.
   */
  public void write_ulonglong(long x)
  {
    write_longlong(x);
  }

  /**
   * Write the array of unsingel long longs in the same way
   * an an array of the ordinary long longs.
   */
  public void write_ulonglong_array(long[] x, int ofs, int len)
  {
    write_longlong_array(x, ofs, len);
  }

  /**
   * Write the unsigned short in the same way as an ordinary short.
   */
  public void write_ushort(short x)
  {
    write_short(x);
  }

  /**
   * Write an array of unsigned short integersin the same way
   * as an array of ordinary short integers.
   */
  public void write_ushort_array(short[] x, int ofs, int len)
  {
    write_short_array(x, ofs, len);
  }

  /**
   * Writes the character as two byte short integer (Unicode value), high byte
   * first. Writes in Big Endian, but never writes the endian indicator.
   *
   * The character is always written using the native UTF-16BE charset because
   * its size under arbitrary encoding is not evident.
   */
  public void write_wchar(char x)
  {
    try
      {
        if (giop.until_inclusive(1, 1))
          {
            align(2);

            if (wide_native)
              b.writeShort(x);
            else
              {
                OutputStreamWriter ow = new OutputStreamWriter(
                  (OutputStream) b, wide_charset);
                ow.write(x);
                ow.flush();
              }
          }
        else if (wide_native)
          {
            b.writeByte(2);
            b.writeChar(x);
          }
        else
          {
            String encoded = new String(new char[] { x });
            byte[] bytes = encoded.getBytes(wide_charset);
            b.write(bytes.length + 2);
            b.write(bytes);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Write the array of wide chars.
   *
   * @param chars the array of wide chars
   * @param offset offset
   * @param length length
   *
   * The char array is always written using the native UTF-16BE charset because
   * the character size under arbitrary encoding is not evident.
   */
  public void write_wchar_array(char[] chars, int offset, int length)
  {
    try
      {
        if (giop.until_inclusive(1, 1))
          align(2);

        if (wide_native)
          {
            for (int i = offset; i < offset + length; i++)
              {
                b.writeShort(chars [ i ]);
              }
          }
        else
          {
            OutputStreamWriter ow =
              new OutputStreamWriter((OutputStream) b, wide_charset);
            ow.write(chars, offset, length);
            ow.flush();
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Writes the length of the string in bytes (not characters) and
   * then all characters as two byte unicode chars. Adds the
   * Big Endian indicator, 0xFFFE, at the beginning and null wide char at
   * the end.
   *
   * @param x the string to write.
   */
  public void write_wstring(String x)
  {
    try
      {
        if (giop.since_inclusive(1, 2))
          {
            byte[] bytes = x.getBytes(wide_charset);
            write_sequence(bytes);
          }
        else
          {
            // Encoding with null terminator always in UTF-16.
            // The wide null terminator needs extra two bytes.
            write_long(2 * x.length() + 2);

            for (int i = 0; i < x.length(); i++)
              {
                b.writeShort(x.charAt(i));
              }

            // Write null terminator.
            b.writeShort(0);
          }
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /** {@inheritDoc} */
  public void write_any_array(Any[] anys, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      {
        write_any(anys [ i ]);
      }
  }

  public String[] _truncatable_ids()
  {
    /**@todo Implement this org.omg.CORBA.portable.ValueBase abstract method*/
    throw new java.lang.UnsupportedOperationException("Method _truncatable_ids() not yet implemented.");
  }

  /** {@inheritDoc} */
  public void write_Abstract(java.lang.Object value)
  {
    write_abstract_interface(value);
  }

  /** {@inheritDoc} */
  public void write_Value(Serializable value)
  {
    write_value(value);
  }
}
