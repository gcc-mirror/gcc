/* cdrInput.java --
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
import gnu.CORBA.Functional_ORB;
import gnu.CORBA.GIOP.CharSets_OSF;
import gnu.CORBA.GIOP.cxCodeSet;
import gnu.CORBA.IOR;
import gnu.CORBA.IOR_Delegate;
import gnu.CORBA.TypeCodeHelper;
import gnu.CORBA.Unexpected;
import gnu.CORBA.Version;
import gnu.CORBA.gnuAny;
import gnu.CORBA.stubFinder;

import org.omg.CORBA.Any;
import org.omg.CORBA.AnySeqHolder;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BooleanSeqHolder;
import org.omg.CORBA.CharSeqHolder;
import org.omg.CORBA.DoubleSeqHolder;
import org.omg.CORBA.FloatSeqHolder;
import org.omg.CORBA.LongLongSeqHolder;
import org.omg.CORBA.LongSeqHolder;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.OctetSeqHolder;
import org.omg.CORBA.ShortSeqHolder;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.TypeCodePackage.Bounds;
import org.omg.CORBA.ULongLongSeqHolder;
import org.omg.CORBA.ULongSeqHolder;
import org.omg.CORBA.UShortSeqHolder;
import org.omg.CORBA.WCharSeqHolder;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Serializable;

import java.math.BigDecimal;

/**
 * A simple CORBA CDR (common data representation)
 * input stream, reading data from the
 * given {@link java.io.InputStream}. The primitive types
 * are aligned on they natural boundaries by implementing the
 * abstract method {@link #align(int boundary)}.
 *
 * The same class also implements {@link org.omg.CORBA.DataInputStream} to
 * read the object content in a user defined way.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class cdrInput
  extends org.omg.CORBA_2_3.portable.InputStream
  implements org.omg.CORBA.DataInputStream
{
  /**
   * The message, explaining that the exception has been thrown due
   * unexpected end of the input stream. This usually happens the
   * server and client disagree on communication or data representation
   * rules.
   */
  protected static final String UNEXP_EOF = "Unexpected end of stream";

  /**
   * This instance is used to convert primitive data types into the
   * byte sequences.
   */
  protected abstractDataInputStream b;

  /**
   * The input stream, from where the data are actually
   * being read.
   */
  protected java.io.InputStream actual_stream;

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
  protected cxCodeSet codeset = cxCodeSet.STANDARD;

  /**
   * The name of the currently used narrow charset, null if
   * the native narrow charset is used.
   */
  private String narrow_charset = null;

  /**
   * The name of the currently used wide charset, null if
   * the native wide charset is used.
   */
  private String wide_charset = null;

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
   * If true, the stream expect
   * the multi-byte data in the form "less significant byte
   * first" (Little Endian). This is the opposite to the
   * java standard (Big Endian).
   */
  private boolean little_endian;

  /**
   * Creates the stream. The stream reads Big Endian by
   * default.
   *
   * @param readFrom a stream to read CORBA input from.
   */
  public cdrInput(java.io.InputStream readFrom)
  {
    setInputStream(readFrom);
    setCodeSet(cxCodeSet.STANDARD);
  }

  /**
   * Creates the stream, requiring the subsequent call
   * of {@link #setInputStream(java.io.InputStream)}.
   */
  public cdrInput()
  {
    setCodeSet(cxCodeSet.STANDARD);
  }

  /**
   * Set the Big Endian or Little Endian encoding.
   * The stream reads Big Endian by default.
   *
   * @param use_little_endian if true, the stream expect
   * the multi-byte data in the form "less significant byte
   * first" (Little Endian). This is the opposite to the
   * java standard (Big Endian).
   */
  public void setBigEndian(boolean use_big_endian)
  {
    little_endian = !use_big_endian;
    setInputStream(actual_stream);
  }

  /**
   * Set the input stream that receives the CORBA input.
   *
   * @param readFrom the stream.
   */
  public void setInputStream(java.io.InputStream readFrom)
  {
    if (little_endian)
      b = new LittleEndianInputStream(readFrom);
    else
      b = new BigEndianInputStream(readFrom);

    actual_stream = readFrom;
  }

  /**
   * Set the alignment offset, if the index of the first byte in the
   * stream is different from 0.
   */
  public abstract void setOffset(int offset);

  /**
   * Set the orb, associated with this stream.
   * @param an_orb
   */
  public void setOrb(ORB an_orb)
  {
    orb = an_orb;
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
   * Align the curretn position at the given natural boundary.
   */
  public abstract void align(int boundary);

  /**
   * Reads the CORBA unsigned long (java int), returning the
   * value in the sufficiently large java long.
   */
  public long gnu_read_ulong()
  {
    try
      {
        long l = b.readInt();
        l &= 0xFFFFFFF;
        return l;
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }
    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the unsigned short integer value and return it as java
   * int, sufficiently large to hold all values.
   */
  public int gnu_read_ushort()
  {
    try
      {
        align(2);
        return b.readUnsignedShort();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
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
   * Read a single byte directly from the buffer.
   */
  public int read()
           throws java.io.IOException
  {
    try
      {
        return b.read();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Read bytes directly from the buffer.
   */
  public int read(byte[] x, int ofs, int len)
           throws java.io.IOException
  {
    try
      {
        return b.read(x, ofs, len);
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Read bytes directly from the buffer.
   */
  public int read(byte[] x)
           throws java.io.IOException
  {
    try
      {
        return b.read(x);
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Read the CORBA object. The object to read is represented in the
   * form of the plain (not a string-encoded) IOR profile without the
   * heading endian indicator. The responsible method for reading such
   * data is {@link IOR.read_no_endian}.
   *
   * The returned object is usually casted into the given type using
   * the .narrow method of its helper, despite in some cases the direct
   * cast would also work.
   *
   * The null objects are recognised from the empty profile set.
   * For such objects, null is returned.
   *
   * @return the loaded and constructed object.
   */
  public org.omg.CORBA.Object read_Object()
  {
    try
      {
        IOR ior = new IOR();
        ior._read_no_endian(this);

        if (ior.Id == null)
          return null;

        // Check maybe this is a remote reference to the local object.
        // This is only possible if we access the repository of the
        // connected object.
        if (orb instanceof Functional_ORB)
          {
            Functional_ORB forb = (Functional_ORB) orb;
            org.omg.CORBA.Object local = forb.find_local_object(ior);
            if (local != null)
              return local;
          }

        // Search for the available stubs.
        ObjectImpl impl = stubFinder.search(orb, ior);
        try
          {
            if (impl._get_delegate() == null)
              impl._set_delegate(new IOR_Delegate(orb, ior));
          }
        catch (BAD_OPERATION ex)
          {
            // Some colaborants may throw this exception
            // in response to the attempt to get the unset delegate.
            impl._set_delegate(new IOR_Delegate(orb, ior));
          }

        return impl;
      }
    catch (IOException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION();
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * Read the type code. The type code format is defined in the
   * CORBA documenation.
   */
  public TypeCode read_TypeCode()
  {
    try
      {
        return TypeCodeHelper.read(this);
      }

    catch (Bounds ex)
      {
        throw new Unexpected();
      }
    catch (BadKind ex)
      {
        throw new Unexpected();
      }
  }

  /**
   * Read the CORBA {@link Any}. This method first reads the
   * type code, then delegates the functionality
   * to {@link Any#read_value}.
   */
  public Any read_any()
  {
    TypeCode ty = read_TypeCode();
    gnuAny any = new gnuAny();
    any.read_value(this, ty);
    return any;
  }

  /**
   * Read the boolean, treating any non zero byte as true,
   * zero byte as false.
   */
  public boolean read_boolean()
  {
    try
      {
        return b.read() == 0 ? false : true;
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }
    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the array of boolean.
   */
  public void read_boolean_array(boolean[] x, int offs, int len)
  {
    try
      {
        for (int i = offs; i < offs + len; i++)
          {
            x [ i ] = b.read() == 0 ? false : true;
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read a character using narrow charset encoding. Depending form
   * which encoding is set, this still can be Unicode or ever wider.
   */
  public char read_char()
  {
    try
      {
        if (narrow_native)
          return (char) b.read();
        else
          return (char) new InputStreamReader((InputStream) b, narrow_charset).read();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read a character array, using narrow charset encoding.
   */
  public void read_char_array(char[] x, int offset, int length)
  {
    try
      {
        if (narrow_native)
          {
            for (int i = offset; i < offset + length; i++)
              x [ i ] = (char) b.read();
          }
        else
          {
            InputStreamReader reader =
              new InputStreamReader((InputStream) b, narrow_charset);
            reader.read(x, offset, length);
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the double value, IEEE 754 format.
   */
  public double read_double()
  {
    try
      {
        align(8);
        return b.readDouble();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected();
      }
  }

  /**
   * Read the array of double values, IEEE 754 format.
   */
  public void read_double_array(double[] x, int offs, int len)
  {
    try
      {
        align(8);
        for (int i = offs; i < offs + len; i++)
          {
            x [ i ] = b.readDouble();
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the encapsulated stream.
   * If the encapsulated sequence appears to be in the
   * Little endian format, the flag of the returned stream
   * is set to read Little endian.
   */
  public cdrBufInput read_encapsulation()
  {
    try
      {
        int l = read_long();

        byte[] r = new byte[ l ];
        int n = 0;
        reading:
        while (n < r.length)
          {
            n += read(r, n, r.length - n);
          }

        cdrBufInput capsule = new cdrBufInput(r);
        capsule.setOrb(orb);

        int endian = capsule.read_octet();

        if (endian != 0)
          {
            capsule.setBigEndian(false);
          }

        return capsule;
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the CORBA fixed (the end of the <code>fixed</code>
   * can be determined by its last byte). The scale is always
   * assumed to be zero.
   */
  public BigDecimal read_fixed()
  {
    try
      {
        return BigDecimalHelper.read(this, 0);
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the float value, IEEE 754 format.
   */
  public float read_float()
  {
    try
      {
        align(4);
        return b.readFloat();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read an array of float values, IEEE 754 format.
   */
  public void read_float_array(float[] x, int offs, int len)
  {
    try
      {
        align(4);
        for (int i = offs; i < offs + len; i++)
          {
            x [ i ] = b.readFloat();
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the CORBA long (java int), high byte first.
   */
  public int read_long()
  {
    try
      {
        align(4);
        return b.readInt();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read an array of CORBA longs (java ints).
   */
  public void read_long_array(int[] x, int offs, int len)
  {
    try
      {
        align(4);
        for (int i = offs; i < offs + len; i++)
          {
            x [ i ] = b.readInt();
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the CORBA long long (java long).
   */
  public long read_longlong()
  {
    try
      {
        align(8);
        return b.readLong();
      }
    catch (EOFException ex)
      {
        throw new MARSHAL(UNEXP_EOF);
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read an array of CORBA long longs (java longs).
   */
  public void read_longlong_array(long[] x, int offs, int len)
  {
    try
      {
        align(8);
        for (int i = offs; i < offs + len; i++)
          {
            x [ i ] = b.readLong();
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read a single byte.
   */
  public byte read_octet()
  {
    try
      {
        return b.readByte();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the byte array.
   */
  public void read_octet_array(byte[] x, int offs, int len)
  {
    try
      {
        b.read(x, offs, len);
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the length of the byte array as CORBA long and then
   * the array itseld.
   */
  public byte[] read_sequence()
  {
    try
      {
        int l = read_long();
        byte[] b = new byte[ l ];
        if (l > 0)
          {
            read(b);
          }
        return b;
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the CORBA short integer.
   */
  public short read_short()
  {
    try
      {
        align(2);
        return b.readShort();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read the array of CORBA short integer values.
   */
  public void read_short_array(short[] x, int offs, int len)
  {
    try
      {
        align(2);
        for (int i = offs; i < offs + len; i++)
          {
            x [ i ] = b.readShort();
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Read a singe byte string. The method firs reads the
   * byte array and then calls a constructor to create a
   * string from this array. The character encoding, if
   * previously set, is taken into consideration.
   *
   * @return a loaded string.
   */
  public String read_string()
  {
    try
      {
        align(4);

        int n = b.readInt();
        byte[] s = new byte[ n ];
        b.read(s);

        // Discard the null terminator.
        if (narrow_charset == null)
          return new String(s, 0, n - 1);
        else
          return new String(s, 0, n - 1, narrow_charset);
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected();
      }
  }

  /**
   * Reads the CORBA unsigned long (java int), delegating
   * functionality to {@link #read_long}.
   */
  public int read_ulong()
  {
    return read_long();
  }

  /**
   * Reads the array of CORBA unsigned long (java integer) values,
   * delegating functionality to
   * {@link #real_long_array}.
   */
  public void read_ulong_array(int[] x, int offs, int len)
  {
    read_long_array(x, offs, len);
  }

  /**
   * Read the CORBA unsigned long long value,
   * delegating functionality to {@link #read_longlong}.
   * There is no way to return values over the limit of
   * the java signed long in other way than returning
   * the negative value.
   */
  public long read_ulonglong()
  {
    return read_longlong();
  }

  /**
   * Reads the array of CORBA long long (java long) values,
   * delegating functionality to
   * {@link #real_longlong_array}.
   */
  public void read_ulonglong_array(long[] x, int offs, int len)
  {
    read_longlong_array(x, offs, len);
  }

  /**
   * Read the unsigned short integer value. Due strange specification,
   * the returned value must be the short type as well, so the
   * the best solution seems just to delegete functionality to
   * read_short.
   */
  public short read_ushort()
  {
    return read_short();
  }

  /**
   * Read an array of unsigned short values, delegating the
   * functionality to {@link read_short_array}.
   */
  public void read_ushort_array(short[] x, int offs, int len)
  {
    read_short_array(x, offs, len);
  }

  /**
   * Reads the wide character using the encoding, specified in the
   * wide_charset.
   */
  public char read_wchar()
  {
    try
      {
        if (giop.until_inclusive(1, 1))
          align(2);

        if (wide_native)
          return (char) b.readShort();
        else
          return (char) new InputStreamReader((InputStream) b, wide_charset).read();
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }
    catch (IOException ex)
      {
        throw new Unexpected();
      }
  }

  /**
   * Read an array of "wide chars", each representing a two byte
   * Unicode character, high byte first.
   */
  public void read_wchar_array(char[] x, int offset, int length)
  {
    try
      {
        if (giop.until_inclusive(1, 1))
          align(2);

        if (wide_native)
          {
            for (int i = offset; i < offset + length; i++)
              x [ i ] = (char) b.readShort();
          }
        else
          {
            InputStreamReader reader =
              new InputStreamReader((InputStream) b, wide_charset);
            reader.read(x, offset, length);
          }
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Reads the string in wide character format
   * (ussually UTF-16, Unicode). Takes the currently set charset
   * into consideration.
   *
   * If the native (UTF-16) encoding is used
   * of the GIOP protocol is before 1.2, delegates functionality
   * to "plain" {@link #read_wstring_UTF_16}.
   */
  public String read_wstring()
  {
    // Native encoding or word oriented data.
    if (wide_native || giop.until_inclusive(1, 1))
      return read_wstring_UTF_16();
    try
      {
        align(4);

        int n = b.readInt();
        byte[] s = new byte[ n ];
        b.read(s);

        return new String(s, 0, n, wide_charset);
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Reads first length of the string and the all characters as an
   * Unicode (UTF-16) characters. Mind that GIOP 1.1 has the extra
   * null character at the end that must be discarded.
   */
  public String read_wstring_UTF_16()
  {
    try
      {
        int p = 0;
        int n = read_long();

        // The null terminator that is no longer present since 1.2 .
        int nt = giop.since_inclusive(1, 2) ? 0 : 1;

        // Convert bytes to shorts.
        n = n / 2;

        char[] s = new char[ n ];

        for (int i = 0; i < s.length; i++)
          s [ i ] = (char) b.readShort();

        // Check for the byte order marker here.
        if (s [ 0 ] == 0xFEFF)
          {
            // Big endian encoding - do nothing, but move the pointer
            // one position forward.
            p = 1;
          }
        else if (s [ 0 ] == 0xFFFE)
          {
            // Little endian encoding, swap the bytes and move one
            // position forward.
            p = 1;

            for (int i = p; i < s.length; i++)
              s [ i ] = swap(s [ i ]);
          }

        // Discard the null terminator and, if needed, the endian marker.
        String r = new String(s, p, n - nt - p);
        return r;
      }
    catch (EOFException ex)
      {
        MARSHAL t = new MARSHAL(UNEXP_EOF);
        t.initCause(ex);
        throw t;
      }

    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Swap bytes in the character.
   */
  public static char swap(char x)
  {
    int hi;
    int lo;

    lo = x & 0xFF;
    hi = (x >> 8) & 0xFF;

    return (char) ((lo << 8) | hi);
  }

  /**
   * Set the current code set context.
   */
  public void setCodeSet(cxCodeSet a_codeset)
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
  public cxCodeSet getCodeSet()
  {
    return codeset;
  }

  /**
   * Read the object that is an instance of the given class. The current
   * implementation delegates functionality to the parameterless
   * {@link readObject()}.
   *
   * @param klass a class of that this object the instance is.
   *
   * @return the returned object.
   */
  public org.omg.CORBA.Object read_Object(Class klass)
  {
    return read_Object();
  }

  /**
   * Read a value type structure from the stream.
   *
   * OMG specification states the writing format is outside the scope
   * of GIOP definition. This implementation uses java serialization
   * mechanism, calling {@link ObjectInputStream#readObject}
   *
   * @return an value type structure, unmarshaled from the stream
   */
  public Serializable read_Value()
  {
    return read_value();
  }

  /**
   * Read the abstract interface. An abstract interface can be either
   * CORBA value type or CORBA object and is returned as an abstract
   * java.lang.Object.
   *
   * As specified in OMG specification, this reads a single
   * boolean and then delegates either to {@link #read_Object()} (for false)
   * or to {@link #read_Value()} (for true).
   *
   * @return an abstract interface, unmarshaled from the stream
   */
  public java.lang.Object read_Abstract()
  {
    return read_abstract_interface();
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_char_array(CharSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_char_array(holder.value, offset, length);
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_wchar_array(WCharSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_wchar_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the char array to fit the newly
   * read values.
   *
   * @param holder_value the existing char array, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private char[] ensureArray(char[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new char[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        char[] value = new char[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_ulong_array(ULongSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_ulong_array(holder.value, offset, length);
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_long_array(LongSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_ulong_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the int array to fit the newly
   * read values.
   *
   * @param holder_value the existing int array, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private int[] ensureArray(int[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new int[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        int[] value = new int[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_float_array(FloatSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_float_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the float array to fit the newly
   * read values.
   *
   * @param holder_value the existing float array, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private float[] ensureArray(float[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new float[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        float[] value = new float[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_double_array(DoubleSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_double_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the double array to fit the newly
   * read values.
   *
   * @param holder_value the existing double array, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private double[] ensureArray(double[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new double[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        double[] value = new double[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_short_array(ShortSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_short_array(holder.value, offset, length);
  }

  /** {@inheritDoc} */
  public void read_ushort_array(UShortSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_ushort_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the short array to fit the newly
   * read values.
   *
   * @param holder_value the existing short array, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private short[] ensureArray(short[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new short[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        short[] value = new short[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_octet_array(OctetSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_octet_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the byte array to fit the newly
   * read values.
   *
   * @param holder_value the existing byte array, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private byte[] ensureArray(byte[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new byte[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        byte[] value = new byte[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_longlong_array(LongLongSeqHolder holder, int offset,
                                  int length
                                 )
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_longlong_array(holder.value, offset, length);
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_ulonglong_array(ULongLongSeqHolder holder, int offset,
                                   int length
                                  )
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_ulonglong_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the array of longs to fit the newly
   * read values.
   *
   * @param holder_value the existing array, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private long[] ensureArray(long[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new long[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        long[] value = new long[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_boolean_array(BooleanSeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    read_boolean_array(holder.value, offset, length);
  }

  /**
   * If required, allocate or resize the array of booleans to fit the newly
   * read values.
   *
   * @param holder_value the existing array of booleans, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private boolean[] ensureArray(boolean[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new boolean[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        boolean[] value = new boolean[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * Read an array. In OMG specification is written that if the data does
   * not fit into the holder value field, that array must be resized.
   * The implementation follows this rule. If the holder value field
   * contains null, it is newly instantiated.
   */
  public void read_any_array(AnySeqHolder holder, int offset, int length)
  {
    holder.value = ensureArray(holder.value, offset, length);
    for (int i = offset; i < offset + length; i++)
      {
        holder.value [ i ] = read_any();
      }
  }

  /**
   * If required, allocate or resize the array of Anys to fit the newly
   * read values.
   *
   * @param holder_value the existing array of Anys, may be null.
   * @param offset the required offset to read.
   * @param length the length of the new sequence.
   *
   * @return the allocated or resized array, same array if no such operations
   * are required.
   */
  private Any[] ensureArray(Any[] holder_value, int offset, int length)
  {
    if (holder_value == null)
      return new Any[ offset + length ];
    else if (holder_value.length < offset + length)
      {
        Any[] value = new Any[ offset + length ];
        System.arraycopy(holder_value, 0, value, 0, holder_value.length);
        return value;
      }
    else
      return holder_value;
  }

  /**
   * This method is required to represent the DataInputStream as a value
   * type object.
   *
   * @return a single entity "IDL:omg.org/CORBA/DataInputStream:1.0",
   * always.
   */
  public String[] _truncatable_ids()
  {
    return new String[] { "IDL:omg.org/CORBA/DataInputStream:1.0" };
  }
}