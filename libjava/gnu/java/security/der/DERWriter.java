/* DERWriter.java -- write Java types in DER format.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package gnu.java.security.der;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import java.math.BigInteger;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;

import java.text.SimpleDateFormat;

import java.util.BitSet;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import gnu.java.security.OID;

/**
 * Methods that allow various Java types to be written as a DER
 * (Distinguished Encoding Rules) stream to the specified output stream.
 * DER is used to encode ASN.1 constructions, but this class provides no
 * methods for interacting with ASN.1. Rather, callers should construct
 * their output objects properly for whatever ASN.1 construct is being
 * output.
 *
 * <p>This class only defines static methods; there are no instance
 * variables needed.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class DERWriter implements DER
{

  // Constructors.
  // ------------------------------------------------------------------------

  /** This class only has static methods. */
  private DERWriter()
  {
  }

  // Class methods.
  // ------------------------------------------------------------------------

  public static int write(OutputStream out, DERValue object) 
    throws IOException
  {
    out.write(object.getExternalTag());
    Object value = object.getValue();
    if (value == null)
      {
        writeLength(out, 0);
        return 0;
      }
    if (value instanceof Boolean)
      return writeBoolean(out, (Boolean) value);
    else if (value instanceof BigInteger)
      return writeInteger(out, (BigInteger) value);
    else if (value instanceof Date)
      return writeDate(out, object.getExternalTag(), (Date) value);
    else if (value instanceof String)
      return writeString(out, object.getExternalTag(), (String) value);
    else if (value instanceof List)
      return writeSequence(out, (List) value);
    else if (value instanceof Set)
      return writeSet(out, (Set) value);
    else if (value instanceof BitString)
      return writeBitString(out, (BitString) value);
    else if (value instanceof OID)
      return writeOID(out, (OID) value);
    else if (value instanceof byte[])
      {
        writeLength(out, ((byte[]) value).length);
        out.write((byte[]) value);
        return ((byte[]) value).length;
      }
    else if (value instanceof DERValue)
      {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        write(bout, (DERValue) value);
        byte[] buf = bout.toByteArray();
        writeLength(out, buf.length);
        out.write(buf);
        return buf.length;
      }
    else
      throw new DEREncodingException("cannot encode " + value.getClass().getName());
  }

  public static int definiteEncodingSize(int length)
  {
    if (length < 128)
      return 1;
    else if (length < 256)
      return 2;
    else if (length < 65536)
      return 3;
    else if (length < 16777216)
      return 4;
    else
      return 5;
  }

  // Own methods.
  // ------------------------------------------------------------------------

  /**
   * Write a BOOLEAN type to the given output stream.
   *
   * @param out The sink output stream.
   * @param b   The boolean value to write.
   */
  private static int writeBoolean(OutputStream out, Boolean b)
    throws IOException
  {
    writeLength(out, 1);
    if (b.booleanValue())
      out.write(0xFF);
    else
      out.write(0);
    return 1;
  }

  /**
   * Write an INTEGER type to the given output stream.
   *
   * @param out The sink output stream.
   * @param integer The integer to write.
   */
  private static int writeInteger(OutputStream out, BigInteger integer)
    throws IOException
  {
    byte[] bytes = integer.toByteArray();
    writeLength(out, bytes.length);
    out.write(bytes);
    return bytes.length;
  }

  private static int writeSequence(OutputStream out, List sequence)
    throws IOException
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream();
    for (Iterator i = sequence.iterator(); i.hasNext(); )
      {
        write(bout, (DERValue) i.next());
      }
    byte[] buf = bout.toByteArray();
    writeLength(out, buf.length);
    out.write(buf);
    return buf.length;
  }

  private static int writeSet(OutputStream out, Set set)
    throws IOException
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream();
    for (Iterator i = set.iterator(); i.hasNext(); )
      {
        write(bout, (DERValue) i.next());
      }
    byte[] buf = bout.toByteArray();
    writeLength(out, buf.length);
    out.write(buf);
    return buf.length;
  }

  private static int writeOID(OutputStream out, OID oid)
    throws IOException
  {
    byte[] der = oid.getDER();
    writeLength(out, der.length);
    out.write(der);
    return der.length;
  }

  private static int writeBitString(OutputStream out, BitString bs)
    throws IOException
  {
    byte[] buf = bs.getShiftedByteArray();
    out.write(buf.length + 1);
    out.write(bs.getIgnoredBits());
    out.write(buf);
    return buf.length;
  }

  private static int writeString(OutputStream out, int tag, String str)
    throws IOException
  {
    Charset charset = null;
    byte[] b = null;
    switch (tag & 0x1F)
      {
        case NUMERIC_STRING:
        case PRINTABLE_STRING:
        case T61_STRING:
        case VIDEOTEX_STRING:
        case IA5_STRING:
        case GRAPHIC_STRING:
        case ISO646_STRING:
        case GENERAL_STRING:
          charset = Charset.forName("ISO-8859-1");
          break;
        case UNIVERSAL_STRING:
        case BMP_STRING:
          charset = Charset.forName("UTF-16BE");
          break;
        case UTF8_STRING:
        default:
          charset = Charset.forName("UTF-8");
          break;
      }
    if (charset == null)
      throw new DEREncodingException("no charset");
    CharsetEncoder encoder = charset.newEncoder();
    ByteBuffer result = encoder.encode(CharBuffer.wrap(str));
    if (result.hasArray())
      {
        b = result.array();
      }
    else
      {
        b = new byte[result.remaining()];
        result.get(b);
      }
    writeLength(out, b.length);
    out.write(b);
    return b.length;
  }

  private static int writeDate(OutputStream out, int tag, Date date)
    throws IOException
  {
    SimpleDateFormat sdf = null;
    if ((tag & 0x1F) == UTC_TIME)
      sdf = new SimpleDateFormat("yyMMddHHmmss'Z'");
    else
      sdf = new SimpleDateFormat("yyyyMMddHHmmss'.'SSS'Z'");
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    byte[] b = sdf.format(date).getBytes("ISO-8859-1");
    writeLength(out, b.length);
    out.write(b);
    return b.length;
  }

  // Package method.
  // ------------------------------------------------------------------------

  static void writeLength(OutputStream out, int len) throws IOException
  {
    if (len < 128)
      out.write(len);
    else if (len < 256)
      {
        out.write(0x81);
        out.write(len);
      }
    else if (len < 65536)
      {
        out.write(0x82);
        out.write(len >> 8);
        out.write(len);
      }
    else if (len < 16777216)
      {
        out.write(0x83);
        out.write(len >> 16);
        out.write(len >>  8);
        out.write(len);
      }
    else
      {
        out.write(0x84);
        out.write(len >> 24);
        out.write(len >> 16);
        out.write(len >>  8);
        out.write(len);
      }
  }
}
