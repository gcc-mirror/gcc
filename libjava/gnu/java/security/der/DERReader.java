/* DERReader.java -- parses ASN.1 DER sequences
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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;

import java.math.BigInteger;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import gnu.java.security.OID;

/**
 * This class decodes DER sequences into Java objects. The methods of
 * this class do not have knowledge of higher-levels of structure in the
 * DER stream -- such as ASN.1 constructions -- and it is therefore up
 * to the calling application to determine if the data are structured
 * properly by inspecting the {@link DERValue} that is returned.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class DERReader implements DER
{

  // Fields.
  // ------------------------------------------------------------------------

  protected InputStream in;

  protected final ByteArrayOutputStream encBuf;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new DER reader from a byte array.
   *
   * @param in The encoded bytes.
   */
  public DERReader(byte[] in)
  {
    this(new ByteArrayInputStream(in));
  }

  public DERReader (byte[] in, int off, int len)
  {
    this (new ByteArrayInputStream (in, off, len));
  }

  /**
   * Create a new DER readed from an input stream.
   *
   * @param in The encoded bytes.
   */
  public DERReader(InputStream in)
  {
    if (!in.markSupported())
      this.in = new BufferedInputStream(in, 16384);
    else
      this.in = in;
    encBuf = new ByteArrayOutputStream(2048);
  }

  // Class methods.
  // ------------------------------------------------------------------------

  /**
   * Convenience method for reading a single primitive value from the
   * given byte array.
   *
   * @param encoded The encoded bytes.
   * @throws IOException If the bytes do not represent an encoded
   * object.
   */
  public static DERValue read(byte[] encoded) throws IOException
  {
    return new DERReader(encoded).read();
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public void skip (int bytes) throws IOException
  {
    in.skip (bytes);
  }

  /**
   * Decode a single value from the input stream, returning it in a new
   * {@link DERValue}. By "single value" we mean any single type in its
   * entirety -- including constructed types such as SEQUENCE and all
   * the values they contain. Usually it is sufficient to call this
   * method once to parse and return the top-level structure, then to
   * inspect the returned value for the proper contents.
   *
   * @return The parsed DER structure.
   * @throws IOException If an error occurs reading from the input
   * stream.
   * @throws DEREncodingException If the input does not represent a
   * valid DER stream.
   */
  public DERValue read() throws IOException
  {
    int tag = in.read();
    if (tag == -1)
      throw new EOFException();
    encBuf.write(tag);
    int len = readLength();
    DERValue value = null;
    if ((tag & CONSTRUCTED) == CONSTRUCTED)
      {
        in.mark(2048);
        byte[] encoded = new byte[len];
        in.read(encoded);
        encBuf.write(encoded);
        value = new DERValue(tag, len, CONSTRUCTED_VALUE, encBuf.toByteArray());
        in.reset();
        encBuf.reset();
        return value;
      }
    switch (tag & 0xC0)
      {
        case UNIVERSAL:
          value = new DERValue(tag, len, readUniversal(tag, len),
            encBuf.toByteArray());
          encBuf.reset();
          break;
        case CONTEXT:
          byte[] encoded = new byte[len];
          in.read(encoded);
          encBuf.write(encoded);
          value = new DERValue(tag, len, encoded, encBuf.toByteArray());
          encBuf.reset();
          break;
        case APPLICATION:
          // This should not be reached, since (I think) APPLICATION is
          // always constructed.
          throw new DEREncodingException("non-constructed APPLICATION data");
        default:
          throw new DEREncodingException("PRIVATE class not supported");
      }
    return value;
  }

  protected int readLength() throws IOException
  {
    int i = in.read();
    if (i == -1)
      throw new EOFException();
    encBuf.write(i);
    if ((i & ~0x7F) == 0)
      {
        return i;
      }
    else if (i < 0xFF)
      {
        byte[] octets = new byte[i & 0x7F];
        in.read(octets);
        encBuf.write(octets);
        return new BigInteger(1, octets).intValue();
      }
    throw new DEREncodingException();
  }

  // Own methods.
  // ------------------------------------------------------------------------

  private Object readUniversal(int tag, int len) throws IOException
  {
    byte[] value = new byte[len];
    in.read(value);
    encBuf.write(value);
    switch (tag & 0x1F)
      {
        case BOOLEAN:
          if (value.length != 1)
            throw new DEREncodingException();
          return Boolean.valueOf(value[0] != 0);
        case NULL:
          if (len != 0)
            throw new DEREncodingException();
          return null;
        case INTEGER:
        case ENUMERATED:
          return new BigInteger(value);
        case BIT_STRING:
          byte[] bits = new byte[len - 1];
          System.arraycopy(value, 1, bits, 0, bits.length);
          return new BitString(bits, value[0] & 0xFF);
        case OCTET_STRING:
          return value;
        case NUMERIC_STRING:
        case PRINTABLE_STRING:
        case T61_STRING:
        case VIDEOTEX_STRING:
        case IA5_STRING:
        case GRAPHIC_STRING:
        case ISO646_STRING:
        case GENERAL_STRING:
        case UNIVERSAL_STRING:
        case BMP_STRING:
        case UTF8_STRING:
          return makeString(tag, value);
        case UTC_TIME:
        case GENERALIZED_TIME:
          return makeTime(tag, value);
        case OBJECT_IDENTIFIER:
          return new OID(value);
        case RELATIVE_OID:
          return new OID(value, true);
        default:
          throw new DEREncodingException("unknown tag " + tag);
      }
  }

  private static String makeString(int tag, byte[] value)
    throws IOException
  {
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
          return fromIso88591(value);

        case UNIVERSAL_STRING:
          // XXX The docs say UniversalString is encoded in four bytes
          // per character, but Java has no support (yet) for UTF-32.
          //return new String(buf, "UTF-32");
        case BMP_STRING:
          return fromUtf16Be(value);

        case UTF8_STRING:
          return fromUtf8(value);

        default:
          throw new DEREncodingException("unknown string tag");
      }
  }

  private static String fromIso88591(byte[] bytes)
  {
    StringBuffer str = new StringBuffer(bytes.length);
    for (int i = 0; i < bytes.length; i++)
      str.append((char) (bytes[i] & 0xFF));
    return str.toString();
  }

  private static String fromUtf16Be(byte[] bytes) throws IOException
  {
    if ((bytes.length & 0x01) != 0)
      throw new IOException("UTF-16 bytes are odd in length");
    StringBuffer str = new StringBuffer(bytes.length / 2);
    for (int i = 0; i < bytes.length; i += 2)
      {
        char c = (char) ((bytes[i] << 8) & 0xFF);
        c |= (char) (bytes[i+1] & 0xFF);
        str.append(c);
      }
    return str.toString();
  }

  private static String fromUtf8(byte[] bytes) throws IOException
  {
    StringBuffer str = new StringBuffer((int)(bytes.length / 1.5));
    for (int i = 0; i < bytes.length; )
      {
        char c = 0;
        if ((bytes[i] & 0xE0) == 0xE0)
          {
            if ((i + 2) >= bytes.length)
              throw new IOException("short UTF-8 input");
            c = (char) ((bytes[i++] & 0x0F) << 12);
            if ((bytes[i] & 0x80) != 0x80)
              throw new IOException("malformed UTF-8 input");
            c |= (char) ((bytes[i++] & 0x3F) << 6);
            if ((bytes[i] & 0x80) != 0x80)
              throw new IOException("malformed UTF-8 input");
            c |= (char) (bytes[i++] & 0x3F);
          }
        else if ((bytes[i] & 0xC0) == 0xC0)
          {
            if ((i + 1) >= bytes.length)
              throw new IOException("short input");
            c = (char) ((bytes[i++] & 0x1F) << 6);
            if ((bytes[i] & 0x80) != 0x80)
              throw new IOException("malformed UTF-8 input");
            c |= (char) (bytes[i++] & 0x3F);
          }
        else if ((bytes[i] & 0xFF) < 0x80)
          {
            c = (char) (bytes[i++] & 0xFF);
          }
        else
          throw new IOException("badly formed UTF-8 sequence");
        str.append(c);
      }
    return str.toString();
  }

  private Date makeTime(int tag, byte[] value) throws IOException
  {
    Calendar calendar = Calendar.getInstance();
    String str = makeString(PRINTABLE_STRING, value);

    // Classpath's SimpleDateFormat does not work for parsing these
    // types of times, so we do this by hand.
    String date = str;
    String tz = "";
    if (str.indexOf("+") > 0)
      {
        date = str.substring(0, str.indexOf("+"));
        tz = str.substring(str.indexOf("+"));
      }
    else if (str.indexOf("-") > 0)
      {
        date = str.substring(0, str.indexOf("-"));
        tz = str.substring(str.indexOf("-"));
      }
    else if (str.endsWith("Z"))
      {
        date = str.substring(0, str.length()-2);
        tz = "Z";
      }
    if (!tz.equals("Z") && tz.length() > 0)
      calendar.setTimeZone(TimeZone.getTimeZone(tz));
    else
      calendar.setTimeZone(TimeZone.getTimeZone("UTC"));
    if ((tag & 0x1F) == UTC_TIME)
      {
        if (date.length() < 10)  // must be at least 10 chars long
          throw new DEREncodingException("cannot parse date");
        // UTCTime is of the form "yyMMddHHmm[ss](Z|(+|-)hhmm)"
        try
          {
            int year = Integer.parseInt(str.substring(0, 2));
            if (year < 50)
              year += 2000;
            else
              year += 1900;
            calendar.set(year,
              Integer.parseInt(str.substring( 2,  4))-1,  // month
              Integer.parseInt(str.substring( 4,  6)),    // day
              Integer.parseInt(str.substring( 6,  8)),    // hour
              Integer.parseInt(str.substring( 8, 10)));   // minute
            if (date.length() == 12);
              calendar.set(Calendar.SECOND,
                Integer.parseInt(str.substring(10, 12)));
          }
        catch (NumberFormatException nfe)
          {
            throw new DEREncodingException("cannot parse date");
          }
      }
    else
      {
        if (date.length() < 10)  // must be at least 10 chars long
          throw new DEREncodingException("cannot parse date");
        // GeneralTime is of the form "yyyyMMddHH[mm[ss[(.|,)SSSS]]]"
        // followed by "Z" or "(+|-)hh[mm]"
        try
          {
            calendar.set(
              Integer.parseInt(date.substring(0, 4)),      // year
              Integer.parseInt(date.substring(4, 6))-1,    // month
              Integer.parseInt(date.substring(6, 8)),      // day
              Integer.parseInt(date.substring(8, 10)), 0); // hour, min
            switch (date.length())
              {
                case 19:
                case 18:
                case 17:
                case 16:
                  calendar.set(Calendar.MILLISECOND,
                    Integer.parseInt(date.substring(15)));
                case 14:
                  calendar.set(Calendar.SECOND,
                    Integer.parseInt(date.substring(12, 14)));
                case 12:
                  calendar.set(Calendar.MINUTE,
                    Integer.parseInt(date.substring(10, 12)));
              }
          }
        catch (NumberFormatException nfe)
          {
            throw new DEREncodingException("cannot parse date");
          }
      }
    return calendar.getTime();
  }
}
