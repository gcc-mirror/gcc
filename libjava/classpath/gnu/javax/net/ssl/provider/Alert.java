/* Alert.java -- SSL Alert message.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.io.PrintWriter;
import java.io.StringWriter;

import java.nio.ByteBuffer;

/**
 * An alert message in the SSL protocol. Alerts are sent both as warnings
 * which may allow execution to continue, or they may be fatal, which will
 * halt this session. An alert object is composed of two enums -- the level,
 * which indicates the seriousness of the alert, and the description, which
 * indicates the reason for the alert.
 *
 * <pre>
 * struct {
 *   AlertLevel       level;
 *   AlertDescription description;
 * }
 * </pre>
 */
public final class Alert implements Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  /** The underlying byte buffer. */
  private final ByteBuffer buffer;

  // Constructor.
  // -------------------------------------------------------------------------

  public Alert (final ByteBuffer buffer)
  {
    this.buffer = buffer;
  }
  
  public Alert (final Level level, final Description description)
  {
    level.getClass ();
    description.getClass ();
    ByteBuffer b = ByteBuffer.allocate (2);
    b.put (0, (byte) level.getValue ());
    b.put (1, (byte) description.getValue ());
    this.buffer = b.asReadOnlyBuffer ();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length ()
  {
    return 2;
  }

  byte[] getEncoded()
  {
    byte[] buf = new byte[2];
    buffer.position (0);
    buffer.get (buf);
    return buf;
  }

  public Level level()
  {
    return Level.forInteger (buffer.get (0) & 0xFF);
  }

  public Description description()
  {
    return Description.forInteger (buffer.get (1) & 0xFF);
  }

  public void setLevel (final Level level)
  {
    buffer.put (0, (byte) level.getValue ());
  }

  public void setDescription (final Description description)
  {
    buffer.put (1, (byte) description.getValue ());
  }

  public boolean equals (Object o)
  {
    if (!(o instanceof Alert))
      return false;
    Alert that = (Alert) o;
    return that.buffer.position (0).equals (buffer.position (0));
  }

  public int hashCode ()
  {
    return buffer.getShort (0) & 0xFFFF;
  }

  public String toString()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);
    if (prefix != null) out.print (prefix);
    out.println ("struct {");
    if (prefix != null) out.print (prefix);
    out.print ("  level:       ");
    out.print (level ());
    out.println (";");
    if (prefix != null) out.print (prefix);
    out.print ("  description: ");
    out.print (description ());
    out.println (";");
    if (prefix != null) out.print (prefix);
    out.print ("} Alert;");
    return str.toString ();
  }

  // Enumerations.
  // -------------------------------------------------------------------------

  /**
   * The level enumeration.
   *
   * <pre>
   * enum { warning(1), fatal(2), (255) } AlertLevel;
   * </pre>
   */
  public static enum Level
  {

    WARNING (1), FATAL (2);
    
    private final int value;

    private Level(int value)
    {
      this.value = value;
    }

    public static Level forInteger (final int value)
    {
      switch (value & 0xFF)
        {
        case 1: return WARNING;
        case 2: return FATAL;
        default: throw new IllegalArgumentException ("invalid alert level: " + value);
        }
    }

    public int getValue()
    {
      return value;
    }
  }

  /**
   * The description enumeration.
   */
  public static enum Description
  {
    CLOSE_NOTIFY                    (  0),
    UNEXPECTED_MESSAGE              ( 10),
    BAD_RECORD_MAC                  ( 20),
    DECRYPTION_FAILED               ( 21),
    RECORD_OVERFLOW                 ( 22),
    DECOMPRESSION_FAILURE           ( 30),
    HANDSHAKE_FAILURE               ( 40),
    NO_CERTIFICATE                  ( 41),
    BAD_CERTIFICATE                 ( 42),
    UNSUPPORTED_CERTIFICATE         ( 43),
    CERTIFICATE_REVOKED             ( 44),
    CERTIFICATE_EXPIRED             ( 45),
    CERTIFICATE_UNKNOWN             ( 46),
    ILLEGAL_PARAMETER               ( 47),
    UNKNOWN_CA                      ( 48),
    ACCESS_DENIED                   ( 49),
    DECODE_ERROR                    ( 50),
    DECRYPT_ERROR                   ( 51),
    EXPORT_RESTRICTION              ( 60),
    PROTOCOL_VERSION                ( 70),
    INSUFFICIENT_SECURITY           ( 71),
    INTERNAL_ERROR                  ( 80),
    USER_CANCELED                   ( 90),
    NO_RENEGOTIATION                (100),
    UNSUPPORTED_EXTENSION           (110),
    CERTIFICATE_UNOBTAINABLE        (111),
    UNRECOGNIZED_NAME               (112),
    BAD_CERTIFICATE_STATUS_RESPONSE (113),
    BAD_CERTIFICATE_HASH_VALUE      (114),
    UNKNOWN_SRP_USERNAME            (120),
    MISSING_SRP_USERNAME            (121);
    
    private final int value;

    private Description(int value)
    {
      this.value = value;
    }

    /**
     * Return an alert description object based on the specified integer
     * value.
     *
     * @param value The raw description value.
     * @return The appropriate description object.
     */
    public static Description forInteger (final int value)
    {
      switch (value & 0xFF)
        {
        case 0: return CLOSE_NOTIFY;
        case 10: return UNEXPECTED_MESSAGE;
        case 20: return BAD_RECORD_MAC;
        case 21: return DECRYPTION_FAILED;
        case 22: return RECORD_OVERFLOW;
        case 30: return DECOMPRESSION_FAILURE;
        case 40: return HANDSHAKE_FAILURE;
        case 41: return NO_CERTIFICATE;
        case 42: return BAD_CERTIFICATE;
        case 43: return UNSUPPORTED_CERTIFICATE;
        case 44: return CERTIFICATE_REVOKED;
        case 45: return CERTIFICATE_EXPIRED;
        case 46: return CERTIFICATE_UNKNOWN;
        case 47: return ILLEGAL_PARAMETER;
        case 48: return UNKNOWN_CA;
        case 49: return ACCESS_DENIED;
        case 50: return DECODE_ERROR;
        case 51: return DECRYPT_ERROR;
        case 60: return EXPORT_RESTRICTION;
        case 70: return PROTOCOL_VERSION;
        case 71: return INSUFFICIENT_SECURITY;
        case 80: return INTERNAL_ERROR;
        case 90: return USER_CANCELED;
        case 100: return NO_RENEGOTIATION;
        case 120: return UNKNOWN_SRP_USERNAME;
        case 121: return MISSING_SRP_USERNAME;
        default: throw new IllegalArgumentException("unknown alert description: " + value);
        }
    }

    public int getValue()
    {
      return value;
    }
  }
}
