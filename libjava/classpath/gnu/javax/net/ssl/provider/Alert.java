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

import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * An alert message in the SSL protocol. Alerts are sent both as warnings
 * which may allow execution to continue, or they may be fatal, which will
 * halt this session. An alert object is composed of two enums -- the level,
 * which indicates the seriousness of the alert, and the description, which
 * indicates the reason for the alert.
 *
 * <pre>
 * struct {
 *   AlertLevel level;
 *   AlertDescription description;
 * }
 * </pre>
 */
final class Alert implements Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  /** The alert level enumerated. */
  private final Level level;

  /** The alert description enumerated. */
  private final Description description;

  // Constructor.
  // -------------------------------------------------------------------------

  Alert(Level level, Description description)
  {
    this.level = level;
    this.description = description;
  }

  // Class method.
  // -------------------------------------------------------------------------

  static Alert read(InputStream in) throws IOException
  {
    Level level = Level.read(in);
    Description desc = Description.read(in);
    return new Alert(level, desc);
  }

  static Alert forName(String name)
  {
    if (name == null)
      {
        return new Alert(Level.FATAL, Description.INTERNAL_ERROR);
      }
    Description desc = Description.INTERNAL_ERROR;
    if (name.equals("close_notify"))
      {
        desc = Description.CLOSE_NOTIFY;
      }
    else if (name.equals("unexpected_message"))
      {
        desc = Description.UNEXPECTED_MESSAGE;
      }
    else if (name.equals("bad_record_mac"))
      {
        desc = Description.BAD_RECORD_MAC;
      }
    else if (name.equals("DECRYPTION_FAILED"))
      {
        desc = Description.DECRYPTION_FAILED;
      }
    else if (name.equals("record_overflow"))
      {
        desc = Description.RECORD_OVERFLOW;
      }
    else if (name.equals("decompression_failure"))
      {
        desc = Description.DECOMPRESSION_FAILURE;
      }
    else if (name.equals("handshake_failure"))
      {
        desc = Description.HANDSHAKE_FAILURE;
      }
    else if (name.equals("no_certificate"))
      {
        desc = Description.NO_CERTIFICATE;
      }
    else if (name.equals("bad_certificate"))
      {
        desc = Description.BAD_CERTIFICATE;
      }
    else if (name.equals("unsupported_certificate"))
      {
        desc = Description.UNSUPPORTED_CERTIFICATE;
      }
    else if (name.equals("certificate_revoked"))
      {
        desc = Description.CERTIFICATE_REVOKED;
      }
    else if (name.equals("certificate_expired"))
      {
        desc = Description.CERTIFICATE_EXPIRED;
      }
    else if (name.equals("certificate_unknown"))
      {
        desc = Description.CERTIFICATE_UNKNOWN;
      }
    else if (name.equals("illegal_parameter"))
      {
        desc = Description.ILLEGAL_PARAMETER;
      }
    else if (name.equals("unknown_ca"))
      {
        desc = Description.UNKNOWN_CA;
      }
    else if (name.equals("access_denied"))
      {
        desc = Description.ACCESS_DENIED;
      }
    else if (name.equals("decode_error"))
      {
        desc = Description.DECODE_ERROR;
      }
    else if (name.equals("decrypt_error"))
      {
        desc = Description.DECRYPT_ERROR;
      }
    else if (name.equals("export_restriction"))
      {
        desc = Description.EXPORT_RESTRICTION;
      }
    else if (name.equals("protocol_version"))
      {
        desc = Description.PROTOCOL_VERSION;
      }
    else if (name.equals("insufficient_security"))
      {
        desc = Description.INSUFFICIENT_SECURITY;
      }
    else if (name.equals("internal_error"))
      {
        desc = Description.INTERNAL_ERROR;
      }
    else if (name.equals("user_canceled"))
      {
        desc = Description.USER_CANCELED;
      }
    else if (name.equals("no_renegotiation"))
      {
        desc = Description.NO_RENEGOTIATION;
      }
    else if (name.equals("unsupported_extension"))
      {
        desc = Description.UNSUPPORTED_EXTENSION;
      }
    else if (name.equals("certificate_unobtainable"))
      {
        desc = Description.CERTIFICATE_UNOBTAINABLE;
      }
    else if (name.equals("unrecognized_name"))
      {
        desc = Description.UNRECOGNIZED_NAME;
      }
    else if (name.equals("bad_certificate_status_response"))
      {
        desc = Description.BAD_CERTIFICATE_STATUS_RESPONSE;
      }
    else if (name.equals("bad_certificate_hash_value"))
      {
        desc = Description.BAD_CERTIFICATE_HASH_VALUE;
      }
    else if (name.equals("unknown_srp_username"))
      {
        desc = Description.UNKNOWN_SRP_USERNAME;
      }
    else if (name.equals("missing_srp_username"))
      {
        desc = Description.MISSING_SRP_USERNAME;
      }
    return new Alert(Level.FATAL, desc);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    out.write((byte) level.getValue());
    out.write((byte) description.getValue());
  }

  byte[] getEncoded()
  {
    return new byte[] { (byte) level.getValue(),
                        (byte) description.getValue() };
  }

  Level getLevel()
  {
    return level;
  }

  Description getDescription()
  {
    return description;
  }

  public String toString()
  {
    String nl = System.getProperty("line.separator");
    return "struct {" + nl +
           "  level = " + level + ";" + nl +
           "  description = " + description + ";" + nl +
           "} Alert;" + nl;
  }

  // Inner classes.
  // -------------------------------------------------------------------------

  /**
   * The level enumeration.
   *
   * <pre>
   * enum { warning(1), fatal(2), (255) } AlertLevel;
   * </pre>
   */
  static final class Level implements Enumerated
  {

    // Constants and fields.
    // -----------------------------------------------------------------------

    static final Level WARNING = new Level(1), FATAL = new Level(2);

    private final int value;

    // Constructor.
    // -----------------------------------------------------------------------

    private Level(int value)
    {
      this.value = value;
    }

    // Class method.
    // -----------------------------------------------------------------------

    static Level read(InputStream in) throws IOException
    {
      int i = in.read();
      if (i == -1)
        {
          throw new EOFException("unexpected end of stream");
        }
      switch (i & 0xFF)
        {
        case 1: return WARNING;
        case 2: return FATAL;
        default: return new Level(i);
        }
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public byte[] getEncoded()
    {
      return new byte[] { (byte) value };
    }

    public int getValue()
    {
      return value;
    }

    public String toString()
    {
      switch (value)
        {
        case 1: return "warning";
        case 2: return "fatal";
        default: return "unknown(" + value + ")";
        }
    }
  }

  /**
   * The description enumeration.
   */
  static final class Description implements Enumerated
  {

    // Constants and fields.
    // -----------------------------------------------------------------------

    static final Description
      CLOSE_NOTIFY                    = new Description(  0),
      UNEXPECTED_MESSAGE              = new Description( 10),
      BAD_RECORD_MAC                  = new Description( 20),
      DECRYPTION_FAILED               = new Description( 21),
      RECORD_OVERFLOW                 = new Description( 22),
      DECOMPRESSION_FAILURE           = new Description( 30),
      HANDSHAKE_FAILURE               = new Description( 40),
      NO_CERTIFICATE                  = new Description( 41),
      BAD_CERTIFICATE                 = new Description( 42),
      UNSUPPORTED_CERTIFICATE         = new Description( 43),
      CERTIFICATE_REVOKED             = new Description( 44),
      CERTIFICATE_EXPIRED             = new Description( 45),
      CERTIFICATE_UNKNOWN             = new Description( 46),
      ILLEGAL_PARAMETER               = new Description( 47),
      UNKNOWN_CA                      = new Description( 48),
      ACCESS_DENIED                   = new Description( 49),
      DECODE_ERROR                    = new Description( 50),
      DECRYPT_ERROR                   = new Description( 51),
      EXPORT_RESTRICTION              = new Description( 60),
      PROTOCOL_VERSION                = new Description( 70),
      INSUFFICIENT_SECURITY           = new Description( 71),
      INTERNAL_ERROR                  = new Description( 80),
      USER_CANCELED                   = new Description( 90),
      NO_RENEGOTIATION                = new Description(100),
      UNSUPPORTED_EXTENSION           = new Description(110),
      CERTIFICATE_UNOBTAINABLE        = new Description(111),
      UNRECOGNIZED_NAME               = new Description(112),
      BAD_CERTIFICATE_STATUS_RESPONSE = new Description(113),
      BAD_CERTIFICATE_HASH_VALUE      = new Description(114),
      UNKNOWN_SRP_USERNAME            = new Description(120),
      MISSING_SRP_USERNAME            = new Description(121);

    private final int value;

    // Constructor.
    // -----------------------------------------------------------------------

    private Description(int value)
    {
      this.value = value;
    }

    // Class method.
    // -----------------------------------------------------------------------

    static Description read(InputStream in) throws IOException
    {
      int i = in.read();
      if (i == -1)
        {
          throw new EOFException("unexpected end of input stream");
        }
      switch (i)
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
        default: return new Description(i);
        }
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public byte[] getEncoded()
    {
      return new byte[] { (byte) value };
    }

    public int getValue()
    {
      return value;
    }

    public String toString()
    {
      switch (value)
        {
        case   0: return "close_notify";
        case  10: return "unexpected_message";
        case  20: return "bad_record_mac";
        case  21: return "decryption_failed";
        case  22: return "record_overflow";
        case  30: return "decompression_failure";
        case  40: return "handshake_failure";
        case  42: return "bad_certificate";
        case  43: return "unsupported_certificate";
        case  44: return "certificate_revoked";
        case  45: return "certificate_expired";
        case  46: return "certificate_unknown";
        case  47: return "illegal_parameter";
        case  48: return "unknown_ca";
        case  49: return "access_denied";
        case  50: return "decode_error";
        case  51: return "decrypt_error";
        case  60: return "export_restriction";
        case  70: return "protocol_version";
        case  71: return "insufficient_security";
        case  80: return "internal_error";
        case  90: return "user_canceled";
        case 100: return "no_renegotiation";
        case 110: return "unsupported_extension";
        case 111: return "certificate_unobtainable";
        case 112: return "unrecognized_name";
        case 113: return "bad_certificate_status_response";
        case 114: return "bad_certificate_hash_value";
        case 120: return "unknown_srp_username";
        case 121: return "missing_srp_username";
        default: return "unknown(" + value + ")";
        }
    }
  }
}
