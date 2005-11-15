/* MessageHeader.java -- GIOP message header.
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


package gnu.CORBA.GIOP;

import gnu.CORBA.Minor;
import gnu.CORBA.Version;
import gnu.CORBA.CDR.BigEndianInputStream;
import gnu.CORBA.CDR.BigEndianOutputStream;
import gnu.CORBA.CDR.LittleEndianInputStream;
import gnu.CORBA.CDR.LittleEndianOutputStream;
import gnu.CORBA.CDR.AbstractDataInput;
import gnu.CORBA.CDR.AbstractDataOutput;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.IDLEntity;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.Arrays;

/**
 * The GIOP message header.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class MessageHeader
  implements IDLEntity
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Request message.
   */
  public static final byte REQUEST = 0;

  /**
   * Reply message
   */
  public static final byte REPLY = 1;

  /**
   * Cancel request message.
   */
  public static final byte CANCEL_REQUEST = 2;

  /**
   * Locate request message, used to check the server ability to process
   * requests for the object reference. This message is also used to get the
   * address where the object reference should be sent.
   */
  public static final byte LOCATE_REQUEST = 3;

  /**
   * Locate reply message, sent in response to the {@link #LocateRequest}
   * message.
   */
  public static final byte LOCATE_REPLY = 4;

  /**
   * Instruction to close the connection.
   */
  public static final byte CLOSE_CONNECTION = 5;

  /**
   * Error report.
   */
  public static final byte MESSAGE_ERROR = 6;

  /**
   * The fragment messge, following the previous message that has more fragments
   * flag set. Added in GIOP 1.1
   */
  public static final byte FRAGMENT = 7;

  /**
   * This must always be "GIOP".
   */
  public static final byte[] MAGIC = new byte[] { 'G', 'I', 'O', 'P' };

  /**
   * The message type names.
   */
  protected static String[] types = new String[] { "Request", "Reply",
    "Cancel", "Locate request", "Locate reply", "Close connection", "Error",
    "Fragment" };

  /**
   * The GIOP version. Initialised to 1.0 .
   */
  public Version version;

  /**
   * The flags field, introduced since GIOP 1.1.
   */
  public byte flags = 0;

  /**
   * The message type.
   */
  public byte message_type = REQUEST;

  /**
   * The message size, excluding the message header.
   */
  public int message_size = 0;

  /**
   * Create an empty message header, corresponding version 1.0.
   */
  public MessageHeader()
  {
    version = new Version(1, 0);
  }

  /**
   * Create an empty message header, corresponding the given version.
   * 
   * @param major the major message header version.
   * @param minor the minot message header version.
   */
  public MessageHeader(int major, int minor)
  {
    version = new Version(major, minor);
  }

  /**
   * Checks if the message is encoded in the Big Endian, most significant byte
   * first.
   */
  public boolean isBigEndian()
  {
    return (flags & 0x1) == 0;
  }

  /**
   * Checks if the message is partial, and more subsequent fragments follow.
   */
  public boolean moreFragmentsFollow()
  {
    return (flags & 0x2) != 0;
  }

  /**
   * Set the encoding to use.
   * 
   * @param use_big_endian if true (default), the Big Endian encoding is used.
   * If false, the Little Endian encoding is used.
   */
  public void setBigEndian(boolean use_big_endian)
  {
    if (use_big_endian)
      flags = (byte) (flags & ~1);
    else
      flags = (byte) (flags | 1);
  }

  /**
   * Get the size of the message header itself. So far, it is always 12 bytes.
   */
  public int getHeaderSize()
  {
    return 12;
  }

  /**
   * Get the message type as string.
   * 
   * @param type the message type as int (the field {@link message_type}).
   * 
   * @return the message type as string.
   */
  public String getTypeString(int type)
  {
    try
      {
        return types[type];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        return "unknown type (" + type + ")";
      }
  }

  /**
   * Creates reply header, matching the message header version number.
   * 
   * @return one of {@link gnu.CORBA.GIOP.v1_0.ReplyHeader},
   * {@link gnu.CORBA.GIOP.v1_2.ReplyHeader}, etc - depending on the version
   * number in this header.
   */
  public ReplyHeader create_reply_header()
  {
    if (version.since_inclusive(1, 2))
      return new gnu.CORBA.GIOP.v1_2.ReplyHeader();
    else
      return new gnu.CORBA.GIOP.v1_0.ReplyHeader();
  }

  /**
   * Creates request header, matching the message header version number.
   * 
   * @return one of {@link gnu.CORBA.GIOP.v1_0.RequestHeader},
   * {@link gnu.CORBA.GIOP.v1_2.RequestHeader}, etc - depending on the version
   * number in this header.
   */
  public RequestHeader create_request_header()
  {
    if (version.since_inclusive(1, 2))
      return new gnu.CORBA.GIOP.v1_2.RequestHeader();
    else
      return new gnu.CORBA.GIOP.v1_0.RequestHeader();
  }

  /**
   * Create the cancel header, matching the message header version number.
   */
  public CancelHeader create_cancel_header()
  {
    return new gnu.CORBA.GIOP.v1_0.CancelHeader();
  }

  /**
   * Create the error message.
   */
  public ErrorMessage create_error_message()
  {
    return new ErrorMessage(version);
  }

  /**
   * Read the header from the stream.
   * 
   * @param istream a stream to read from.
   * 
   * @throws MARSHAL if this is not a GIOP 1.0 header.
   */
  public void read(java.io.InputStream istream)
    throws MARSHAL
  {
    try
      {
        byte[] xMagic = new byte[MAGIC.length];
        istream.read(xMagic);
        if (!Arrays.equals(xMagic, MAGIC))
          {
            MARSHAL m = new MARSHAL("Not a GIOP message");
            m.minor = Minor.Giop;
            throw m;
          }

        version = Version.read_version(istream);

        AbstractDataInput din;

        flags = (byte) istream.read();

        // This checks the bit in the byte we have just received.
        if (isBigEndian())
          din = new BigEndianInputStream(istream);
        else
          din = new LittleEndianInputStream(istream);

        message_type = (byte) din.read();

        message_size = din.readInt();
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.Header;
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Get the short string summary of the message.
   * 
   * @return a short message summary.
   */
  public String toString()
  {
    return "GIOP " + version + ", " + (isBigEndian() ? "Big" : "Little")
      + " endian, " + getTypeString(message_type) + ", " + message_size
      + " bytes. ";
  }

  /**
   * Write the header to stream.
   * 
   * @param out a stream to write into.
   */
  public void write(java.io.OutputStream out)
  {
    try
      {
        AbstractDataOutput dout;

        if (isBigEndian())
          dout = new BigEndianOutputStream(out);
        else
          dout = new LittleEndianOutputStream(out);

        // Write magic sequence.
        dout.write(MAGIC);

        // Write version number.
        version.write((OutputStream) dout);
        dout.write(flags);
        dout.write(message_type);
        dout.writeInt(message_size);
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.Header;
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Read data, followed by the message header. Handle fragmented messages.
   * 
   * @param source the data source to read from.
   * @param service the socket on that the time outs are set. Can be null (no
   * timeouts are set).
   * @param to_read the timeout while reading the message.
   * @param to_pause the timeout for pauses between the message parts.
   */
  public byte[] readMessage(InputStream source, Socket service, int to_read,
    int to_pause)
  {
    try
      {
        byte[] r = new byte[message_size];

        int n = 0;
        if (service != null)
          service.setSoTimeout(to_read);

        reading: while (n < r.length)
          {
            n += source.read(r, n, r.length - n);
          }
        if (service != null)
          service.setSoTimeout(to_pause);

        // Read the message remainder if the message is fragmented.
        if (moreFragmentsFollow())
          {
            ByteArrayOutputStream buffer = new ByteArrayOutputStream(
              2 * r.length);
            buffer.write(r);

            if (r.length < 10)
              // Increase the buffer size if the default value (size of the
              // previous message) is really too small.
              r = new byte[1024];

            MessageHeader h2 = new MessageHeader();

            do
              {
                h2.read(source);

                int dn;

                n = 0;
                reading: while (n < h2.message_size)
                  {
                    dn = source.read(r, 0, h2.message_size - n);

                    if (n == 0 && service != null)
                      service.setSoTimeout(to_read);

                    if (n == 0 && version.since_inclusive(1, 2))
                      {
                        // Skip the four byte request id.
                        buffer.write(r, 4, dn - 4);
                      }
                    else
                      buffer.write(r, 0, dn);
                    n = +dn;
                  }

                if (service != null)
                  service.setSoTimeout(to_pause);
              }
            while (h2.moreFragmentsFollow());
            return buffer.toByteArray();
          }
        else
          return r;
      }
    catch (IOException ioex)
      {
        MARSHAL m = new MARSHAL("Unable to read the message continuation.");
        m.minor = Minor.Header;
        m.initCause(ioex);
        throw m;
      }
  }
}