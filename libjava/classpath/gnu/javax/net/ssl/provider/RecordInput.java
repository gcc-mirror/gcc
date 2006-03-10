/* RecordInput.java -- record layer input.
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

import gnu.classpath.SystemProperties;
import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.PrintWriter;

import java.util.logging.Logger;

import javax.net.ssl.SSLProtocolException;

class RecordInput
{

  // Fields.
  // -------------------------------------------------------------------------

  private static final boolean DEBUG_RECORD_LAYER = true;
  private static final Logger logger = SystemLogger.SYSTEM;

  private byte[] fragment;
  private int index;
  private ContentType type;

  private final DataInputStream in;
  private Session session;

  // Constructor.
  // -------------------------------------------------------------------------

  RecordInput (final InputStream in, final Session session)
  {
    this.in = new DataInputStream (in);
    this.session = session;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  synchronized int available (ContentType type) throws IOException
  {
    if (fragment == null)
      {
        readRecord ();
      }
    if (type != this.type)
      {
        return 0;
      }
    return fragment.length - index;
  }

  void setSession (Session session)
  {
    this.session = session;
  }

  synchronized int read (byte[] buf, int off, int len, ContentType type)
    throws IOException
  {
    if (off < 0 || len < 0 || off + len > buf.length)
      {
        throw new ArrayIndexOutOfBoundsException ("size=" + buf.length +
                                                  " off=" + off + " len=" + len);
      }
    if (fragment == null || index >= fragment.length)
      {
        readRecord ();
      }
    if (type != this.type)
      {
        return 0;
      }
    len = Math.min (len, fragment.length - index);
    System.arraycopy (fragment, index, buf, off, len);
    index += len;
    return len;
  }

  boolean pollClose () throws IOException
  {
    if (fragment == null || index >= fragment.length)
      {
        try
          {
            readRecord();
          }
        catch (AlertException ae)
          {
            Alert alert = ae.getAlert();
            if (alert.getDescription() == Alert.Description.CLOSE_NOTIFY)
              {
                return true;
              }
            throw ae;
          }
      }
    return false;
  }

  private void readRecord() throws IOException
  {
    type = ContentType.read (in);
    if ((type.getValue() & 0x80) != 0 || (type.getValue() & 0x40) != 0)
      {
        in.read();
        if ((type.getValue() & 0x40) != 0)
          {
            in.read();
          }
        type = ContentType.read(in);
        if (type != ContentType.CLIENT_HELLO_V2)
          {
            throw new SSLProtocolException("unsupported V2 message");
          }
        type = ContentType.HANDSHAKE;
        // Record this message, and re-present it as a normal handshake
        // layer message. ClientHello will handle the real parsing.
        ByteArrayOutputStream buffer = new ByteArrayOutputStream (256);
        buffer.write(1); // The type we just read.
        RecordingInputStream in2 = new RecordingInputStream (in, buffer);
        ProtocolVersion version = ProtocolVersion.read (in2);
        if (version.compareTo (ProtocolVersion.SSL_3) < 0)
          {
            throw new SSLProtocolException("unsupported client version");
          }
        int len = (in2.read() & 0xFF) << 8 | (in2.read() & 0xFF);
        len += (in2.read() & 0xFF) << 8 | (in2.read() & 0xFF);
        len += (in2.read() & 0xFF) << 8 | (in2.read() & 0xFF);
        int count = 0;
        while (count < len)
          {
            int l = (int) in2.skip(len - count);
            if (l > 0)
              {
                count += l;
              }
          }
        fragment = buffer.toByteArray ();
        index = 0;

        // We can't be encrypted/MACed/compressed here, since a V2 message
        // will only be sent as the first message, and only by the client.
        return;
      }
    ProtocolVersion v = ProtocolVersion.read (in);
    int len = in.readUnsignedShort ();
    if (len > session.params.getFragmentLength() + 2048)
      {
        throw new OverflowException();
      }
    fragment = new byte [len];
    in.readFully (fragment);

    if (DEBUG_RECORD_LAYER)
      {
        logger.log (Component.SSL_RECORD_LAYER,
                    ">> READ RECORD <<{4}" +
                    "struct {{4}" +
                    "  type = {0};{4}" +
                    "  version = {1};{4}" +
                    "  length = {2};{4}" +
                    "{3}{4}" +
                    "} TLSCiphertext;", new Object[]
          {
            type, v, new Integer (len),
            Util.hexDump (fragment, "  "),
            SystemProperties.getProperty ("line.separator")
          });
      }

    fragment = session.params.decrypt (fragment, v, type);
    index = 0;

    if (session.random != null)
      session.random.setSeed (fragment);

    if (type == ContentType.ALERT)
      {
        Alert alert = Alert.read (new ByteArrayInputStream (fragment));
        session.currentAlert = alert;
      }
    if (session.currentAlert != null)
      {
        throw new AlertException (session.currentAlert, false);
      }
  }
}
