/* CloseMessage.java --
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

import org.omg.CORBA.MARSHAL;

import java.io.IOException;
import java.io.OutputStream;

/**
 * The explicit command to close the connection.
 *
 *
 * The close message consists from the message header only and
 * is the same for GIOP 1.0, 1.1, 1.2 and 1.3. The CloseMessage
 * uses the default value from the {@link MessageHeader}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class CloseMessage
  extends MessageHeader
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The singleton close message is typically enough, despite new
   * instances may be instantiated if the specific version field
   * value is mandatory.
   */
  private static final CloseMessage Singleton = new CloseMessage();

  /**
   * Create a new error message, setting the message field
   * to the {@link MESSAGE_CLOSE} and the version number to
   * the given major and minor values.
   */
  public CloseMessage()
  {
    message_type = CLOSE_CONNECTION;
  }

  /**
   * Send the close message to the given output stream. The method,
   * however, does not close the socket itself, this must be done
   * explicitly in the calling code.
   *
   * @param socketStream a stream, where the close message is
   * written.
   */
  public static void close(OutputStream socketStream)
  {
    try
      {
        Singleton.write(socketStream);
        socketStream.flush();
      }
    catch (IOException ex)
      {
        MARSHAL m = new MARSHAL("Unable to flush the stream");
        m.minor = Minor.Header;
        m.initCause(ex);
        throw m;
      }
  }
}
