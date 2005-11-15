/* ErrorMessage.java --
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

import gnu.CORBA.OrbFunctional;
import gnu.CORBA.IOR;
import gnu.CORBA.Minor;

import java.io.IOException;
import java.io.OutputStream;

import java.net.Socket;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;

/**
 * The error message is sent in response to the message, encoded
 * in the unsupported version of the format or otherwise invalid.
 *
 * The error message consists from the message header only and
 * is the same for GIOP 1.0, 1.1, 1.2 and 1.3.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ErrorMessage
  extends MessageHeader
{
  /** 
   * Use serialVersionUID for interoperability. 
   */
  private static final long serialVersionUID = 1;
  
  /**
   * Create a new error message, setting the message field
   * to the {@link MESSAGE_ERROR} and the version number to
   * the given major and minor values.
   */
  public ErrorMessage(gnu.CORBA.Version msg_version)
  {
    version = msg_version;
    message_type = MESSAGE_ERROR;
  }

  /**
   * Send the error message to the given IOR address.
   *
   * @param ior the IOR address (host and port, other fields
   * are not used).
   * 
   * @param orb the ORB, sending the error message.
   */
  public void send(IOR ior, ORB orb)
  {
    try
      {
        Socket socket;
        
        if (orb instanceof OrbFunctional)
          socket = ((OrbFunctional) orb).socketFactory.createClientSocket(
            ior.Internet.host, ior.Internet.port);
        else
          socket = new Socket(ior.Internet.host, ior.Internet.port);

        OutputStream socketOutput = socket.getOutputStream();
        write(socketOutput);
        socketOutput.close();
        socket.close();
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.Header;
        t.initCause(ex);
        throw t;
      }
  }
}
