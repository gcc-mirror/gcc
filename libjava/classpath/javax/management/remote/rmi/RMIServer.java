/* RMIServer.java -- RMI object for connecting to an RMI JMX connector.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package javax.management.remote.rmi;

import java.io.IOException;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * RMI interface for obtaining an instance of an
 * {@link RMIConnection}.  An implementation of this
 * interface exists for each RMI connector.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface RMIServer
  extends Remote
{

  /**
   * Returns the version of the RMI connection protocol used
   * by this server.  The returned string takes the form of
   * <emph>protocol-version implementation-name</emph> where
   * <emph>protocol-version</emph> is a series of two or more
   * non-negative integers, separated by a decimal point (.)
   * (currently {@code 1.0}) and <emph>implementation-name</emph>
   * is the string {@code "GNU Classpath"} followed by the version
   * of GNU Classpath in use.
   *
   * @return the string specified above.
   * @throws RemoteException if there is a problem with the transfer
   *                         of the string via RMI.
   */
  String getVersion()
    throws RemoteException;

  /**
   * Constructs and returns a new RMI connection using the specified
   * authentication credentials.  Each client calls this method to
   * obtain a connection to the server.
   *
   * @param credentials a user-defined object passed to the server
   *                    to authenticate the client.  May be {@code null}.
   * @return the new connection.
   * @throws IOException if the new connection can not be created or
   *                     exported, or an error occurs in the RMI transmission.
   * @throws SecurityException if the client could not be authenticated
   *                           correctly using the supplied credientials.
   */
  RMIConnection newClient(Object credentials)
    throws IOException;
}
