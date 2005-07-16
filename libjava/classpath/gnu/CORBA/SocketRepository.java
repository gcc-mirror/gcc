/* SocketRepository.java --
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


package gnu.CORBA;

import java.net.Socket;

import java.util.HashMap;

/**
 * This class caches the opened sockets that are reused during the
 * frequent calls. Otherwise, some CORBA applications may spend
 * up to 90 % of the working time just for closing and opening the sockets.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class SocketRepository
{
  /**
   * The socket map.
   */
  private static HashMap sockets = new HashMap();

  /**
   * Put a socket.
   *
   * @param key as socket key.
   *
   * @param s a socket.
   */
  public static void put_socket(Object key, Socket s)
  {
    sockets.put(key, s);
  }

  /**
   * Get a socket.
   *
   * @param key a socket key.
   *
   * @return an opened socket for reuse, null if no such
   * available or it is closed.
   */
  public static Socket get_socket(Object key)
  {
    Socket s = (Socket) sockets.get(key);
    if (s != null && s.isClosed())
      {
        sockets.remove(key);
        return null;
      }
    else
      {
        sockets.remove(key);
        return s;
      }
  }
}