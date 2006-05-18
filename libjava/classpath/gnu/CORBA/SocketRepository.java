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
import java.net.SocketException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

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
   * Put a socket. This method also discards all not reusable sockets from
   * the map.
   *
   * @param key as socket key.
   *
   * @param s a socket.
   */
  public static void put_socket(Object key, Socket s)
  {
    synchronized (sockets)
      {
        sockets.put(key, s);
        gc();
      }
  }
  
  /**
   * Removes all non reusable sockets. As it is private,
   * we know we call from the synchronized code already. 
   */
  private static void gc()
  {
    Iterator iter = sockets.entrySet().iterator();
    
    Map.Entry e;
    Socket sx;
    
    while (iter.hasNext())
      {
        e = (Map.Entry) iter.next();
        sx = (Socket) e.getValue();
        
        if (not_reusable(sx))
          iter.remove();
      }
  }
  
  /**
   * Return true if the socket is no longer reusable.
   */
  static boolean not_reusable(Socket s)
  {
    return (s.isClosed() || !s.isBound() || !s.isConnected() ||
        s.isInputShutdown() || s.isOutputShutdown());
  }

  /**
   * Get a socket.
   * 
   * @param key a socket key.
   * 
   * @return an opened socket for reuse, null if no such available or it is
   * closed, its input or output has been shutown or otherwise the socket is not
   * reuseable.
   */
  public static Socket get_socket(Object key)
  {
    synchronized (sockets)
      {
        Socket s = (Socket) sockets.get(key);
        if (s == null)
          return null;

        // Ensure that the socket is fully reusable.
        else if (not_reusable(s))
          {
            sockets.remove(key);
            return null;
          }
        else
          {
            try
              {
                // Set one minute time out that will be changed later.
                s.setSoTimeout(60 * 1000);
              }
            catch (SocketException e)
              {
                s = null;
              }

            sockets.remove(key);
            return s;
          }
      }
  }
}