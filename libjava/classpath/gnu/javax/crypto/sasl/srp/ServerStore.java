/* ServerStore.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl.srp;

import java.util.HashMap;

/**
 * The server-side implementation of the SRP security context store.
 */
public class ServerStore
{
  /** The underlying singleton. */
  private static ServerStore singleton = null;
  /** The map of sid --> Security Context record. */
  private static final HashMap sid2ssc = new HashMap();
  /** The map of sid --> Session timing record. */
  private static final HashMap sid2ttl = new HashMap();
  /** A synchronisation lock. */
  private static final Object lock = new Object();
  /** A counter to generate legible SIDs. */
  private static int counter = 0;

  /** Private constructor to enforce Singleton pattern. */
  private ServerStore()
  {
    super();

    // TODO: add a cleaning timer thread
  }

  /**
   * Returns the classloader Singleton.
   * 
   * @return the classloader Singleton instance.
   */
  static synchronized final ServerStore instance()
  {
    if (singleton == null)
      singleton = new ServerStore();
    return singleton;
  }

  /**
   * Returns a legible new session identifier.
   * 
   * @return a new session identifier.
   */
  static synchronized final byte[] getNewSessionID()
  {
    final String sid = String.valueOf(++counter);
    return new StringBuffer("SID-")
        .append("0000000000".substring(0, 10 - sid.length())).append(sid)
        .toString().getBytes();
  }

  /**
   * Returns a boolean flag indicating if the designated session is still alive
   * or not.
   * 
   * @param sid the identifier of the session to check.
   * @return <code>true</code> if the designated session is still alive.
   *         <code>false</code> otherwise.
   */
  boolean isAlive(final byte[] sid)
  {
    boolean result = false;
    if (sid != null && sid.length != 0)
      {
        synchronized (lock)
          {
            final String key = new String(sid);
            final StoreEntry ctx = (StoreEntry) sid2ttl.get(key);
            if (ctx != null)
              {
                result = ctx.isAlive();
                if (! result) // invalidate it en-passant
                  {
                    sid2ssc.remove(key);
                    sid2ttl.remove(key);
                  }
              }
          }
      }
    return result;
  }

  /**
   * Records a mapping between a session identifier and the Security Context of
   * the designated SRP server mechanism instance.
   * 
   * @param ttl the session's Time-To-Live indicator (in seconds).
   * @param ctx the server's security context.
   */
  void cacheSession(final int ttl, final SecurityContext ctx)
  {
    synchronized (lock)
      {
        final String key = new String(ctx.getSID());
        sid2ssc.put(key, ctx);
        sid2ttl.put(key, new StoreEntry(ttl));
      }
  }

  /**
   * Updates the mapping between the designated session identifier and the
   * designated server's SASL Security Context. In the process, computes and
   * return the underlying mechanism server's evidence that shall be returned to
   * the client in a session re-use exchange.
   * 
   * @param sid the identifier of the session to restore.
   * @return an SRP server's security context.
   */
  SecurityContext restoreSession(final byte[] sid)
  {
    final String key = new String(sid);
    final SecurityContext result;
    synchronized (lock)
      {
        result = (SecurityContext) sid2ssc.remove(key);
        sid2ttl.remove(key);
      }
    return result;
  }

  /**
   * Removes all information related to the designated session ID.
   * 
   * @param sid the identifier of the seesion to invalidate.
   */
  void invalidateSession(final byte[] sid)
  {
    final String key = new String(sid);
    synchronized (lock)
      {
        sid2ssc.remove(key);
        sid2ttl.remove(key);
      }
  }
}
