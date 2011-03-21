/* ClientStore.java --
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
 * The client-side implementation of the SRP security context store.
 */
public class ClientStore
{
  /** The underlying singleton. */
  private static ClientStore singleton = null;
  /** The map of uid --> SASL Security Context record. */
  private static final HashMap uid2ssc = new HashMap();
  /** The map of sid --> Session timing record. */
  private static final HashMap uid2ttl = new HashMap();
  /** A synchronisation lock. */
  private static final Object lock = new Object();

  /** Private constructor to enforce Singleton pattern. */
  private ClientStore()
  {
    super();

    // TODO: add a cleaning timer thread
  }

  /**
   * Returns the classloader Singleton.
   *
   * @return the classloader Singleton instance.
   */
  static synchronized final ClientStore instance()
  {
    if (singleton == null)
      singleton = new ClientStore();
    return singleton;
  }

  /**
   * Returns a boolean flag indicating if the designated client's session is
   * still alive or not.
   *
   * @param uid the identifier of the client whose session to check.
   * @return <code>true</code> if the designated client's session is still
   *         alive. <code>false</code> otherwise.
   */
  boolean isAlive(final String uid)
  {
    final boolean result;
    synchronized (lock)
      {
        final Object obj = uid2ssc.get(uid);
        result = (obj != null);
        if (result) // is it still alive?
          {
            final StoreEntry sto = (StoreEntry) uid2ttl.get(uid);
            if (! sto.isAlive()) // invalidate it
              {
                uid2ssc.remove(uid);
                uid2ttl.remove(uid);
              }
          }
      }
    return result;
  }

  /**
   * Records a mapping between a client's unique identifier and its security
   * context.
   *
   * @param uid the unique identifier of the SRP client for which the session is
   *          to be cached.
   * @param ttl the session's Time-To-Live indicator (in seconds).
   * @param ctx the client's security context.
   */
  void cacheSession(final String uid, final int ttl, final SecurityContext ctx)
  {
    synchronized (lock)
      {
        uid2ssc.put(uid, ctx);
        uid2ttl.put(uid, new StoreEntry(ttl));
      }
  }

  /**
   * Removes the mapping between the designated SRP client unique identifier and
   * the its session security context (and other timing information).
   *
   * @param uid the identifier of the client whose session is to invalidate.
   */
  void invalidateSession(final String uid)
  {
    synchronized (lock)
      {
        uid2ssc.remove(uid);
        uid2ttl.remove(uid);
      }
  }

  /**
   * Returns an SRP client's security context record mapped by that client's
   * unique identifier.
   *
   * @param uid the identifier of the client whose session is to restore.
   * @return the SRP client's security context.
   */
  SecurityContext restoreSession(final String uid)
  {
    final SecurityContext result;
    synchronized (lock)
      {
        result = (SecurityContext) uid2ssc.remove(uid);
        uid2ttl.remove(uid);
      }
    return result;
  }
}
