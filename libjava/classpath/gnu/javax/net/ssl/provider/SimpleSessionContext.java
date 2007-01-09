/* SimpleSessionContext.java -- memory-only session store.
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

import gnu.javax.net.ssl.AbstractSessionContext;
import gnu.javax.net.ssl.Session;
import gnu.javax.net.ssl.SessionStoreException;
import gnu.javax.net.ssl.Session.ID;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * A simple, non-persistent SessionContext.
 * 
 * @author csm
 */
public final class SimpleSessionContext
  extends AbstractSessionContext
{
  /**
   * By default, sessions last for 5 minutes.
   */
  public static final int DEFAULT_TIMEOUT = 300;
  
  private final HashMap<Session.ID, Session> store;
  private int storeLimit;
  
  public SimpleSessionContext()
  {
    super(DEFAULT_TIMEOUT);
    storeLimit = 0;
    store = new HashMap<Session.ID, Session>();
  }
  
  @Override
  protected Session implGet(byte[] sessionId)
  {
    return store.get(new Session.ID(sessionId));
  }

  @Override
  public void load(char[] password) throws SessionStoreException
  {
    // Not supported. Memory-only.
  }

  @Override
  public void put(Session session)
  {
    if (storeLimit > 0 && store.size() >= storeLimit)
      {
        Session oldest = null;
        for (Map.Entry<Session.ID, Session> e : store.entrySet())
          {
            Session s = e.getValue();
            long stamp = s.getLastAccessedTime();
            if (oldest == null || oldest.getLastAccessedTime() > stamp)
              oldest = s;
          }
        store.remove(oldest.id());
      }
    store.put(session.id(), session);
  }

  @Override
  public void remove(byte[] sessionId)
  {
    store.remove(new Session.ID(sessionId));
  }

  @Override
  public void store(char[] password) throws SessionStoreException
  {
    // Not supported. Memory-only.
  }

  public Enumeration getIds()
  {
    return new Enumeration()
    {
      Iterator<Session.ID> it = store.keySet().iterator();
      
      public boolean hasMoreElements()
      {
        return it.hasNext();
      }
      
      public Object nextElement()
      {
        return it.next().id();
      }
    };
  }

  public int getSessionCacheSize()
  {
    return storeLimit;
  }

  public void setSessionCacheSize(int size)
  {
    if (size < 0)
      throw new IllegalArgumentException("cache size must be nonnegative");
    this.storeLimit = size;
  }

}
