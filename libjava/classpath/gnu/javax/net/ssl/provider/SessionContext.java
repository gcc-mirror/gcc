/* SessionContext.java -- Implementation of a session context.
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

import java.security.Security;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSessionContext;

/**
 * A collection of SSL sessions. This implementation is a memory-only
 * store; subclasses may implement persistent storage.
 */
class SessionContext implements SSLSessionContext
{

  // Fields.
  // -------------------------------------------------------------------------

  /** The map of Session.ID objects to Sessions. */
  protected final HashMap sessions;

  /** The number of sessions to cache. */
  protected int cacheSize;

  /** The session timeout, in seconds. */
  protected int timeout;

  // Constructor.
  // -------------------------------------------------------------------------

  SessionContext()
  {
    sessions = new HashMap();
    cacheSize = 0;
    try
      {
        timeout = Integer.parseInt(Util.getSecurityProperty("jessie.session.timeout"));
      }
    catch (Exception x)
      {
        // Default 24-hour timeout.
        timeout = 86400;
      }
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public synchronized Enumeration getIds()
  {
    Vector ids = new Vector();
    for(Iterator i = sessions.keySet().iterator(); i.hasNext(); )
      {
        Session.ID id = (Session.ID) i.next();
        ids.add(id.getId());
      }
    return ids.elements();
  }

  public synchronized SSLSession getSession(byte[] sessionId)
  {
    Session session = (Session) sessions.get(new Session.ID(sessionId));
    if (session == null)
      return null;
    long elapsed = System.currentTimeMillis() - session.getLastAccessedTime();
    if ((int) (elapsed / 1000) > timeout)
      {
        removeSession(session.sessionId);
        session.invalidate();
        return null;
      }
    if (!session.valid)
      {
        removeSession(session.sessionId);
        session.invalidate();
        return null;
      }
    return session;
  }

  public int getSessionCacheSize()
  {
    return cacheSize;
  }

  public void setSessionCacheSize(int cacheSize)
  {
    if (cacheSize < 0)
      throw new IllegalArgumentException();
    this.cacheSize = cacheSize;
  }

  public int getSessionTimeout()
  {
    return timeout;
  }

  public void setSessionTimeout(int timeout)
  {
    if (timeout <= 0)
      throw new IllegalArgumentException();
    this.timeout = timeout;
  }

  public String toString()
  {
    return sessions.keySet().toString();
  }

  // Package methods.
  // -------------------------------------------------------------------------

  /**
   * Adds a session to this context. This method:
   *
   * <ol>
   * <li>Will do nothing if the cache already contains the given ID.</li>
   * <li>Will do nothing if the cache limit has been reached (and is
   * not zero).</li>
   * <li>Will remove any invalid sessions in the cache before trying to insert
   * the new one.</li>
   * <li>Will remove any expired sessions before trying to insert the new
   * one.</li>
   * </ol>
   *
   * @param sessionId This session's ID.
   * @param session The session to add.
   * @return True if the session was added, false otherwise.
   */
  synchronized boolean addSession(Session.ID sessionId, Session session)
  {
    if (sessions.containsKey(sessionId))
      return false;
    if (cacheSize > 0 && sessions.size() > cacheSize)
      {
        boolean removed = false;
        for (Iterator i = sessions.values().iterator(); i.hasNext(); )
          {
            Session s = (Session) i.next();
            long elapsed = System.currentTimeMillis() - s.getCreationTime();
            if (!s.valid)
              {
                removeSession(session.sessionId);
                removed = true;
              }
            else if ((int) (elapsed / 1000) > timeout)
              {
                removeSession(session.sessionId);
                removed = true;
              }
          }
        if (removed)
          {
            sessions.put(sessionId, session);
            session.context = this;
            session.sessionId = sessionId;
            return true;
          }
        return false;
      }
    else
      {
        sessions.put(sessionId, session);
        session.context = this;
        session.sessionId = sessionId;
        return true;
      }
  }

  /**
   * Returns whether or not a session with the given ID is cached by this
   * context.
   */
  synchronized boolean containsSessionID(Session.ID sessionId)
  {
    Session s = (Session) sessions.get(sessionId);
    if (s == null)
      {
        return false;
      }
    long elapsed = System.currentTimeMillis() - s.getCreationTime();
    if (!s.valid || (int) (elapsed / 1000) > timeout)
      {
        removeSession(sessionId);
        return false;
      }
    return true;
  }

  /**
   * Removes a session from this context.
   *
   * @param sessionId The ID of the session to remove.
   */
  synchronized boolean removeSession(Session.ID sessionId)
  {
    return sessions.remove(sessionId) != null;
  }

  /**
   * Notifies this context of an access event on a session.
   *
   * @param session The session that was accessed.
   */
  void notifyAccess(Session session)
  {
  }
}
