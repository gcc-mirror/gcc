/* AbstractSessionContext -- stores SSL sessions, possibly persistently.
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


package gnu.javax.net.ssl;

import gnu.java.security.Requires;

import gnu.javax.net.ssl.provider.SimpleSessionContext;

import java.util.Enumeration;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLPermission;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSessionContext;

/**
 * A skeletal implementation of {@link SSLSessionContext}. This class may
 * be subclassed to add extended functionality to session contexts, such
 * as by storing sessions in files on disk, or by sharing contexts
 * across different JVM instances.
 *
 * <p>In order to securely store sessions, along with private key data,
 * the abstract methods {@lnk {@link #load(char[])} and {@link #store(char[])}
 * come into play. When storing sessions, a session context implementation
 * must pass this password to the {@link Session#prepare(char[])} method,
 * before either writing the {@link java.io.Serializable} session to the
 * underlying store, or getting the opaque {@link Session#privateData()}
 * class from the session, and storing that.
 *
 * <p>As a simple example, that writes sessions to some object output
 * stream:
 *
 * <pre>
  char[] password = ...;
  ObjectOutputStream out = ...;
  ...
  for (Session s : this)
    {
      s.prepare(password);
      out.writeObject(s);
    }</pre>
 *
 * <p>The reverse must be done when deserializing sessions, by using the
 * {@link Session#repair(char[])} method, possibly by first calling
 * {@link Session#setPrivateData(java.io.Serializable)} with the read,
 * opaque private data type. Thus an example of reading may be:
 *
 * <pre>
  char[] password = ...;
  ObjectInputStream in = ...;
  ...
  while (hasMoreSessions(in))
    {
      Session s = (Session) in.readObject();
      s.repair(password);
      addToThisStore(s);
    }</pre>
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public abstract class AbstractSessionContext implements SSLSessionContext
{
  protected long timeout;
  private static Class<? extends AbstractSessionContext>
    implClass = SimpleSessionContext.class;

  /**
   * Create a new instance of a session context, according to the configured
   * implementation class.
   *
   * @return The new session context.
   * @throws SSLException If an error occurs in creating the instance.
   */
  public static AbstractSessionContext newInstance () throws SSLException
  {
    try
      {
        return implClass.newInstance();
      }
    catch (IllegalAccessException iae)
      {
        throw new SSLException(iae);
      }
    catch (InstantiationException ie)
      {
        throw new SSLException(ie);
      }
  }

  /**
   * Reconfigure this instance to use a different session context
   * implementation.
   *
   * <p><strong>Note:</strong> this method requires that the caller have
   * {@link SSLPermission} with target
   * <code>gnu.javax.net.ssl.AbstractSessionContext</code> and action
   * <code>setImplClass</code>.
   *
   * @param clazz The new implementation class.
   * @throws SecurityException If the caller does not have permission to
   *  change the session context.
   */
  @Requires(permissionClass = SSLPermission.class,
            target = "gnu.javax.net.ssl.AbstractSessionContext",
            action = "setImplClass")
  public static synchronized void setImplClass
    (Class<? extends AbstractSessionContext> clazz)
    throws SecurityException
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkPermission(new SSLPermission("gnu.javax.net.ssl.AbstractSessionContext",
                                           "setImplClass"));
    implClass = clazz;
  }

  /**
   * @param timeout The initial session timeout.
   */
  protected AbstractSessionContext (final int timeout)
  {
    setSessionTimeout(timeout);
  }

  /**
   * Fetch a saved session by its ID. This method will (possibly)
   * deserialize and return the SSL session with that ID, or null if
   * the requested session does not exist, or has expired.
   *
   * <p>Subclasses implementing this class <strong>must not</strong>
   * perform any blocking operations in this method. If any blocking
   * behavior is required, it must be done in the {@link load(char[])}
   * method.
   *
   * @param sessionId The ID of the session to get.
   * @return The found session, or null if no such session was found,
   * or if that session has expired.
   */
  public final SSLSession getSession (byte[] sessionId)
  {
    Session s = implGet (sessionId);
    if (s != null
        && System.currentTimeMillis () - s.getLastAccessedTime () > timeout)
      {
        remove (sessionId);
        return null;
      }
    return s;
  }

  public final SSLSession getSession(String host, int port)
  {
    for (Enumeration e = getIds(); e.hasMoreElements(); )
      {
        byte[] id = (byte[]) e.nextElement();
        SSLSession s = getSession(id);
        if (s == null) // session expired.
          continue;
        String host2 = s.getPeerHost();
        if (host == null)
          {
            if (host2 != null)
              continue;
          }
        else if (!host.equals(host2))
          continue;
        int port2 = s.getPeerPort();
        if (port != port2)
          continue;

        // Else, a match.
        return s;
      }

    return null;
  }

  /**
   * To be implemented by subclasses. Subclasses do not need to check
   * timeouts in this method.
   *
   * @param sessionId The session ID.
   * @return The session, or <code>null</code> if the requested session
   *  was not found.
   */
  protected abstract Session implGet (byte[] sessionId);

  public int getSessionTimeout()
  {
    return (int) (timeout / 1000);
  }

  /**
   * Load this session store from the underlying media, if supported
   * by the implementation.
   *
   * @param password The password that protects the sensitive data in
   * this store.
   * @throws SessionStoreException If reading this store fails, such
   * as when an I/O exception occurs, or if the password is incorrect.
   */
  public abstract void load (char[] password) throws SessionStoreException;

  /**
   * Add a new session to the store. The underlying implementation
   * will add the session to its store, possibly overwriting any
   * existing session with the same ID.
   *
   * <p>Subclasses implementing this class <strong>must not</strong>
   * perform any blocking operations in this method. If any blocking
   * behavior is required, it must be done in the {@link
   * #store(char[])} method.
   *
   * @param session The session to add.
   * @throws NullPointerException If the argument is null.
   */
  public abstract void put (Session session);

  /**
   * Remove a session from this store.
   *
   * <p>Subclasses implementing this class <strong>must not</strong>
   * perform any blocking operations in this method. If any blocking
   * behavior is required, it must be done in the {@link
   * #store(char[])} method.
   *
   * @param sessionId The ID of the session to remove.
   */
  public abstract void remove (byte[] sessionId);

  /**
   *
   */
  public final void setSessionTimeout(int seconds)
  {
    if (timeout < 0)
      throw new IllegalArgumentException("timeout may not be negative");
    this.timeout = (long) seconds * 1000;
  }

  /**
   * Commit this session store to the underlying media. For session
   * store implementations that support saving sessions across
   * invocations of the JVM, this method will save any sessions that
   * have not expired to some persistent media, so they may be loaded
   * and used again later.
   *
   * @param password The password that will protect the sensitive data
   * in this store.
   */
  public abstract void store (char[] password) throws SessionStoreException;
}
