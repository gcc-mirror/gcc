/* SSLSessionContext.java -- collection of SSL sessions.
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.net.ssl;

import java.util.Enumeration;

/**
 * A collection of saved SSL sessions, with thier corresponding session
 * IDs.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public interface SSLSessionContext
{

  /**
   * Returns an enumeration of all saved session IDs. Every element in
   * the returned enumeration is a byte array.
   *
   * @return The session IDs.
   */
  Enumeration getIds();

  /**
   * Gets the session specified by its ID, or <code>null</code> if there
   * is no session, or if it has expired.
   *
   * @param sessionId The ID of the session to get.
   * @return The session, or <code>null</code>.
   */
  SSLSession getSession(byte[] sessionId);

  /**
   * Returns the maximum number of sessions that may be cached by this
   * session context.
   *
   * @return The maximum number of sessions that may be cached.
   */
  int getSessionCacheSize();

  /**
   * Returns the period of time (in seconds) that a session may be cached
   * for before becoming invalid.
   *
   * @return The time a session may be valid.
   */
  int getSessionTimeout();

  /**
   * Sets the maximum number of sessions that may be cached by this
   * session context. A cache size of 0 means no limit.
   *
   * @param size The new cache size.
   * @throws IllegalArgumentException If <code>size</code> is negative.
   */
  void setSessionCacheSize(int size);

  /**
   * Sets the period of time (in seconds) that a session may be cached
   * for before becoming invalid. A timeout of 0 means that sessions
   * never expire.
   *
   * @param seconds The new timeout.
   * @throws IllegalArgumentException If <code>seconds</code> is negative.
   */
  void setSessionTimeout(int seconds);
}
