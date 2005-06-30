/* HostnameVerifier.java -- verifies disparate hostnames.
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

/**
 * The interface for classes that perform hostname verification for cases
 * when the hostname used to begin the connection (such as in a URL)
 * does not match the hostname used in the SSL handshake.
 * Implementations of this interface should provide an implementation
 * of the {@link #verify(java.lang.String,javax.net.ssl.SSLSession)}
 * method that accepts or rejects hostnames as appropriate.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public interface HostnameVerifier
{

  /**
   * Verifies a hostname given a particular SSL session. This method
   * should return <code>true</code> if the hostname is an accepted
   * alias for the hostname negotiated in the SSL handshake.
   *
   * @param hostname The hostname in question.
   * @param session  The current SSL session.
   * @return <code>true</code> if the hostname is acceptable.
   */
  boolean verify(String hostname, SSLSession session);
}
