/* LDAPCertStoreParameters.java -- LDAP CertStore parameters.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.security.cert;

/**
 * Parameters for CertStores that are retrieved via the <i>lightweight
 * directory access protocol</i> (<b>LDAP</b>).
 *
 * @see CertStore
 */
public class LDAPCertStoreParameters implements CertStoreParameters
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** The default LDAP port. */
  private static final int LDAP_PORT = 389;

  /** The server name. */
  private final String serverName;

  /** The LDAP port. */
  private final int port;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new LDAPCertStoreParameters object, with a servername of
   * "localhost" and a port of 389.
   */
  public LDAPCertStoreParameters()
  {
    this("localhost", LDAP_PORT);
  }

  /**
   * Create a new LDAPCertStoreParameters object, with a specified
   * server name and a port of 389.
   *
   * @param serverName The LDAP server name.
   * @throws NullPointerException If <i>serverName</i> is null.
   */
  public LDAPCertStoreParameters(String serverName)
  {
    this(serverName, LDAP_PORT);
  }

  /**
   * Create a new LDAPCertStoreParameters object, with a specified
   * server name and port.
   *
   * @param serverName The LDAP server name.
   * @param port       The LDAP port.
   * @throws NullPointerException If <i>serverName</i> is null.
   */
  public LDAPCertStoreParameters(String serverName, int port)
  {
    if (serverName == null)
      throw new NullPointerException();
    this.serverName = serverName;
    this.port = port;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public Object clone()
  {
    return new LDAPCertStoreParameters(serverName, port);
  }

  /**
   * Return the server name.
   *
   * @return The server name.
   */
  public String getServerName()
  {
    return serverName;
  }

  /**
   * Return the port.
   *
   * @return the port.
   */
  public int getPort()
  {
    return port;
  }

  /**
   * Return a string representation of these parameters.
   *
   * @return The string representation of these parameters.
   */
  public String toString()
  {
    return "LDAPCertStoreParameters: [ serverName: " + serverName
      + "; port: " + port + " ]";
  }
}
