/* InetSocketAddress.java --
   Copyright (C) 2002, 2006  Free Software Foundation, Inc.

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

package java.net;


/**
 * InetSocketAddress instances represent socket addresses
 * in the java.nio package. They encapsulate a InetAddress and
 * a port number.
 *
 * @since 1.4
 */
public class InetSocketAddress extends SocketAddress
{
  /**
   * Compatible with JDK 1.4+
   */
  private static final long serialVersionUID = 5076001401234631237L;

  /**
   * Name of host.
   */
  private String hostname;

  /**
   * Address of host.
   */
  private InetAddress addr;

  /**
   * Port of host.
   */
  private int port;

  /**
   * Constructs an InetSocketAddress instance.
   *
   * @param addr Address of the socket
   * @param port Port if the socket
   *
   * @exception IllegalArgumentException If the port number is illegel
   */
  public InetSocketAddress(InetAddress addr, int port)
    throws IllegalArgumentException
  {
    if (port < 0 || port > 65535)
      throw new IllegalArgumentException("Bad port number: " + port);

    if (addr == null)
      addr = InetAddress.ANY_IF;

    this.addr = addr;
    this.port = port;
  }

  /**
   * Constructs an InetSocketAddress instance.
   *
   * @param port Port if the socket
   *
   * @exception IllegalArgumentException If the port number is illegal
   */
  public InetSocketAddress(int port) throws IllegalArgumentException
  {
    this((InetAddress) null, port);
  }

  /**
   * Constructs an InetSocketAddress instance.
   *
   * @param hostname The hostname for the socket address
   * @param port The port for the socket address
   *
   * @exception IllegalArgumentException If the port number is illegal or
   * the hostname argument is null
   */
  public InetSocketAddress(String hostname, int port)
  {
    this(hostname, port, true);
  }

  /**
   * Constructs an InetSocketAddress instance.
   *
   * @param hostname The hostname for the socket address
   * @param port The port for the socket address
   * @param resolve <code>true</code> if the address has to be resolved,
   * <code>false</code> otherwise
   *
   * @exception IllegalArgumentException If the port number is illegal or
   * the hostname argument is null
   */
  private InetSocketAddress(String hostname, int port, boolean resolve)
  {
    if (hostname == null)
      throw new IllegalArgumentException("Null host name value");

    if (port < 0 || port > 65535)
      throw new IllegalArgumentException("Bad port number: " + port);

    this.port = port;
    this.hostname = hostname;
    this.addr = null;

    if (resolve)
    {
      try
        {
          this.addr = InetAddress.getByName(hostname);
        }
      catch (Exception e) // UnknownHostException, SecurityException
        {
          // Do nothing here. this.addr is already set to null.
        }
    }

  }

  /**
   * Creates an unresolved <code>InetSocketAddress</code> object.
   *
   * @param hostname The hostname for the socket address
   * @param port The port for the socket address
   *
   * @exception IllegalArgumentException If the port number is illegal or
   * the hostname argument is null
   *
   * @since 1.5
   */
  public static InetSocketAddress createUnresolved(String hostname, int port)
  {
    return new InetSocketAddress(hostname, port, false);
  }

  /**
   * Test if obj is a <code>InetSocketAddress</code> and
   * has the same address and port
   *
   * @param obj The obj to compare this address with.
   *
   * @return True if obj is equal.
   */
  public final boolean equals(Object obj)
  {
    // InetSocketAddress objects are equal when addr and port are equal.
    // The hostname may differ.
    if (obj instanceof InetSocketAddress)
      {
	InetSocketAddress sa = (InetSocketAddress) obj;

	if (addr == null && sa.addr != null)
	  return false;
	else if (addr == null && sa.addr == null) // we know hostname != null
	  return hostname.equals(sa.hostname) && sa.port == port;
	else
	  return addr.equals(sa.addr) && sa.port == port;
      }

    return false;
  }

  /**
   * Returns the <code>InetAddress</code> or
   * <code>null</code> if its unresolved
   *
   * @return The IP address of this address.
   */
  public final InetAddress getAddress()
  {
    return addr;
  }

  /**
   * Returns <code>hostname</code>
   *
   * @return The hostname of this address.
   */
  public final String getHostName()
  {
    if (hostname == null) // we know addr != null
      hostname = addr.getHostName();

    return hostname;
  }

  /**
   * Returns the <code>port</code>
   *
   * @return The port of this address.
   */
  public final int getPort()
  {
    return port;
  }

  /**
   * Returns the hashcode of the <code>InetSocketAddress</code>
   *
   * @return The hashcode for this address.
   */
  public final int hashCode()
  {
    return port + addr.hashCode();
  }

  /**
   * Checks wether the address has been resolved or not
   *
   * @return True if address is unresolved.
   */
  public final boolean isUnresolved()
  {
    return addr == null;
  }

  /**
   * Returns the <code>InetSocketAddress</code> as string
   *
   * @return A string representation of this address.
   */
  public String toString()
  {
    // Note: if addr is null, then hostname != null.
    return (addr == null ? hostname : addr.toString()) + ":" + port;
  }
}
