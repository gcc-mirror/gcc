/* InetSocketAddress.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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
  
  String hostname;
  InetAddress addr;
  int port;
    
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
      throw new IllegalArgumentException();
  
    this.addr = addr;
    this.port = port;
    this.hostname = addr.getHostName ();
  }

  /**
   * Constructs an InetSocketAddress instance.
   * 
   * @param port Port if the socket
   *
   * @exception IllegalArgumentException If the port number is illegal
   */
  public InetSocketAddress(int port)
    throws IllegalArgumentException
  {
    if (port < 0 || port > 65535)
      throw new IllegalArgumentException();

    this.port = port;
    
    try
      {
	byte[] any = { 0, 0, 0, 0 };
	this.addr = InetAddress.getByAddress (any);
	this.hostname = "0.0.0.0";
      }
    catch (UnknownHostException e)
      {
        this.addr = null;
	this.hostname = "";
      }
  }


  /**
   * Constructs an InetSocketAddress instance.
   *
   * @param addr Address of the socket
   * @param port Port if the socket
   *
   * @exception IllegalArgumentException If the port number is illegal
   */
  public InetSocketAddress(String hostname, int port)
    throws IllegalArgumentException
  {
    if (port < 0 || port > 65535)
      throw new IllegalArgumentException();

    this.port = port;
    this.hostname = hostname;

    try
      {
        this.addr = InetAddress.getByName(hostname);
      }
    catch (Exception e) // UnknownHostException, SecurityException
      {
        this.addr = null;
      }
  }
 
  /** 
   * Test if obj is a <code>InetSocketAddress</code> and
   * has the same address and port
   */
  public final boolean equals (Object obj)
  {
    // InetSocketAddress objects are equal when addr and port are equal.
    // The hostname may differ.

    if (obj instanceof InetSocketAddress)
      {
        InetSocketAddress a = (InetSocketAddress) obj;
        return addr.equals(a.addr) && a.port == port;
      }
    
    return false;
  }

  /**
   * Returns the <code>InetAddress</code> or
   * <code>null</code> if its unresolved
   */
  public final InetAddress getAddress()
  {
    return addr;
  }

  /**
   * Returns <code>hostname</code>
   */
  public final String getHostName()
  {
    return hostname;
  }

  /**
   * Returns the <code>port</code>
   */
  public final int getPort()
  {
    return port;
  }
    
  /**
   * Returns the hashcode of the <code>InetSocketAddress</code>
   */
  public final int hashCode()
  {
    return port + addr.hashCode();
  }

  /**
   * Checks wether the address has been resolved or not
   */
  public final boolean isUnresolved()
  {
    return addr == null;
  }
    
  /**
   * Returns the <code>InetSocketAddress</code> as string
   */
  public String toString()
  {
    return addr + ":" + port;
  }
}
