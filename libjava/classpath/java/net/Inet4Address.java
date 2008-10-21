/* Inet4Address.java --
   Copyright (C) 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import java.io.ObjectStreamException;

/*
 * Written using on-line Java Platform 1.4 API Specification and
 * RFC 1884 (http://www.ietf.org/rfc/rfc1884.txt),
 * RFC 1918 (http://www.ietf.org/rfc/rfc1918.txt),
 * RFC 2365 (http://www.ietf.org/rfc/rfc2365.txt)
 *
 * @author Michael Koch
 * @status Believed complete and correct.
 */
public final class Inet4Address extends InetAddress
{
  /**
   * For compatability with Sun's JDK 1.4.2 rev. 5
   */
  static final long serialVersionUID = 3286316764910316507L;

  /**
   * The address family of these addresses (used for serialization).
   */
  private static final int AF_INET = 2;

  /**
   * Inet4Address objects are serialized as InetAddress objects.
   */
  private Object writeReplace() throws ObjectStreamException
  {
    return new InetAddress(addr, hostName, AF_INET);
  }
  
  /**
   * Initializes this object's addr instance variable from the passed in
   * byte array.  Note that this constructor is protected and is called
   * only by static methods in this class.
   *
   * @param addr The IP number of this address as an array of bytes
   * @param host The hostname of this IP address.
   */
  Inet4Address(byte[] addr, String host)
  {
    super(addr, host, AF_INET);
  }

  /**
   * Checks if the address is a multicast address
   *
   * @since 1.1
   */
  public boolean isMulticastAddress()
  {
    return (addr[0] & 0xf0) == 0xe0;
  }

  /**
   * Checks if this address is a loopback address
   */
  public boolean isLoopbackAddress()
  {
    return (addr[0] & 0xff) == 0x7f;
  }

  /**
   * Checks if this address is a wildcard address
   *
   * @since 1.4
   */
  public boolean isAnyLocalAddress()
  {
    return equals(InetAddress.ANY_IF);
  }

  /**
   * Checks if this address is a link local address
   *
   * @since 1.4
   */
  public boolean isLinkLocalAddress()
  {
    return false;
  }

  /**
   * Checks if this address is a site local address
   *
   * @since 1.4
   */
  public boolean isSiteLocalAddress()
  {
    // 10.0.0.0/8
    if ((addr[0] & 0xff) == 0x0a)
      return true;

    // 172.16.0.0/12
    if ((addr[0] & 0xff) == 0xac && (addr[1] & 0xf0) == 0x10)
      return true;

    // 192.168.0.0/16
    if ((addr[0] & 0xff) == 0xc0 && (addr[1] & 0xff) == 0xa8)
      return true;

    return false;
  }

  /**
   * Checks if this multicast address has global scope
   *
   * @since 1.4
   */
  public boolean isMCGlobal()
  {
    return false;
  }

  /**
   * Checks if this multicast address has node scope
   *
   * @since 1.4
   */
  public boolean isMCNodeLocal()
  {
    return false;
  }

  /**
   * Checks if this multicast address has link scope
   *
   * @since 1.4
   */
  public boolean isMCLinkLocal()
  {
    if (! isMulticastAddress())
      return false;

    return ((addr[0] & 0xff) == 0xe0
	    && (addr[1] & 0xff)  == 0x00
	    && (addr[2] & 0xff)  == 0x00);
  }

  /**
   * Checks if this multicast address has site scope
   *
   * @since 1.4
   */
  public boolean isMCSiteLocal()
  {
    return false;
  }

  /**
   * Checks if this multicast address has organization scope
   *
   * @since 1.4
   */
  public boolean isMCOrgLocal()
  {
    return false;
  }

  /**
   * Returns the address of the current instance
   */
  public byte[] getAddress()
  {
    return (byte[]) addr.clone();
  }

  /**
   * Returns the address as string
   *
   * @since 1.0.2
   */
  public String getHostAddress()
  {
    CPStringBuilder sb = new CPStringBuilder(40);

    int len = addr.length;
    int i = 0;
    
    for ( ; ; )
      {
        sb.append(addr[i] & 0xff);
        i++;
	
        if (i == len)
          break;
	
        sb.append('.');
      }

    return sb.toString();
  }

  /**
   * Computes the hashcode of the instance
   */
  public int hashCode()
  {
    int hash = 0;
    int len = addr.length;
    int i = len > 4 ? len - 4 : 0;

    for (; i < len; i++)
      hash = (hash << 8) | (addr[i] & 0xFF);

    return hash;
  }

  /**
   * Compare the current Inet4Address instance with obj
   *
   * @param obj Object to compare with
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof InetAddress))
      return false;

    byte[] addr1 = addr;
    byte[] addr2 = ((InetAddress) obj).addr;

    if (addr1.length != addr2.length)
      return false;

    for (int i = addr1.length; --i >= 0;)
      if (addr1[i] != addr2[i])
	return false;

    return true;
  }
}
