/* Inet6Address.java
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

import java.util.Arrays;

/**
 * @author Michael Koch
 * @date August 3, 2002.
 */

/*
 * Written using on-line Java Platform 1.4 API Specification and
 * RFC 1884 (http://www.ietf.org/rfc/rfc1884.txt)
 * Status: Believed complete and correct.
 */

public final class Inet6Address extends InetAddress
{
  static final long serialVersionUID = 6880410070516793377L;

  /**
   * Needed for serialization
   */
  byte[] ipaddress;
  
  /**
   * Create an Inet6Address object
   *
   * @param addr The IP address
   * @param host The hostname
   */
  Inet6Address (byte[] addr, String host)
  {
    super (addr, host);
    this.ipaddress = addr;
  }

  /**
   * Utility routine to check if the InetAddress is an IP multicast address
   * 
   * @since 1.1
   */
  public boolean isMulticastAddress ()
  {
    return ipaddress [0] == 0xFF;
  }
 
  /**
   * Utility routine to check if the InetAddress in a wildcard address
   * 
   * @since 1.4
   */
  public boolean isAnyLocalAddress ()
  {
    byte[] anylocal = { 0, 0, 0, 0, 0, 0, 0, 0,
	                0, 0, 0, 0, 0, 0, 0, 0 };
    
    return Arrays.equals(ipaddress, anylocal);
  }
	  
  /**
   * Utility routine to check if the InetAddress is a loopback address
   * 
   * @since 1.4
   */
  public boolean isLoopbackAddress ()
  {
    byte[] loopback = { 0, 0, 0, 0, 0, 0, 0, 0,
	                0, 0, 0, 0, 0, 0, 0, 1 };
    
    return Arrays.equals(ipaddress, loopback);
  }

  /**
   * Utility routine to check if the InetAddress is an link local address
   * 
   * @since 1.4
   */
  public boolean isLinkLocalAddress ()
  {
    return ipaddress [0] == 0xFA;
  }

  /**
   * Utility routine to check if the InetAddress is a site local address
   * 
   * @since 1.4
   */
  public boolean isSiteLocalAddress ()
  {
    return ipaddress [0] == 0xFB;
  }

  /**
   * Utility routine to check if the multicast address has global scope
   * 
   * @since 1.4
   */
  public boolean isMCGlobal ()
  {
    if (!isMulticastAddress ())
      return false;
    
    return (ipaddress [1] & 0x0F) == 0xE;
  }

  /**
   * Utility routine to check if the multicast address has node scope
   * 
   * @since 1.4
   */
  public boolean isMCNodeLocal ()
  {
    if (!isMulticastAddress ())
      return false;
    
    return (ipaddress [1] & 0x0F) == 0x1;
  }

  /**
   * Utility routine to check if the multicast address has link scope
   * 
   * @since 1.4
   */
  public boolean isMCLinkLocal ()
  {
    if (!isMulticastAddress ())
      return false;
    
    return (ipaddress [1] & 0x0F) == 0x2;
  }

  /**
   * Utility routine to check if the multicast address has site scope
   * 
   * @since 1.4
   */
  public boolean isMCSiteLocal ()
  {
    if (!isMulticastAddress ())
      return false;
    
    return (ipaddress [1] & 0x0F) == 0x5;
  }

  /**
   * Utility routine to check if the multicast address has organization scope
   * 
   * @since 1.4
   */
  public boolean isMCOrgLocal ()
  {
    if (!isMulticastAddress ())
      return false;
    
    return (ipaddress [1] & 0x0F) == 0x8;
  }
  
  /**
   * Returns the raw IP address of this InetAddress object. The result is in
   * network byte order: the highest order byte of the address is i
   * n getAddress()[0]
   */
  public byte[] getAddress ()
  {
    return ipaddress;
  }
  
  /**
   * Returns the IP address string in textual presentation
   */
  public String getHostAddress ()
  {
    StringBuffer sbuf = new StringBuffer (40);

    for (int i = 0; i < 16; i += 2)
      {
        int x = ((ipaddress [i] & 0xFF) << 8) | (ipaddress [i + 1] & 0xFF);
        boolean empty = sbuf.length () == 0;
	
        if (empty)
          {
            if (i > 0)
              sbuf.append ("::");
          }
        else
          sbuf.append (':');

        if (x != 0 || i >= 14)
          sbuf.append (Integer.toHexString (x));
      }
   
    return sbuf.toString ();
  }

  /**
   * Returns a hashcode for this IP address
   */
  public int hashCode ()
  {
    return super.hashCode ();
  }
 
  /**
   * Compares this object against the specified object
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof Inet6Address))
      return false;

    Inet6Address tmp = (Inet6Address) obj;

    return super.equals (tmp)
           && this.ipaddress == tmp.ipaddress;
  }
  
  /**
   * Utility routine to check if the InetAddress is an
   * IPv4 compatible IPv6 address
   *
   * @since 1.4
   */
  public boolean isIPv4CompatibleAddress ()
  {
    if (ipaddress [0] != 0x00 || ipaddress [1] != 0x00 ||
        ipaddress [2] != 0x00 || ipaddress [3] != 0x00 ||
	ipaddress [4] != 0x00 || ipaddress [5] != 0x00 ||
	ipaddress [6] != 0x00 || ipaddress [7] != 0x00 ||
	ipaddress [8] != 0x00 || ipaddress [9] != 0x00 ||
	ipaddress [10] != 0x00 || ipaddress [11] != 0x00)
      return false;

    return true;
  }
}
