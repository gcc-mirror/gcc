/* Inet4Address.java
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

import java.io.ObjectStreamException;
import java.util.Arrays;

/**
 * @author Michael Koch
 * @date August 3, 2002.
 */

/*
 * Written using on-line Java Platform 1.4 API Specification and
 * RFC 1884 (http://www.ietf.org/rfc/rfc1884.txt),
 * RFC 1918 (http://www.ietf.org/rfc/rfc1918.txt),
 * RFC 2365 (http://www.ietf.org/rfc/rfc2365.txt)
 * Status: Believed complete and correct.
 */

public final class Inet4Address extends InetAddress
{
  static final long serialVersionUID = 3286316764910316507L;

  /**
   * needed for serialization
   */
  private Object writeReplace () throws ObjectStreamException
  {
    return new InetAddress (addr, hostName);
  }

  /**
   * Creates a Inet4Address
   * 
   * @param addr The IP address
   * @param host The Hostname
   */
  Inet4Address(byte[] addr, String host)
  {
    super (addr, host);
  }

  /**
   * Checks if the address is a multicast address
   *
   * @since 1.1
   */
  public boolean isMulticastAddress ()
  {
    return (addr [0] & 0xF0) == 0xE0;
  }
  
  /**
   * Checks if this address is a loopback address
   */
  public boolean isLoopbackAddress ()
  {
    return addr [0] == 0x7F;
  }
 
  /**
   * Checks if this address is a wildcard address
   *
   * @since 1.4
   */
  public boolean isAnyLocalAddress ()
  {
    byte[] anylocal = { 0, 0, 0, 0 };
    
    return Arrays.equals(addr, anylocal);
  }

  /**
   * Checks if this address is a link local address
   * 
   * @since 1.4
   */
  public boolean isLinkLocalAddress ()
  {
    // XXX: This seems to not exist with IPv4 addresses
    return false;
  }

  /**
   * Checks if this address is a site local address
   * 
   * @since 1.4
   */
  public boolean isSiteLocalAddress ()
  {
    // 10.0.0.0/8
    if (addr [0] == 0x0A)
      return true;

    // XXX: Suns JDK 1.4.1 (on Linux) seems to have a bug here:
    // it says 172.16.0.0 - 172.255.255.255 are site local addresses
    //
    // 172.16.0.0/12
    if (addr [0] == 0xAC && (addr [1] & 0xF0) == 0x01)
      return true;

    // 192.168.0.0/16
    if (addr [0] == 0xC0 && addr [1] == 0xA8)
      return true;
   
    // XXX: Do we need to check more addresses here ?
    return false;
  }

  /**
   * Checks if this multicast address has global scope
   * 
   * @since 1.4
   */
  public boolean isMCGlobal ()
  {
    // XXX: This seems to net exist with IPv4 addresses
    return false;
  }

  /**
   * Checks if this multicast address has node scope
   * 
   * @since 1.4
   */
  public boolean isMCNodeLocal ()
  {
    // XXX: This seems to net exist with IPv4 addresses
    return false;
  }
  
  /**
   * Checks if this multicast address has link scope
   * 
   * @since 1.4
   */
  public boolean isMCLinkLocal ()
  {
    if (!isMulticastAddress ())
      return false;
    
    return (addr [0] == 0xE0)
	   && (addr [1] == 0x00)
	   && (addr [2] == 0x00);
  }
  
  /**
   * Checks if this multicast address has site scope
   * 
   * @since 1.4
   */
  public boolean isMCSiteLocal ()
  {
    // XXX: This seems to net exist with IPv4 addresses
    return false;
  }
  
  /**
   * Checks if this multicast address has organization scope
   * 
   * @since 1.4
   */
  public boolean isMCOrgLocal ()
  {
    // XXX: This seems to net exist with IPv4 addresses
    return false;
  }
  
  /**
   * Returns the address of the current instance
   */
  public byte[] getAddress ()
  {
    return addr;
  }
  
  /**
   * Returns the address as string
   * 
   * @since 1.0.2
   */
  public String getHostAddress ()
  {
    StringBuffer sbuf = new StringBuffer (40);
    int len = addr.length;
    int i = 0;
    
    for ( ;  ; )
      {
	sbuf.append (addr [i] & 0xFF);
	i++;
	
	if (i == len)
	  break;
	
	sbuf.append ('.');
      }
    
    return sbuf.toString ();
  }
  
  /**
   * Computes the hashcode of the instance
   */
  public int hashCode ()
  {
    int hash = 0;
    int len = addr.length;
    int i = len > 4 ? len - 4 : 0;
    
    for ( ; i < len;  i++)
      hash = (hash << 8) | (addr [i] & 0xFF);
    
    return hash;
  }
 
  /**
   * Compare the current Inet4Address instance with obj
   * 
   * @param obj Object to compare with
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof InetAddress))
      return false;
    
    byte[] addr1 = addr;
    byte[] addr2 = ((InetAddress) obj).addr;
    
    if (addr1.length != addr2.length)
      return false;
    
    for (int i = addr1.length;  --i >= 0; )
      if (addr1 [i] != addr2 [i])
        return false;
    
    return true;
  }
} // class Inet4Address
