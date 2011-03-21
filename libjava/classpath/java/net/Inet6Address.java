/* Inet6Address.java --
   Copyright (C) 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

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

import java.util.Arrays;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;

/*
 * Written using on-line Java Platform 1.4 API Specification and
 * RFC 1884 (http://www.ietf.org/rfc/rfc1884.txt)
 *
 * @author Michael Koch
 * @status Updated to 1.5. Serialization compatibility is tested.
 */
public final class Inet6Address extends InetAddress
{
  static final long serialVersionUID = 6880410070516793377L;

  /**
   * Needed for serialization
   */
  byte[] ipaddress;

  /**
   * The scope ID, if any.
   * @since 1.5
   * @serial
   */
  private int scope_id;

  /**
   * The scope ID, if any.
   * @since 1.5
   * @serial
   */
  private boolean scope_id_set;

  /**
   * Whether ifname is set or not.
   * @since 1.5
   * @serial
   */
  private boolean scope_ifname_set;

  /**
   * Name of the network interface, used only by the serialization methods
   * @since 1.5
   * @serial
   */
  private String ifname;

  /**
   * Scope network interface, or <code>null</code>.
   */
  private transient NetworkInterface nif;

  /**
   * The address family of these addresses (used for serialization).
   */
  private static final int AF_INET6 = 10;

  /**
   * Create an Inet6Address object
   *
   * @param addr The IP address
   * @param host The hostname
   */
  Inet6Address(byte[] addr, String host)
  {
    super(addr, host, AF_INET6);
    // Super constructor clones the addr.  Get a reference to the clone.
    this.ipaddress = this.addr;
    ifname = null;
    scope_ifname_set = scope_id_set = false;
    scope_id = 0;
    nif = null;
  }

  /**
   * Utility routine to check if the InetAddress is an IP multicast address
   *
   * @since 1.1
   */
  public boolean isMulticastAddress()
  {
    return ipaddress[0] == (byte) 0xFF;
  }

  /**
   * Utility routine to check if the InetAddress in a wildcard address
   *
   * @since 1.4
   */
  public boolean isAnyLocalAddress()
  {
    byte[] anylocal = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

    return Arrays.equals(ipaddress, anylocal);
  }

  /**
   * Utility routine to check if the InetAddress is a loopback address
   *
   * @since 1.4
   */
  public boolean isLoopbackAddress()
  {
    byte[] loopback = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };

    return Arrays.equals(ipaddress, loopback);
  }

  /**
   * Utility routine to check if the InetAddress is an link local address
   *
   * @since 1.4
   */
  public boolean isLinkLocalAddress()
  {
    return ipaddress[0] == 0xFA;
  }

  /**
   * Utility routine to check if the InetAddress is a site local address
   *
   * @since 1.4
   */
  public boolean isSiteLocalAddress()
  {
    return ipaddress[0] == 0xFB;
  }

  /**
   * Utility routine to check if the multicast address has global scope
   *
   * @since 1.4
   */
  public boolean isMCGlobal()
  {
    if (! isMulticastAddress())
      return false;

    return (ipaddress[1] & 0x0F) == 0xE;
  }

  /**
   * Utility routine to check if the multicast address has node scope
   *
   * @since 1.4
   */
  public boolean isMCNodeLocal()
  {
    if (! isMulticastAddress())
      return false;

    return (ipaddress[1] & 0x0F) == 0x1;
  }

  /**
   * Utility routine to check if the multicast address has link scope
   *
   * @since 1.4
   */
  public boolean isMCLinkLocal()
  {
    if (! isMulticastAddress())
      return false;

    return (ipaddress[1] & 0x0F) == 0x2;
  }

  /**
   * Utility routine to check if the multicast address has site scope
   *
   * @since 1.4
   */
  public boolean isMCSiteLocal()
  {
    if (! isMulticastAddress())
      return false;

    return (ipaddress[1] & 0x0F) == 0x5;
  }

  /**
   * Utility routine to check if the multicast address has organization scope
   *
   * @since 1.4
   */
  public boolean isMCOrgLocal()
  {
    if (! isMulticastAddress())
      return false;

    return (ipaddress[1] & 0x0F) == 0x8;
  }

  /**
   * Returns the raw IP address of this InetAddress object. The result is in
   * network byte order: the highest order byte of the address is i
   * n getAddress()[0]
   */
  public byte[] getAddress()
  {
    return (byte[]) ipaddress.clone();
  }

  /**
   * Creates a scoped Inet6Address where the scope has an integer id.
   *
   * @throws UnkownHostException if the address is an invalid number of bytes.
   * @since 1.5
   */
  public static Inet6Address getByAddress(String host, byte[] addr,
                                          int scopeId)
    throws UnknownHostException
  {
    if( addr.length != 16 )
      throw new UnknownHostException("Illegal address length: " + addr.length
                                     + " bytes.");
    Inet6Address ip = new Inet6Address( addr, host );
    ip.scope_id = scopeId;
    ip.scope_id_set = true;
    return ip;
  }

  /**
   * Creates a scoped Inet6Address where the scope is a given
   * NetworkInterface.
   *
   * @throws UnkownHostException if the address is an invalid number of bytes.
   * @since 1.5
   */
  public static Inet6Address getByAddress(String host, byte[] addr,
                                          NetworkInterface nif)
    throws UnknownHostException
  {
    if( addr.length != 16 )
      throw new UnknownHostException("Illegal address length: " + addr.length
                                     + " bytes.");
    Inet6Address ip = new Inet6Address( addr, host );
    ip.nif = nif;

    return ip;
  }

  /**
   * Returns the <code>NetworkInterface</code> of the address scope
   * if it is a scoped address and the scope is given in the form of a
   * NetworkInterface.
   * (I.e. the address was created using  the
   * getByAddress(String, byte[], NetworkInterface) method)
   * Otherwise this method returns <code>null</code>.
   * @since 1.5
   */
  public NetworkInterface getScopedInterface()
  {
    return nif;
  }

  /**
   * Returns the scope ID of the address scope if it is a scoped adress using
   * an integer to identify the scope.
   *
   * Otherwise this method returns 0.
   * @since 1.5
   */
  public int getScopeId()
  {
    // check scope_id_set because some JDK-serialized objects seem to have
    // scope_id set to a nonzero value even when scope_id_set == false
    if( scope_id_set )
      return scope_id;
    return 0;
  }

  /**
   * Returns the IP address string in textual presentation
   */
  public String getHostAddress()
  {
    CPStringBuilder sbuf = new CPStringBuilder(40);

    for (int i = 0; i < 16; i += 2)
      {
        int x = ((ipaddress[i] & 0xFF) << 8) | (ipaddress[i + 1] & 0xFF);

        if (i > 0)
          sbuf.append(':');

        sbuf.append(Integer.toHexString(x));
      }
    if( nif != null )
      sbuf.append( "%" + nif.getName() );
    else if( scope_id_set )
      sbuf.append( "%" + scope_id );

    return sbuf.toString();
  }

  /**
   * Returns a hashcode for this IP address
   * (The hashcode is independent of scope)
   */
  public int hashCode()
  {
    return super.hashCode();
  }

  /**
   * Compares this object against the specified object
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof Inet6Address))
      return false;

    Inet6Address ip = (Inet6Address)obj;
    if (ipaddress.length != ip.ipaddress.length)
      return false;

    for (int i = 0; i < ip.ipaddress.length; i++)
      if (ipaddress[i] != ip.ipaddress[i])
        return false;

    if( ip.nif != null && nif != null )
      return nif.equals( ip.nif );
    if( ip.nif != nif )
      return false;
    if( ip.scope_id_set != scope_id_set )
      return false;
    if( scope_id_set )
      return (scope_id == ip.scope_id);
    return true;
  }

  /**
   * Utility routine to check if the InetAddress is an
   * IPv4 compatible IPv6 address
   *
   * @since 1.4
   */
  public boolean isIPv4CompatibleAddress()
  {
    if (ipaddress[0] != 0x00 || ipaddress[1] != 0x00 || ipaddress[2] != 0x00
        || ipaddress[3] != 0x00 || ipaddress[4] != 0x00
        || ipaddress[5] != 0x00 || ipaddress[6] != 0x00
        || ipaddress[7] != 0x00 || ipaddress[8] != 0x00
        || ipaddress[9] != 0x00 || ipaddress[10] != 0x00
        || ipaddress[11] != 0x00)
      return false;

    return true;
  }

  /**
   * Required for 1.5-compatible serialization.
   * @since 1.5
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    try
      {
        if( scope_ifname_set )
          nif = NetworkInterface.getByName( ifname );
      }
    catch( SocketException se )
      {
        // FIXME: Ignore this? or throw an IOException?
      }
  }

  /**
   * Required for 1.5-compatible serialization.
   * @since 1.5
   */
  private void writeObject(ObjectOutputStream s)
    throws IOException
  {
    if( nif != null )
      {
        ifname = nif.getName();
        scope_ifname_set = true;
      }
    s.defaultWriteObject();
  }
}
