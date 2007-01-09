/* InetAddress.java -- Class to model an Internet address
   Copyright (C) 1998, 1999, 2002, 2004, 2005, 2006
   Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;

/**
 * This class models an Internet address.  It does not have a public
 * constructor.  Instead, new instances of this objects are created
 * using the static methods getLocalHost(), getByName(), and
 * getAllByName().
 *
 * <p>This class fulfills the function of the C style functions gethostname(),
 * gethostbyname(), and gethostbyaddr().  It resolves Internet DNS names
 * into their corresponding numeric addresses and vice versa.</p>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner
 * @author Gary Benson (gbenson@redhat.com)
 *
 * @specnote This class is not final since JDK 1.4
 */
public class InetAddress implements Serializable
{
  private static final long serialVersionUID = 3286316764910316507L;

  /**
   * Dummy InetAddress, used to bind socket to any (all) network interfaces.
   */
  static InetAddress ANY_IF;
  static
  {
    byte[] addr;
    try
      {
	addr = VMInetAddress.lookupInaddrAny();
      }
    catch (UnknownHostException e)
      {
	// Make one up and hope it works.
	addr = new byte[] {0, 0, 0, 0};
      }
    try
      {
	ANY_IF = getByAddress(addr);
      }
    catch (UnknownHostException e)
      {
	throw (InternalError) new InternalError().initCause(e);
      }
    ANY_IF.hostName = ANY_IF.getHostName();
  }
  
  /**
   * Stores static localhost address object.
   */
  static InetAddress LOCALHOST;
  static
  {
    try
      {
	LOCALHOST = getByAddress("localhost", new byte[] {127, 0, 0, 1});
      }
    catch (UnknownHostException e)
      {
	throw (InternalError) new InternalError().initCause(e);
      }
  }    

  /**
   * The Serialized Form specifies that an int 'address' is saved/restored.
   * This class uses a byte array internally so we'll just do the conversion
   * at serialization time and leave the rest of the algorithm as is.
   */
  private int address;

  /**
   * An array of octets representing an IP address.
   */
  transient byte[] addr;

  /**
   * The name of the host for this address.
   */
  String hostName;

  /**
   * Needed for serialization.
   */
  private int family;

  /**
   * Constructor.  Prior to the introduction of IPv6 support in 1.4,
   * methods such as InetAddress.getByName() would return InetAddress
   * objects.  From 1.4 such methods returned either Inet4Address or
   * Inet6Address objects, but for compatibility Inet4Address objects
   * are serialized as InetAddresses.  As such, there are only two
   * places where it is appropriate to invoke this constructor: within
   * subclasses constructors and within Inet4Address.writeReplace().
   *
   * @param ipaddr The IP number of this address as an array of bytes
   * @param hostname The hostname of this IP address.
   * @param family The address family of this IP address.
   */
  InetAddress(byte[] ipaddr, String hostname, int family)
  {
    addr = (null == ipaddr) ? null : (byte[]) ipaddr.clone();
    hostName = hostname;
    this.family = family;
  }

  /**
   * Returns true if this address is a multicast address, false otherwise.
   * An address is multicast if the high four bits are "1110".  These are
   * also known as "Class D" addresses.
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @return true if mulitcast, false if not
   *
   * @since 1.1
   */
  public boolean isMulticastAddress()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if the InetAddress in a wildcard address
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isAnyLocalAddress()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if the InetAddress is a loopback address
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isLoopbackAddress()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if InetAddress is a link local address
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isLinkLocalAddress()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if InetAddress is a site local address
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isSiteLocalAddress()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if InetAddress is a global multicast address
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isMCGlobal()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if InetAddress is a node local multicast address.
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isMCNodeLocal()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if InetAddress is a link local multicast address.
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isMCLinkLocal()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if InetAddress is a site local multicast address.
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isMCSiteLocal()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Utility routine to check if InetAddress is a organization local
   * multicast address.
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @since 1.4
   */
  public boolean isMCOrgLocal()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the hostname for this address.  This will return the IP address
   * as a String if there is no hostname available for this address
   *
   * @return The hostname for this address
   */
  public String getHostName()
  {
    if (hostName == null)
      hostName = getCanonicalHostName();

    return hostName;
  }

  /**
   * Returns the canonical hostname represented by this InetAddress
   */
  String internalGetCanonicalHostName()
  {
    try
      {
	return ResolverCache.getHostByAddr(addr);
      }
    catch (UnknownHostException e)
      {
	return getHostAddress();
      }
  }

  /**
   * Returns the canonical hostname represented by this InetAddress
   * 
   * @since 1.4
   */
  public String getCanonicalHostName()
  {
    String hostname = internalGetCanonicalHostName();

    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        try
	  {
            sm.checkConnect(hostname, -1);
	  }
	catch (SecurityException e)
	  {
	    return getHostAddress();
	  }
      }

    return hostname;
  }

  /**
   * Returns the IP address of this object as a byte array.
   *
   * @return IP address
   */
  public byte[] getAddress()
  {
    // An experiment shows that JDK1.2 returns a different byte array each
    // time.  This makes sense, in terms of security.
    return (byte[]) addr.clone();
  }

  /**
   * Returns the IP address of this object as a String.
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   * 
   * @return The IP address of this object in String form
   *
   * @since 1.0.2
   */
  public String getHostAddress()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns a hash value for this address.  Useful for creating hash
   * tables.  Overrides Object.hashCode()
   *
   * @return A hash value for this address.
   */
  public int hashCode()
  {
    // There hashing algorithm is not specified, but a simple experiment
    // shows that it is equal to the address, as a 32-bit big-endian integer.
    int hash = 0;
    int len = addr.length;
    int i = len > 4 ? len - 4 : 0;

    for (; i < len; i++)
      hash = (hash << 8) | (addr[i] & 0xff);

    return hash;
  }

  /**
   * Tests this address for equality against another InetAddress.  The two
   * addresses are considered equal if they contain the exact same octets.
   * This implementation overrides Object.equals()
   *
   * @param obj The address to test for equality
   *
   * @return true if the passed in object's address is equal to this one's,
   * false otherwise
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof InetAddress))
      return false;

    // "The Java Class Libraries" 2nd edition says "If a machine has
    // multiple names instances of InetAddress for different name of
    // that same machine are not equal.  This is because they have
    // different host names."  This violates the description in the
    // JDK 1.2 API documentation.  A little experimentation
    // shows that the latter is correct.
    byte[] addr2 = ((InetAddress) obj).addr;

    if (addr.length != addr2.length)
      return false;

    for (int i = 0; i < addr.length; i++)
      if (addr[i] != addr2[i])
	return false;

    return true;
  }

  /**
   * Converts this address to a String.  This string contains the IP in
   * dotted decimal form. For example: "127.0.0.1"  This method is equivalent
   * to getHostAddress() and overrides Object.toString()
   *
   * @return This address in String form
   */
  public String toString()
  {
    String addr = getHostAddress();
    String host = (hostName != null) ? hostName : "";
    return host + "/" + addr;
  }

  /**
   * Returns an InetAddress object given the raw IP address.
   *
   * The argument is in network byte order: the highest order byte of the
   * address is in getAddress()[0].
   *
   * @param addr The IP address to create the InetAddress object from
   *
   * @exception UnknownHostException If IP address has illegal length
   *
   * @since 1.4
   */
  public static InetAddress getByAddress(byte[] addr)
    throws UnknownHostException
  {
    return getByAddress(null, addr);
  }

  /**
   * Creates an InetAddress based on the provided host name and IP address.
   * No name service is checked for the validity of the address.
   *
   * @param host The hostname of the InetAddress object to create
   * @param addr The IP address to create the InetAddress object from
   *
   * @exception UnknownHostException If IP address is of illegal length
   *
   * @since 1.4
   */
  public static InetAddress getByAddress(String host, byte[] addr)
    throws UnknownHostException
  {
    if (addr.length == 4)
      return new Inet4Address(addr, host);

    if (addr.length == 16)
      {
	for (int i = 0; i < 12; i++)
	  {
	    if (addr[i] != (i < 10 ? 0 : (byte) 0xFF))
	      return new Inet6Address(addr, host);
	  }
	  
	byte[] ip4addr = new byte[4];
	ip4addr[0] = addr[12];
	ip4addr[1] = addr[13];
	ip4addr[2] = addr[14];
	ip4addr[3] = addr[15];
	return new Inet4Address(ip4addr, host);
      }

    throw new UnknownHostException("IP address has illegal length");
  }

  /**
   * Returns an InetAddress object representing the IP address of
   * the given literal IP address in dotted decimal format such as
   * "127.0.0.1".  This is used by SocketPermission.setHostPort()
   * to parse literal IP addresses without performing a DNS lookup.
   *
   * @param literal The literal IP address to create the InetAddress
   * object from
   *
   * @return The address of the host as an InetAddress object, or
   * null if the IP address is invalid.
   */
  static InetAddress getByLiteral(String literal)
  {
    byte[] address = VMInetAddress.aton(literal);
    if (address == null)
      return null;
    
    try
      {
	return getByAddress(address);
      }
    catch (UnknownHostException e)
      {
	throw (InternalError) new InternalError().initCause(e);
      }
  }

  /**
   * Returns an InetAddress object representing the IP address of the given
   * hostname.  This name can be either a hostname such as "www.urbanophile.com"
   * or an IP address in dotted decimal format such as "127.0.0.1".  If the
   * hostname is null or "", the hostname of the local machine is supplied by
   * default.  This method is equivalent to returning the first element in
   * the InetAddress array returned from GetAllByName.
   *
   * @param hostname The name of the desired host, or null for the local 
   * loopback address.
   *
   * @return The address of the host as an InetAddress object.
   *
   * @exception UnknownHostException If no IP address for the host could
   * be found
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   */
  public static InetAddress getByName(String hostname)
    throws UnknownHostException
  {
    InetAddress[] addresses = getAllByName(hostname);
    return addresses[0];
  }

  /**
   * Returns an array of InetAddress objects representing all the host/ip
   * addresses of a given host, given the host's name.  This name can be
   * either a hostname such as "www.urbanophile.com" or an IP address in
   * dotted decimal format such as "127.0.0.1".  If the value is null, the
   * hostname of the local machine is supplied by default.
   *
   * @param hostname The name of the desired host, or null for the
   * local loopback address.
   *
   * @return All addresses of the host as an array of InetAddress objects.
   *
   * @exception UnknownHostException If no IP address for the host could
   * be found
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   */
  public static InetAddress[] getAllByName(String hostname)
    throws UnknownHostException
  {
    // If null or the empty string is supplied, the loopback address
    // is returned.
    if (hostname == null || hostname.length() == 0)
      return new InetAddress[] {LOCALHOST};

    // Check if hostname is an IP address
    InetAddress address = getByLiteral(hostname);
    if (address != null)
      return new InetAddress[] {address};

    // Perform security check before resolving
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkConnect(hostname, -1);

    // Resolve the hostname
    byte[][] iplist = ResolverCache.getHostByName(hostname);
    if (iplist.length == 0)
      throw new UnknownHostException(hostname);

    InetAddress[] addresses = new InetAddress[iplist.length];
    for (int i = 0; i < iplist.length; i++)
      addresses[i] = getByAddress(hostname, iplist[i]);

    return addresses;
  }

  /**
   * Returns an InetAddress object representing the address of the current
   * host.
   *
   * @return The local host's address
   *
   * @exception UnknownHostException If no IP address for the host could
   * be found
   */
  public static InetAddress getLocalHost() throws UnknownHostException
  {
    String hostname = VMInetAddress.getLocalHostname();
    try
      {
	return getByName(hostname);
      }
    catch (SecurityException e)
      {
	return LOCALHOST;
      }
  }

  /**
   * Inet4Address objects are serialized as InetAddress objects.
   * This deserializes them back into Inet4Address objects.
   */
  private Object readResolve() throws ObjectStreamException
  {
    return new Inet4Address(addr, hostName);
  }

  private void readObject(ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    ois.defaultReadObject();
    addr = new byte[4];
    addr[3] = (byte) address;

    for (int i = 2; i >= 0; --i)
      addr[i] = (byte) (address >>= 8);
  }

  private void writeObject(ObjectOutputStream oos) throws IOException
  {
    // Build a 32 bit address from the last 4 bytes of a 4 byte IPv4 address
    // or a 16 byte IPv6 address.
    int len = addr.length;
    int i = len - 4;

    for (; i < len; i++)
      address = address << 8 | (addr[i] & 0xff);

    oos.defaultWriteObject();
  }
}
