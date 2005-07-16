/* InetAddress.java -- Class to model an Internet address
   Copyright (C) 1998, 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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
import java.util.HashMap;
import java.util.StringTokenizer;

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
 *
 * @specnote This class is not final since JK 1.4
 */
public class InetAddress implements Serializable
{
  private static final long serialVersionUID = 3286316764910316507L;

  /**
   * The default DNS hash table size,
   * Use a prime number happy with hash table.
   */
  private static final int DEFAULT_CACHE_SIZE = 89;

  /**
   * The default caching period in minutes.
   */
  private static final int DEFAULT_CACHE_PERIOD = 4 * 60;

  /**
   * Percentage of cache entries to purge when the table gets full.
   */
  private static final int DEFAULT_CACHE_PURGE_PCT = 30;

  /**
   * The special IP address INADDR_ANY.
   */
  private static InetAddress inaddr_any;

  /**
   * Dummy InetAddress, used to bind socket to any (all) network interfaces.
   */
  static InetAddress ANY_IF;

  /**
   * Stores static localhost address object.
   */
  static InetAddress LOCALHOST;

  /**
   * The size of the cache.
   */
  private static int cache_size = 0;

  /**
   * The length of time we will continue to read the address from cache
   * before forcing another lookup.
   */
  private static int cache_period = 0;

  /**
   * What percentage of the cache we will purge if it gets full.
   */
  private static int cache_purge_pct = 0;

  /**
   * HashMap to use as DNS lookup cache.
   * Use HashMap because all accesses to cache are already synchronized.
   */
  private static HashMap cache;

  static
  {
    // Look for properties that override default caching behavior
    cache_size =
      Integer.getInteger("gnu.java.net.dns_cache_size", DEFAULT_CACHE_SIZE)
             .intValue();
    cache_period =
      Integer.getInteger("gnu.java.net.dns_cache_period",
                         DEFAULT_CACHE_PERIOD * 60 * 1000).intValue();

    cache_purge_pct =
      Integer.getInteger("gnu.java.net.dns_cache_purge_pct",
                         DEFAULT_CACHE_PURGE_PCT).intValue();

    // Fallback to  defaults if necessary
    if ((cache_purge_pct < 1) || (cache_purge_pct > 100))
      cache_purge_pct = DEFAULT_CACHE_PURGE_PCT;

    // Create the cache
    if (cache_size != 0)
      cache = new HashMap(cache_size);

    // precompute the ANY_IF address
    try
      {
        ANY_IF = getInaddrAny();

	byte[] ip_localhost = { 127, 0, 0, 1 };
	LOCALHOST = new Inet4Address(ip_localhost, "localhost");
      }
    catch (UnknownHostException uhe)
      {
        // Hmmm, make one up and hope that it works.
        byte[] zeros = { 0, 0, 0, 0 };
        ANY_IF = new Inet4Address(zeros, "0.0.0.0");
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
   * The time this address was looked up.
   */
  transient long lookup_time;

  /**
   * The field 'family' seems to be the AF_ value.
   * FIXME: Much of the code in the other java.net classes does not make
   * use of this family field.  A better implementation would be to make
   * use of getaddrinfo() and have other methods just check the family
   * field rather than examining the length of the address each time.
   */
  int family;

  /**
   * Initializes this object's addr instance variable from the passed in
   * byte array.  Note that this constructor is protected and is called
   * only by static methods in this class.
   *
   * @param ipaddr The IP number of this address as an array of bytes
   * @param hostname The hostname of this IP address.
   */
  InetAddress(byte[] ipaddr, String hostname)
  {
    addr = (null == ipaddr) ? null : (byte[]) ipaddr.clone();
    hostName = hostname;
    
    lookup_time = System.currentTimeMillis();

    family = 2; /* AF_INET */
  }

  /**
   * Returns true if this address is a multicast address, false otherwise.
   * An address is multicast if the high four bits are "1110".  These are
   * also known as "Class D" addresses.
   *
   * @return true if mulitcast, false if not
   *
   * @since 1.1
   */
  public boolean isMulticastAddress()
  {
    // Mask against high order bits of 1110
    if (addr.length == 4)
      return (addr[0] & 0xf0) == 0xe0;

    return false;
  }

  /**
   * Utility routine to check if the InetAddress in a wildcard address
   *
   * @since 1.4
   */
  public boolean isAnyLocalAddress()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    return equals(ANY_IF);
  }

  /**
   * Utility routine to check if the InetAddress is a loopback address
   *
   * @since 1.4
   */
  public boolean isLoopbackAddress()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    return (addr[0] & 0xff) == 0x7f;
  }

  /**
   * Utility routine to check if InetAddress is a link local address
   *
   * @since 1.4
   */
  public boolean isLinkLocalAddress()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    // XXX: This seems to not exist with IPv4 addresses
    return false;
  }

  /**
   * Utility routine to check if InetAddress is a site local address
   *
   * @since 1.4
   */
  public boolean isSiteLocalAddress()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.

    // 10.0.0.0/8
    if ((addr[0] & 0xff) == 0x0a)
      return true;

    // 172.16.0.0/12
    if ((addr[0] & 0xff) == 0xac && (addr[1] & 0xf0) == 0x10)
      return true;

    // 192.168.0.0/16
    if ((addr[0] & 0xff) == 0xc0 && (addr[1] & 0xff) == 0xa8)
      return true;

    // XXX: Do we need to check more addresses here ?
    return false;
  }

  /**
   * Utility routine to check if InetAddress is a global multicast address
   *
   * @since 1.4
   */
  public boolean isMCGlobal()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    // XXX: This seems to not exist with IPv4 addresses
    return false;
  }

  /**
   * Utility routine to check if InetAddress is a node local multicast address.
   *
   * @since 1.4
   */
  public boolean isMCNodeLocal()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    // XXX: This seems to not exist with IPv4 addresses
    return false;
  }

  /**
   * Utility routine to check if InetAddress is a link local multicast address.
   *
   * @since 1.4
   */
  public boolean isMCLinkLocal()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    if (! isMulticastAddress())
      return false;

    return ((addr[0] & 0xff) == 0xe0
	    && (addr[1] & 0xff)  == 0x00
	    && (addr[2] & 0xff)  == 0x00);
  }

  /**
   * Utility routine to check if InetAddress is a site local multicast address.
   *
   * @since 1.4
   */
  public boolean isMCSiteLocal()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    // XXX: This seems to not exist with IPv4 addresses
    return false;
  }

  /**
   * Utility routine to check if InetAddress is a organization local
   * multicast address.
   *
   * @since 1.4
   */
  public boolean isMCOrgLocal()
  {
    // This is the IPv4 implementation.
    // Any class derived from InetAddress should override this.
    // XXX: This seems to not exist with IPv4 addresses
    return false;
  }

  /**
   * Returns the hostname for this address.  This will return the IP address
   * as a String if there is no hostname available for this address
   *
   * @return The hostname for this address
   */
  public String getHostName()
  {
    if (hostName != null)
      return hostName;

    try
      {
	hostName = VMInetAddress.getHostByAddr(addr);
	return hostName;
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
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        try
	  {
            sm.checkConnect(hostName, -1);
	  }
	catch (SecurityException e)
	  {
	    return getHostAddress();
	  }
      }

    // Try to find the FDQN now
    InetAddress address;
    byte[] ipaddr = getAddress();

    if (ipaddr.length == 16)
      address = new Inet6Address(getAddress(), null);
    else
      address = new Inet4Address(getAddress(), null);

    return address.getHostName();
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
   * Returns the IP address of this object as a String.  The address is in
   * the dotted octet notation, for example, "127.0.0.1".
   *
   * @return The IP address of this object in String form
   *
   * @since 1.0.2
   */
  public String getHostAddress()
  {
    StringBuffer sb = new StringBuffer(40);

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
      return new Inet6Address(addr, host);

    throw new UnknownHostException("IP address has illegal length");
  }

  /**
   * If hostname is a valid numeric IP address, return the numeric address.
   * Otherwise, return null.
   *
   * @param hostname the name of the host
   */
  private static byte[] aton(String hostname)
  {
    StringTokenizer st = new StringTokenizer(hostname, ".");

    if (st.countTokens() == 4)
      {
	int index;
	byte[] address = new byte[4];

	for (index = 0; index < 4; index++)
	  {
	    try
	      {
		short n = Short.parseShort(st.nextToken());

		if ((n < 0) || (n > 255))
		  break;

		address[index] = (byte) n;
	      }
	    catch (NumberFormatException e)
	      {
		break;
	      }
	  }

	if (index == 4)
	  return address;
      }

    return null;
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
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkConnect(hostname, -1);

    InetAddress[] addresses;

    // Default to current host if necessary
    if (hostname == null)
      {
	addresses = new InetAddress[1];
	addresses[0] = LOCALHOST;
	return addresses;
      }

    // Check the cache for this host before doing a lookup
    addresses = checkCacheFor(hostname);

    if (addresses != null)
      return addresses;

    // Not in cache, try the lookup
    byte[][] iplist = VMInetAddress.getHostByName(hostname);

    if (iplist.length == 0)
      throw new UnknownHostException(hostname);

    addresses = new InetAddress[iplist.length];

    for (int i = 0; i < iplist.length; i++)
      {
	if (iplist[i].length != 4)
	  throw new UnknownHostException(hostname);

	addresses[i] = new Inet4Address(iplist[i], hostname);
      }

    addToCache(hostname, addresses);
    return addresses;
  }

  /**
   * This method checks the DNS cache to see if we have looked this hostname
   * up before. If so, we return the cached addresses unless it has been in the
   * cache too long.
   *
   * @param hostname The hostname to check for
   *
   * @return The InetAddress for this hostname or null if not available
   */
  private static synchronized InetAddress[] checkCacheFor(String hostname)
  {
    InetAddress[] addresses = null;

    if (cache_size == 0)
      return null;

    Object obj = cache.get(hostname);
    if (obj == null)
      return null;

    if (obj instanceof InetAddress[])
      addresses = (InetAddress[]) obj;

    if (addresses == null)
      return null;

    if (cache_period != -1)
      if ((System.currentTimeMillis() - addresses[0].lookup_time) > cache_period)
        {
	  cache.remove(hostname);
	  return null;
        }

    return addresses;
  }

  /**
   * This method adds an InetAddress object to our DNS cache.  Note that
   * if the cache is full, then we run a purge to get rid of old entries.
   * This will cause a performance hit, thus applications using lots of
   * lookups should set the cache size to be very large.
   *
   * @param hostname The hostname to cache this address under
   * @param obj The InetAddress or InetAddress array to store
   */
  private static synchronized void addToCache(String hostname, Object obj)
  {
    if (cache_size == 0)
      return;

    // Check to see if hash table is full
    if (cache_size != -1)
      if (cache.size() == cache_size)
        {
	  // FIXME Add code to purge later.
        }

    cache.put(hostname, obj);
  }

  /**
   * Returns the special address INADDR_ANY used for binding to a local
   * port on all IP addresses hosted by a the local host.
   *
   * @return An InetAddress object representing INDADDR_ANY
   *
   * @exception UnknownHostException If an error occurs
   */
  static InetAddress getInaddrAny() throws UnknownHostException
  {
    if (inaddr_any == null)
      {
	byte[] tmp = VMInetAddress.lookupInaddrAny();
	inaddr_any = new Inet4Address(tmp, null);
      }

    return inaddr_any;
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
    return getByName(hostname);
  }

  /*
   * Needed for serialization
   */
  private void readResolve() throws ObjectStreamException
  {
    // FIXME: implement this
  }

  private void readObject(ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    ois.defaultReadObject();
    addr = new byte[4];
    addr[3] = (byte) address;

    for (int i = 2; i >= 0; --i)
      addr[i] = (byte) (address >>= 8);

    family = 2; /* AF_INET  */
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
