/* ResolverCache.java -- A cache of resolver lookups for InetAddress.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import java.security.Security;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * This class provides a cache of name service resolutions.  By
 * default successful resolutions are cached forever to guard
 * against DNS spoofing attacks and failed resolutions are cached
 * for 10 seconds to improve performance.  The length of time that
 * results remain in the cache is determined by the following
 * security properties:
 * <dl>
 *   <dt><code>networkaddress.cache.ttl</code></dt>
 *   <dd>
 *     This property specifies the length of time in seconds that
 *     successful resolutions remain in the cache.  The default is
 *     -1, indicating to cache forever.
 *   </dd>
 *   <dt><code>networkaddress.cache.negative.ttl</code></dt>
 *   <dd>
 *     This property specifies the length of time in seconds that
 *     unsuccessful resolutions remain in the cache.  The default
 *     is 10, indicating to cache for 10 seconds.
 *   </dd>
 * In both cases, a value of -1 indicates to cache forever and a
 * value of 0 indicates not to cache.
 *
 * @author Gary Benson (gbenson@redhat.com)
 */
class ResolverCache
{
  /**
   * The time in seconds for which successful lookups are cached.
   */
  private static final int POSITIVE_TTL =
    getTTL("networkaddress.cache.ttl", -1);

  /**
   * The time in seconds for which unsuccessful lookups are cached.
   */
  private static final int NEGATIVE_TTL =
    getTTL("networkaddress.cache.negative.ttl", 10);

  /**
   * Helper function to set the TTLs.
   */
  private static int getTTL(String propName, int defaultValue)
  {
    String propValue = Security.getProperty(propName);
    if (propValue == null)
      return defaultValue;

    return Integer.parseInt(propValue);
  }

  /**
   * The cache itself.
   */
  private static HashMap<Object, Entry> cache = new HashMap<Object, Entry>();

  /**
   * List of entries which may expire.
   */
  private static LinkedList<Entry> killqueue = new LinkedList<Entry>();

  /**
   * Return the hostname for the specified IP address.
   *
   * @param addr The IP address as a byte array
   *
   * @return The hostname
   *
   * @exception UnknownHostException If the reverse lookup fails
   */
  public static String getHostByAddr(byte[] addr) throws UnknownHostException
  {
    Object key = makeHashableAddress(addr);
    Entry entry = get(key);
    if (entry != null)
      {
	if (entry.value == null)
	  throw new UnknownHostException();
	return (String) entry.value;
      }

    try
      {
	String hostname = VMInetAddress.getHostByAddr(addr);
	put(new Entry(key, hostname));
	return hostname;
      }
    catch (UnknownHostException e)
      {
	put(new Entry(key, null));
	throw e;
      }
  }

  /**
   * Return a list of all IP addresses for the specified hostname.
   *
   * @param hostname The hostname
   *
   * @return An list of IP addresses as byte arrays
   *
   * @exception UnknownHostException If the lookup fails
   */
  public static byte[][] getHostByName(String hostname)
    throws UnknownHostException
  {
    Entry entry = get(hostname);
    if (entry != null)
      {
	if (entry.value == null)
	  throw new UnknownHostException();
	return (byte[][]) entry.value;
      }

    try
      {
	byte[][] addrs = VMInetAddress.getHostByName(hostname);
	put(new Entry(hostname, addrs));
	return addrs;
      }
    catch (UnknownHostException e)
      {
	put(new Entry(hostname, null));
	throw e;
      }
  }

  /**
   * Convert an IP address expressed as a byte array into something
   * we can use as a hashtable key.
   */
  private static Object makeHashableAddress(byte[] addr)
  {
    char[] chars = new char[addr.length];
    for (int i = 0; i < addr.length; i++)
      chars[i] = (char) addr[i];
    return new String(chars);
  }

  /**
   * Return the entry in the cache associated with the supplied key,
   * or <code>null</code> if the cache does not contain an entry
   * associated with this key.
   */
  private static synchronized Entry get(Object key)
  {
    reap();
    return (Entry) cache.get(key);
  }

  /**
   * Insert the supplied entry into the cache.
   */
  private static synchronized void put(Entry entry)
  {
    reap();
    if (entry.expires != 0)
      {
	if (entry.expires != -1)
	  killqueue.add(entry);
	cache.put(entry.key, entry);
      }
  }

  /**
   * Clear expired entries.  This method is not synchronized, so
   * it must only be called by methods that are.
   */
  private static void reap()
  {
    if (!killqueue.isEmpty())
      {
	long now = System.currentTimeMillis();

	Iterator iter = killqueue.iterator();
	while (iter.hasNext())
	  {
	    Entry entry = (Entry) iter.next();
	    if (entry.expires > now)
	      break;
	    cache.remove(entry.key);
	    iter.remove();
	  }
      }
  }
  
  /**
   * An entry in the cache.
   */
  private static class Entry
  {
    /**
     * The key by which this entry is referenced.
     */
    public final Object key;

    /**
     * The entry itself.  A null value indicates a failed lookup.
     */
    public final Object value;
    
    /**
     * The time when this cache entry expires.  If set to -1 then
     * this entry will never expire.  If set to 0 then this entry
     * expires immediately and will not be inserted into the cache.
     */
    public final long expires;

    /**
     * Constructor.
     */
    public Entry(Object key, Object value)
    {
      this.key = key;
      this.value = value;

      int ttl = value != null ? POSITIVE_TTL : NEGATIVE_TTL;
      if (ttl < 1)
	expires = ttl;
      else
	expires = System.currentTimeMillis() + ttl * 1000;
    }
  }
}
