// INetAddress.java -- An Internet Protocol (IP) address.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

/**
 * @author Per Bothner
 * @date January 6, 1999.
 */

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * (The latter turns out to have some errors ...)
 * Status:  Believed complete and correct.
 */

public final class InetAddress
{
  String hostname;
  byte[] address;

  InetAddress (byte[] address, String hostname)
  {
    this.address = address;
    this.hostname = hostname;
  }

  public boolean isMulticastAddress ()
  {
    int len = address.length;
    if (len == 4)
      return (address[0] & 0xF0) == 0xE0;
    if (len == 16)
      return address[0] == (byte) 0xFF;
    return false;
  }

  public String getHostName ()
  {
    if (hostname == null)
      lookup (null, this, false);
    return hostname;
  }

  public byte[] getAddress ()
  {
    // An experiment shows that JDK1.2 returns a different byte array each
    // time.  This makes sense, in terms of security.
    return (byte[]) address.clone();
  }

  /* Helper function due to a CNI limitation.  */
  private static InetAddress[] allocArray (int count)
  {
    return new InetAddress[count];
  }

  /* Helper function due to a CNI limitation.  */
  private static SecurityException checkConnect (String hostname)
  {
    SecurityManager s = System.getSecurityManager();
    if (s == null)
      return null;
    try
      {
	s.checkConnect(hostname, -1);
	return null;
      }
    catch (SecurityException ex)
      {
	return ex;
      }
  }

  public String getHostAddress ()
  {
    StringBuffer sbuf = new StringBuffer(40);
    int len = address.length;
    int i = 0;
    if (len == 16)
      { // An IPv6 address.
	for (;  ;  i += 2)
	  {
	    if (i >= 16)
	      return sbuf.toString();
	    int x = ((address[i] & 0xFF) << 8) | (address[i+1] & 0xFF);
	    boolean empty = sbuf.length() == 0;
	    if (empty)
	      {
		if (i == 10 && x == 0xFFFF)
		  { // IPv4-mapped IPv6 address.
		    sbuf.append(":FFFF:");
		    break;  // Continue as IPv4 address;
		  }
		else if (i == 12)
		  { // IPv4-compatible IPv6 address.
		    sbuf.append(':');
		    break;  // Continue as IPv4 address.
		  }
		else if (i > 0)
		  sbuf.append("::");
	      }
	    else
	      sbuf.append(':');
	    if (x != 0 || i >= 14)
	      sbuf.append(Integer.toHexString(x).toUpperCase());
	  }
      }
    for ( ;  ; )
      {
	sbuf.append(address[i] & 0xFF);
	i++;
	if (i == len)
	  break;
	sbuf.append('.');
      }
    return sbuf.toString();
  }

  public int hashCode()
  {
    // There hashing algorithm is not specified, but a simple experiment
    // shows that it is equal to the address, as a 32-bit big-endian integer.
    int hash = 0;
    int len = address.length;
    int i = len > 4 ? len - 4 : 0;
    for ( ; i < len;  i++)
      hash = (hash << 8) | (address[i] & 0xFF);
    return hash;
  }

  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof InetAddress))
      return false;
    // "The Java Class Libraries" 2nd edition says "If a machine has
    // multiple names instances of InetAddress for different name of
    // that same machine are not equal.  This is because they have
    // different host names."  This violates the description in the
    // JDK 1.2 API documentation.  A little experiementation
    // shows that the latter is correct.
    byte[] addr1 = address;
    byte[] addr2 = ((InetAddress) obj).address;
    if (addr1.length != addr2.length)
      return false;
    for (int i = addr1.length;  --i >= 0;  )
      if (addr1[i] != addr2[i])
	return false;
    return true;
  }

  public String toString()
  {
    return getHostName()+'/'+getHostAddress();
  }

  /** If host is a valid numeric IP address, return the numeric address.
   * Otherwise, return null. */
  private static native byte[] aton (String host);

  private static native InetAddress[] lookup
  (String hostname, InetAddress addr, boolean all);

  public static InetAddress getByName (String host)
    throws UnknownHostException
  {
    if (host == null)
      return getLocalHost();
    byte[] address = aton(host);
    if (address != null)
      return new InetAddress(address, null);
    InetAddress iaddr = new InetAddress(null, host);
    lookup(host, iaddr, false);
    return iaddr;
  }

  public static InetAddress[] getAllByName (String host)
    throws UnknownHostException
  {
    byte[] address = aton(host);
    if (address != null)
      {
	InetAddress[] result = new InetAddress[1];
	result[0] = new InetAddress(address, null);
	return result;
      }
    return lookup(host, null, true);
  }

  private static final byte[] localhostAddress = { 127, 0, 0, 1 };

  private static native String getLocalHostname ();

  private static InetAddress localhost = null;

  public static InetAddress getLocalHost() throws UnknownHostException
  {
    SecurityManager s = System.getSecurityManager();
    // Experimentation shows that JDK1.2 does cache the result.
    // However, if there is a security manager, and the cached result
    // is other than "localhost", we need to check again.
    if (localhost == null
	|| (s != null && localhost.address != localhostAddress))
      getLocalHost(s);
    return localhost;
  }

  private static synchronized void getLocalHost(SecurityManager s)
    throws UnknownHostException
  {
    // Check the localhost cache again, now that we've synchronized.
    if (s == null && localhost != null)
      return;
    String hostname = getLocalHostname();
    if (s != null)
      {
	// "The Java Class Libraries" suggests that if the security
	// manager disallows getting the local host name, then
	// we use the loopback host.
	// However, the JDK 1.2 API claims to throw SecurityException,
	// which seems to suggest SecurityException is *not* caught.
	// In this case, experimentation shows that former is correct.
	try
	  {
	    // This is wrong, if the name returned from getLocalHostname()
	    // is not a fully qualified name.  FIXME.
	    s.checkConnect(hostname, -1);
	  }
	catch (SecurityException ex)
	  {
	    hostname = null;
	  }
      }
    if (hostname != null)
      {
	try
	  {
	    localhost = new InetAddress(null, null);
	    lookup(hostname, localhost, false);
	  }
	catch (Exception ex)
	  {
	  }
      }
    if (localhost == null)
      localhost = new InetAddress (localhostAddress, "localhost");
  }
}
