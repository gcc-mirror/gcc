// INetAddress.java -- An Internet Protocol (IP) address.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;

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

public final class InetAddress implements java.io.Serializable
{
  // The Serialized Form specifies that an int 'address' is saved/restored.
  // This class uses a byte array internally so we'll just do the conversion
  // at serialization time and leave the rest of the algorithm as is.
  private int address;
  transient byte[] addr;
  String hostName;
  // The field 'family' seems to be the AF_ value.
  // FIXME: Much of the code in the other java.net classes does not make
  // use of this family field.  A better implementation would be to make
  // use of getaddrinfo() and have other methods just check the family
  // field rather than examining the length of the address each time.
  int family;
  private static final long serialVersionUID = 3286316764910316507L;

  private void readObject(ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    ois.defaultReadObject();
    addr = new byte[4];
    addr[3] = (byte) address;
    for (int i = 2; i >= 0; --i)
      addr[i] = (byte) (address >>= 8);
    // Ignore family from serialized data.  Since the saved address is 32 bits
    // the deserialized object will have an IPv4 address i.e. AF_INET family.
    // FIXME: An alternative is to call the aton method on the deserialized
    // hostname to get a new address.  The Serialized Form doc is silent
    // on how these fields are used.
    family = getFamily (addr);
  }

  private void writeObject(ObjectOutputStream oos) throws IOException
  {
    // Build a 32 bit address from the last 4 bytes of a 4 byte IPv4 address
    // or a 16 byte IPv6 address.
    int len = addr.length;
    int i = len - 4;
    for (; i < len; i++)
      address = address << 8 | (((int) addr[i]) & 0xFF);
    oos.defaultWriteObject();
  }

  private static native int getFamily (byte[] address);

  InetAddress (byte[] address, String hostname)
  {
    addr = address;
    hostName = hostname;
    if (address != null)
      family = getFamily (address);
  }

  public boolean isMulticastAddress ()
  {
    int len = addr.length;
    if (len == 4)
      return (addr[0] & 0xF0) == 0xE0;
    if (len == 16)
      return addr[0] == (byte) 0xFF;
    return false;
  }

  public String getHostName ()
  {
    if (hostName == null)
      lookup (null, this, false);
    return hostName;
  }

  public byte[] getAddress ()
  {
    // An experiment shows that JDK1.2 returns a different byte array each
    // time.  This makes sense, in terms of security.
    return (byte[]) addr.clone();
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
    int len = addr.length;
    int i = 0;
    if (len == 16)
      { // An IPv6 address.
	for (;  ;  i += 2)
	  {
	    if (i >= 16)
	      return sbuf.toString();
	    int x = ((addr[i] & 0xFF) << 8) | (addr[i+1] & 0xFF);
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
	sbuf.append(addr[i] & 0xFF);
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
    int len = addr.length;
    int i = len > 4 ? len - 4 : 0;
    for ( ; i < len;  i++)
      hash = (hash << 8) | (addr[i] & 0xFF);
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
    byte[] addr1 = addr;
    byte[] addr2 = ((InetAddress) obj).addr;
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

  static final byte[] zeros = {0,0,0,0};
  /* dummy InetAddress, used to bind socket to any (all) network interfaces */
  static final InetAddress ANY_IF = new InetAddress(zeros, null);
    
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
	|| (s != null && localhost.addr != localhostAddress))
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
