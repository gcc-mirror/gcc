/* NetworkInterface.java --
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2008 Free Software Foundation, Inc.

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

import gnu.classpath.SystemProperties;

import gnu.java.lang.CPStringBuilder;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;

/**
 * This class models a network interface on the host computer.  A network
 * interface contains a name (typically associated with a specific
 * hardware adapter) and a list of addresses that are bound to it.
 * For example, an ethernet interface may be named "eth0" and have the
 * address 192.168.1.101 assigned to it.
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @since 1.4
 */
public final class NetworkInterface
{
  private final VMNetworkInterface netif;
  
  private NetworkInterface(VMNetworkInterface netif)
    {
    this.netif = netif;
    }
  
  /** Creates an NetworkInterface instance which
   * represents any interface in the system. Its only
   * address is <code>0.0.0.0/0.0.0.0</code>. This
   * method is needed by {@link MulticastSocket#getNetworkInterface}
   */
  static NetworkInterface createAnyInterface()
  {
    return new NetworkInterface(new VMNetworkInterface());
  }
  
  /**
   * Returns the name of the network interface
   *
   * @return The name of the interface.
   */
  public String getName()
  {
    return netif.name;
  }

  /**
   * Returns all available addresses of the network interface
   *
   * If a @see SecurityManager is available all addresses are checked
   * with @see SecurityManager::checkConnect() if they are available.
   * Only <code>InetAddresses</code> are returned where the security manager
   * doesn't throw an exception.
   *
   * @return An enumeration of all addresses.
   */
  public Enumeration<InetAddress> getInetAddresses()
  {
    SecurityManager s = System.getSecurityManager();
    Vector<InetAddress> inetAddresses
      = new Vector<InetAddress>(netif.addresses);

    if (s == null)
      return inetAddresses.elements();

    Vector<InetAddress> tmpInetAddresses = new Vector<InetAddress>(1, 1);

    for (Enumeration<InetAddress> addresses = inetAddresses.elements();
         addresses.hasMoreElements();)
      {
	InetAddress addr = addresses.nextElement();
	try
	  {
	    s.checkConnect(addr.getHostAddress(), -1);
	    tmpInetAddresses.add(addr);
	  }
	catch (SecurityException e)
	  {
	    // Ignore.
	  }
      }

    return tmpInetAddresses.elements();
  }

  /**
   * Returns the display name of the interface
   *
   * @return The display name of the interface
   */
  public String getDisplayName()
  {
    return netif.name;
  }

  /**
   * Returns an network interface by name
   *
   * @param name The name of the interface to return
   * 
   * @return a <code>NetworkInterface</code> object representing the interface,
   * or null if there is no interface with that name.
   *
   * @exception SocketException If an error occurs
   * @exception NullPointerException If the specified name is null
   */
  public static NetworkInterface getByName(String name)
    throws SocketException
  {
    if (name == null)
      throw new NullPointerException();
    VMNetworkInterface[] netifs = VMNetworkInterface.getVMInterfaces();
    for (int i = 0; i < netifs.length; i++)
      {
        if (netifs[i].name.equals(name))
          return new NetworkInterface(netifs[i]);
      }
    return null;
  }

  /**
   * Return a network interface by its address
   *
   * @param addr The address of the interface to return
   *
   * @return the interface, or <code>null</code> if none found
   *
   * @exception SocketException If an error occurs
   * @exception NullPointerException If the specified addess is null
   */
  public static NetworkInterface getByInetAddress(InetAddress addr)
    throws SocketException
  {
    if (addr == null)
      throw new NullPointerException();
    VMNetworkInterface[] netifs = VMNetworkInterface.getVMInterfaces();
    for (int i = 0; i < netifs.length; i++)
      {
        if (netifs[i].addresses.contains(addr))
          return new NetworkInterface(netifs[i]);
      }
    return null;
  }

  /**
   * Return an <code>Enumeration</code> of all available network interfaces
   *
   * @return all interfaces
   * 
   * @exception SocketException If an error occurs
   */
  public static Enumeration<NetworkInterface> getNetworkInterfaces()
    throws SocketException
  {
    VMNetworkInterface[] netifs = VMNetworkInterface.getVMInterfaces();
    Vector<NetworkInterface> networkInterfaces = 
      new Vector<NetworkInterface>(netifs.length);
    for (int i = 0; i < netifs.length; i++)
      {
        if (!netifs[i].addresses.isEmpty())
          networkInterfaces.add(new NetworkInterface(netifs[i]));
      }
    return networkInterfaces.elements();
  }

  /**
   * Checks if the current instance is equal to obj
   *
   * @param obj The object to compare with
   *
   * @return <code>true</code> if equal, <code>false</code> otherwise
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof NetworkInterface))
      return false;

    NetworkInterface tmp = (NetworkInterface) obj;
    
    if (netif.name == null)
      return tmp.netif.name == null;

    return (netif.name.equals(tmp.netif.name)
            && (netif.addresses.equals(tmp.netif.addresses)));
  }

  /**
   * Returns the hashcode of the current instance
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    // FIXME: hash correctly
    int hc = netif.addresses.hashCode();
    
    if (netif.name != null)
      hc += netif.name.hashCode();
    
    return hc;
  }

  /**
   * Returns a string representation of the interface
   *
   * @return the string
   */
  public String toString()
  {
    // FIXME: check if this is correct
    CPStringBuilder result;
    String separator = SystemProperties.getProperty("line.separator");

    result = new CPStringBuilder();
    
    result.append("name: ");
    result.append(getDisplayName());
    result.append(" (").append(getName()).append(") addresses:");
    result.append(separator);

    for (Iterator it = netif.addresses.iterator(); it.hasNext(); )
      {
	InetAddress address = (InetAddress) it.next();
	result.append(address.toString()).append(";").append(separator);
      }

    return result.toString();
  }

  /**
   * Determines whether this interface is ready to transfer data.
   *
   * @return whether the interface is up
  */
  public boolean isUp()
    throws SocketException
  {
    return VMNetworkInterface.isUp(netif.name);
  }

  /**
   * Determines whether this interface does point to point
   * transmission.
   *
   * @return whether the interface does point to point transmission
  */
  public boolean isPointToPoint()
    throws SocketException
  {
    return VMNetworkInterface.isPointToPoint(netif.name);
  }

  /**
   * Determines whether this interface is the loopback interface.
   *
   * @return whether the interface is the loopback interface
  */
  public boolean isLoopback()
    throws SocketException
  {
    return VMNetworkInterface.isLoopback(netif.name);
  }

  /**
   * Determines whether this interface supports multicast transmission.
   *
   * @return whether the interface supports multicast transmission.
  */
  public boolean supportsMulticast()
    throws SocketException
  {
    return VMNetworkInterface.supportsMulticast(netif.name);
  }

}
