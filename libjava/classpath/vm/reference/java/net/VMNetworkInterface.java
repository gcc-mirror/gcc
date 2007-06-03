/* VMNetworkInterface.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;

import java.nio.ByteBuffer;
import java.util.HashSet;
import java.util.Set;

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
final class VMNetworkInterface
{
  String name;
  Set<InetAddress> addresses;

  VMNetworkInterface(String name)
  {
    this.name = name;
    addresses = new HashSet<InetAddress>();
  }
  
  /**
   * Creates a dummy instance which represents any network
   * interface.
   */
  public VMNetworkInterface()
  {
    addresses = new HashSet<InetAddress>();
    try
      {
        addresses.add(InetAddress.getByName("0.0.0.0"));
      }
    catch (UnknownHostException _)
      {
        // Cannot happen.
      }
  }
  
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      System.loadLibrary("javanet");
    
    initIds();
  }
  
  private static native void initIds();

  /**
   * Return a list of VM network interface objects.
   *
   * @return The list of network interfaces.
   * @throws SocketException
   */
  public static native VMNetworkInterface[] getVMInterfaces()
    throws SocketException;
  
  private void addAddress(ByteBuffer addr)
    throws SocketException, UnknownHostException
  {
    if (addr.remaining() == 4)
      {
        byte[] ipv4 = new byte[4];
        addr.get(ipv4);
        addresses.add(Inet4Address.getByAddress(ipv4));
      }
    else if (addr.remaining() == 16)
      {
        byte[] ipv6 = new byte[16];
        addr.get(ipv6);
        addresses.add(Inet6Address.getByAddress(ipv6));
      }
    else
      throw new SocketException("invalid interface address");
  }
}
