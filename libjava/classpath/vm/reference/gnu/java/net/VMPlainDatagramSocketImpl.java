/* PlainDatagramSocketImpl.java -- VM interface for DatagramSocket impl
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.java.net;

import gnu.classpath.Configuration;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketAddress;
import java.net.SocketException;

/**
 * The VM interface for {@link gnu.java.net.PlainDatagramSocketImpl}.
 *
 * @author Ingo Proetel (proetel@aicas.com)
 * @author Roman Kennke (kennke@aicas.com)
 */
public final class VMPlainDatagramSocketImpl
{
  /**
   * Option id for the IP_TTL (time to live) value.
   */
  static final int IP_TTL = 0x1E61; // 7777


  // Static initializer to load native library
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javanet");
      }
  }

  /**
   * Binds this socket to a particular port and interface
   *
   * @param socket the socket object
   * @param port the port to bind to
   * @param addr the address to bind to
   *
   * @throws SocketException If an error occurs
   */
  static native void bind(PlainDatagramSocketImpl socket, int port,
                          InetAddress addr)
    throws SocketException;

  /**
   * Creates a new datagram socket.
   *
   * @param socket the socket object
   *
   * @throws SocketException If an error occurs
   */
  static native void create(PlainDatagramSocketImpl socket)
    throws SocketException;

  /**
   * Connects to the remote address and port specified as arguments.
   *
   * @param socket the socket object
   * @param addr the remote address to connect to
   * @param port the remote port to connect to
   *
   * @throws SocketException If an error occurs
   */
  static native void connect(PlainDatagramSocketImpl socket, InetAddress addr,
                             int port)
    throws SocketException;

  /**
   * Sends a packet of data to a remote host.
   *
   * @param socket the socket object
   * @param packet the packet to send
   *
   * @throws IOException If an error occurs
   */
  static void send(PlainDatagramSocketImpl socket, DatagramPacket packet)
    throws IOException
  {
    nativeSendTo(socket, packet.getAddress(), packet.getPort(),
                 packet.getData(), packet.getOffset(), packet.getLength());
  }


  /**
   * Sends a packet of data to a remote host.
   *
   * @param socket the socket object
   * @param addr the address to send to
   * @param port the port to send to
   * @param buf the buffer to send
   * @param offset the offset of the data in the buffer to send
   * @param len the length of the data to send
   *
   * @throws IOException If an error occurs
   */
  private static native void nativeSendTo(PlainDatagramSocketImpl socket,
                                          InetAddress addr, int port,
                                          byte[] buf, int offset, int len)
    throws IOException;

  /**
   * Receives a UDP packet from the network
   *
   * @param socket the socket object
   * @param packet the packet to fill in with the data received
   *
   * @throws IOException IOException If an error occurs
   */
  static void receive(PlainDatagramSocketImpl socket, DatagramPacket packet)
    throws IOException
  {
    byte[] receiveFromAddress = new byte[4];
    int[] receiveFromPort = new int[1];
    int[] receivedLength = new int[1];

    nativeReceive(socket, packet.getData(), packet.getOffset(),
                  packet.getLength(),
                  receiveFromAddress, receiveFromPort, receivedLength);

    packet.setAddress(InetAddress.getByAddress(receiveFromAddress));
    packet.setPort(receiveFromPort[0]);
    packet.setLength(receivedLength[0]);
  }

  private static native void nativeReceive(PlainDatagramSocketImpl socket,
                                           byte[] buf, int offset, int len,
                                           byte[] receiveFromAddress,
                                           int[] receiveFromPort,
                                           int[] receivedLength)
    throws IOException;

  /**
   * Sets the value of an option on the socket
   *
   * @param socket the socket object
   * @param optionId the identifier of the option to set
   * @param value the value of the option to set
   *
   * @exception SocketException If an error occurs
   */
  static native void setOption(PlainDatagramSocketImpl socket, int optionId,
                               Object value)
    throws SocketException;

  /**
   * Retrieves the value of an option on the socket.
   *
   * @param socket the socket object
   * @param optionId the identifier of the option to retrieve
   *
   * @return the value of the option
   *
   * @throws SocketException if an error occurs
   */
  static native Object getOption(PlainDatagramSocketImpl socket, int optionId)
    throws SocketException;

  /**
   * Closes the socket.
   *
   * @param socket the socket object
   */
  static native void close(PlainDatagramSocketImpl socket);

  /**
   * Joins a multicast group
   *
   * @param addr The group to join
   *
   * @exception IOException If an error occurs
   */
  static native void join(PlainDatagramSocketImpl socket, InetAddress addr)
    throws IOException;

  /**
   * Leaves a multicast group
   *
   * @param addr The group to leave
   *
   * @exception IOException If an error occurs
   */
  static native void leave(PlainDatagramSocketImpl socket, InetAddress addr)
    throws IOException;

  /**
   * Joins a multicast group.
   *
   * @param socket the socket object
   * @param address the socket address
   * @param netIf the network interface
   *
   * @throws IOException if I/O errors occur
   */
  static void joinGroup(PlainDatagramSocketImpl socket, SocketAddress address,
                        NetworkInterface netIf)
    throws IOException
  {
    throw new InternalError
      ("PlainDatagramSocketImpl::joinGroup is not implemented");
  }

  /**
   * Leaves a multicast group.
   *
   * @param socket the socket object
   * @param address the socket address
   * @param netIf the network interface
   *
   * @throws IOException if I/O errors occur
   */
  static void leaveGroup(PlainDatagramSocketImpl socket, SocketAddress address,
                         NetworkInterface netIf)
    throws IOException
  {
    throw new InternalError
      ("PlainDatagramSocketImpl::leaveGroup is not implemented");
  }

}
