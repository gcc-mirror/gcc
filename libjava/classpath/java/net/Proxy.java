/* Proxy.java -- Represends a proxy for a network connection
   Copyright (C) 2006, 2007  Free Software Foundation, Inc.

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


/**
 * Defines a proxy setting. This setting contains a type (https, socks,
 * direct) and a socket address.
 * 
 * @since 1.5
 */
public class Proxy
{
  /**
   * Represents the proxy type.
   */
  public enum Type 
  { 
    DIRECT, HTTP, SOCKS;

    /**
     * For compatability with Sun's JDK
     */
    private static final long serialVersionUID = -2231209257930100533L;
  }

  public static final Proxy NO_PROXY = new Proxy(Type.DIRECT, null);

  private Type type;
  private SocketAddress address;

  /**
   * Creates a new <code>Proxy</code> object.
   *
   * @param type The type for this proxy
   * @param address The address of this proxy
   */
  public Proxy(Type type, SocketAddress address)
  {
    this.type = type;
    this.address = address;
  }

  /**
   * Returns the socket address for this proxy object.
   *
   * @return the socket address
   */
  public SocketAddress address()
  {
    return address;
  }

  /**
   * Returns the of this proxy instance.
   *
   * @return the type
   *
   * @see Type
   */
  public Type type()
  {
    return type;
  }

  /**
   * Compares the given object with this object.
   *
   * @return <code>true</code> if both objects or equals,
   * <code>false</code> otherwise.
   */
  public final boolean equals(Object obj)
  {
    if (! (obj instanceof Proxy))
      return false;

    Proxy tmp = (Proxy) obj;

    return (type.equals(tmp.type)
	    && (address == null ? tmp.address == null
		: address.equals(tmp.address)));
  }

  /**
   * Returns the hashcode for this <code>Proxy</code> object.
   *
   * @return the hashcode
   */
  public final int hashCode()
  {
    return type.hashCode() ^ (address == null ? 0 : address.hashCode());
  }

  /**
   * Returns a string representation of this <code>Proxy</code> object.
   *
   * @return the string
   */
  public String toString()
  {
    return type.toString() + (address == null ? ""
			      : (":" + address.toString()));
  }
}
