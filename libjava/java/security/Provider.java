/* Provider.java -- Security provider information
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.security;

import java.io.Serializable;
import java.util.Properties;

/**
 * This class represents a Java security architecture service provider.
 * The services provided by a such a provider can range from security
 * algorithms to key generation.
 * <p>
 * Providers are installed by name and verion number.  There is one
 * standard provider supplied with the class library.  This is the
 * "GNU" provider, which can also be accessed by the alias "SUN" for
 * compatibility with the JDK.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public abstract class Provider extends Properties implements Serializable
{
  /**
   * This is a textual description of the provider
   */
  private String info;

  /**
   * This is the name of the provider
   */
  private String name;

  /**
   * This is the version number of the provider
   */
  private double version;

  /**
   * This method initializes a new instance of <code>Provider</code> to have
   * the specified name, version, and description information.
   *
   * @param name The name to assign to this <code>Provider</code>.
   * @param version The version number for this <code>Provider</code>.
   * @param info A textual description of this provider.
   */
  protected Provider(String name, double version, String info)
  {
    this.name = name;
    this.version = version;
    this.info = info;
  }

  /**
   * This method returns the name assigned to this <code>Provider</code>.
   *
   * @return The <code>Provider</code>'s name.
   */
  public String getName()
  {
    return (name);
  }

  /**
   * This method retunrs the version number of this <code>Provider</code>.
   * 
   * @return The <code>Provider</code>'s version number.
   */
  public double getVersion()
  {
    return (version);
  }

  /**
   * This method returns a textual description of the <code>Provider</code>.
   *
   * @return A description of the <code>Provider</code>.
   */
  public String getInfo()
  {
    return (info);
  }

  /**
   * This method sets the specified key to have the specified value.
   *
   * @param key The property key
   * @param value The property value
   *
   * @return The previous value for this key, or <code>null</code> if no previous value.
   */
  public Object put(Object key, Object value)
  {
    return (super.put(key, value));
  }

  /**
   * This method removes the specified key entry (and its associated value)
   * from the property mapping list.
   * 
   * @param key The key to remove
   *
   * @return The previous value for this key, or <code>null</code> if no previous value.
   */
  public Object remove(Object key)
  {
    return (super.remove(key));
  }

  /**
   * This method clears the entire property list such that it no longer
   * contains the properties used to look up the services provided by
   * the <code>Provider</code>.
   */
  public void clear()
  {
    super.clear();
  }

  /**
   * This method returns a <code>String</code> representation of this
   * object.  This will include the <code>Provider</code> name and
   * version number.
   *
   * @return A <code>String</code> representation of this object.
   */
  public String toString()
  {
    return (getClass().getName() + ": name=" + getName() + " version=" +
	    version);
  }
}
