/* Provider.java -- Security provider information
   Copyright (C) 1998, 1999, 2000, 2002, 2006 Free Software Foundation, Inc.

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

package java.security;

import java.io.Serializable;
import java.util.Properties;

/**
 * This class represents a Java security architecture service provider. The
 * services provided by a such a provider can range from security algorithms to
 * key generation.
 * <p>
 * Providers are installed by name and version number. See the static
 * initializer of the {@link java.security.Security} class for the default
 * security providers installed by this class library.
 * 
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public abstract class Provider
    extends Properties
    implements Serializable
{
  private static final long serialVersionUID = -4298000515446427739L;

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
   * Maps a key property to a designated value.
   * <p>
   * If there is an installed {@link SecurityManager} object in the underlying
   * VM, its {@link SecurityManager#checkSecurityAccess(String)} method is
   * called with the string <code>"putProviderProperty." + name</code>, where
   * <code>name</code> is this provider's name. For the default implementation
   * this translates into a {@link SecurityManager#checkPermission(Permission)}
   * for a <code>SecurityPermission("putProviderProperty." + name)</code>.
   * 
   * @param key The property key.
   * @param value The property value.
   * @return The previous value of the specified property (<code>key</code>),
   *         or <code>null</code> if it did not have one.
   * @throws SecurityException If a security manager is installed and its
   *           {@link SecurityManager#checkSecurityAccess(String)} method
   *           disallows adding properties at run-time.
   * @since Classpath 0.4+cvs, JDK 1.2
   * @see java.lang.Object#equals(Object)
   * @see java.util.Hashtable#get(Object)
   */
  public Object put(Object key, Object value)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("putProviderProperty." + this.name);
    return super.put(toCanonicalKey(key), value);
  }

  // overrides same in java.util.Hashtable
  public Object get(Object key)
  {
    return super.get(toCanonicalKey(key));
  }

  /**
   * This method removes the specified key entry (and its associated value)
   * from the property mapping collection.
   * <p>
   * If there is an installed {@link SecurityManager} object in the underlying
   * VM, its {@link SecurityManager#checkSecurityAccess(String)} method is
   * called with the string <code>"removeProviderProperty." + name</code>, where
   * <code>name</code> is this provider's name. For the default implementation
   * this translates into a {@link SecurityManager#checkPermission(Permission)}
   * for a <code>SecurityPermission("removeProviderProperty." + name)</code>.
   * 
   * @param key The key to remove
   * @return The previous value for this key, or <code>null</code> if no
   * previous value.
   */
  public Object remove(Object key)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("removeProviderProperty." + this.name);
    return super.remove(toCanonicalKey(key));
  }

  /**
   * This method clears the entire property collection such that it no longer
   * contains the properties used to look up the services provided by
   * this <code>Provider</code>.
   * <p>
   * If there is an installed {@link SecurityManager} object in the underlying
   * VM, its {@link SecurityManager#checkSecurityAccess(String)} method is
   * called with the string <code>"clearProviderProperties." + name</code>,
   * where <code>name</code> is this provider's name. For the default
   * implementation this translates into a
   * {@link SecurityManager#checkPermission(Permission)} for a
   * <code>SecurityPermission("clearProviderProperties." + name)</code>.
   */
  public void clear()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("clearProviderProperties." + this.name);
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

  private Object toCanonicalKey(Object key)
  {
    if (key.getClass().isAssignableFrom(String.class)) // is it ours?
      return ((String) key).toUpperCase(); // use default locale
    return key;
  }
}
