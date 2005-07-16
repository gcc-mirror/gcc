/* IIOServiceProvider.java -- General service provider for image I/O.
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.imageio.spi;

import java.util.Locale;


/**
 * An abstract superclass for service providers that perform image I/O.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class IIOServiceProvider
  implements RegisterableService
{
  /**
   * The vendor of this service provider, or <code>null</code> if the
   * subclass constructor did not set this field.
   *
   * @see #getVendorName()
   */
  protected String vendorName;


  /**
   * The version of this service provider, or <code>null</code> if the
   * subclass constructor did not set this field.
   *
   * @see #getVersion()
   */
  protected String version;


  /**
   * Constructs a general <code>IIOServiceProvider</code>, given the
   * vendor name and a version string.
   *
   * @throws IllegalArgumentException if <code>vendorName</code>
   * or <code>version</code> is <code>null</code>.
   */
  public IIOServiceProvider(String vendorName, String version)
  {
    if (vendorName == null || version == null)
      throw new IllegalArgumentException();

    this.vendorName = vendorName;
    this.version = version;
  }


  /**
   * Constructs a general <code>IIOServiceProvider</code> without
   * specifying a vendor name and a version string. The subclass
   * constructor should set the {@link #vendorName} and {@link
   * #version} to non-null values.
   */
  public IIOServiceProvider()
  {
  }


  /**
   * Informs this service provider that it has been registered in a
   * {@link ServiceRegistry}. If this provider gets registered as an
   * implementor for several service categories, its
   * <code>onRegistration</code> method will be called multiple times.
   * The default implementation does nothing.
   *
   * @param registry the registry to which this service provider has
   * been added.
   *
   * @param category the service category for which this provider has
   * been registered as an implementor.
   */
  public void onRegistration(ServiceRegistry registry, Class category)
  {
  }


  /**
   * Informs this service provider that it has been de-registered from
   * a {@link ServiceRegistry}. If this provider had been registered
   * as an implementor for several service categories, its
   * <code>onDeregistration</code> method will be called multiple
   * times. The default implementation does nothing.
   *
   * @param registry the registry from which this service provider has
   * been removed.
   *
   * @param category the service category for which this provider has
   * been registered as an implementor.
   */
  public void onDeregistration(ServiceRegistry registry, Class category)
  {
  }


  /**
   * Returns the name of the vendor of this service provider.
   */
  public String getVendorName()
  {
    return vendorName;
  }


  /**
   * Returns an identifier string for the version of this service
   * provider.
   */
  public String getVersion()
  {
    return version;
  }


  /**
   * Returns a short description of this service provider that can be
   * presented to a human user.
   *
   * @param locale the locale for which the description string should
   * be localized.
   */
  public abstract String getDescription(Locale locale);
}
