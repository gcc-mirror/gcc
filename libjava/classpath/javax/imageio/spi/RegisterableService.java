/* RegisterableService.java -- An interface for service providers.
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


/**
 * An interface which service providers may optionally implement in
 * order to get notified when they are added or removed from a {@link
 * ServiceRegistry}.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public interface RegisterableService
{
  /**
   * Informs this service provider that it has been registered in a
   * {@link ServiceRegistry}. If this provider gets registered as an
   * implementor for several service categories, its
   * <code>onRegistration</code> method will be called multiple times.
   *
   * @param registry the registry to which this service provider has
   * been added.
   *
   * @param category the service category for which this provider has
   * been registered as an implementor.
   */
  void onRegistration(ServiceRegistry registry, Class<?> category);


  /**
   * Informs this service provider that it has been de-registered from
   * a {@link ServiceRegistry}. If this provider had been registered
   * as an implementor for several service categories, its
   * <code>onDeregistration</code> method will be called multiple
   * times.
   *
   * @param registry the registry from which this service provider has
   * been removed.
   *
   * @param category the service category for which this provider has
   * been registered as an implementor.
   */
  void onDeregistration(ServiceRegistry registry, Class<?> category);
}

