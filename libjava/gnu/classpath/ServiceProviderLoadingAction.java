/* ServiceProviderLoadingAction.java -- Action for loading plug-in services.
   Copyright (C) 2004  Free Software Foundation

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


package gnu.classpath;

import java.security.PrivilegedExceptionAction;

/**
 * A privileged action for creating a new instance of a service
 * provider.
 *
 * <p>Class loading and instantiation is encapsulated in a
 * <code>PriviledgedAction</code> in order to restrict the loaded
 * service providers to the {@link java.security.AccessControlContext}
 * that was active when {@link
 * gnu.classpath.ServiceFactory#lookupProviders} was called, even
 * though the actual loading is delayed to the time when the provider
 * is actually needed.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
final class ServiceProviderLoadingAction
  implements PrivilegedExceptionAction
{
  /**
   * The interface to which the loaded service provider implementation
   * must conform.  Usually, this is a Java interface type, but it
   * might also be an abstract class or even a concrete class.
   */
  private final Class spi;


  /**
   * The fully qualified name of the class that gets loaded when
   * this action is executed.
   */
  private final String providerName;


  /**
   * The ClassLoader that gets used for loading the service provider
   * class.
   */
  private final ClassLoader loader;


  /**
   * Constructs a privileged action for loading a service provider.
   *
   * @param spi the interface to which the loaded service provider
   * implementation must conform. Usually, this is a Java interface
   * type, but it might also be an abstract class or even a concrete
   * superclass.
   *
   * @param providerName the fully qualified name of the class that
   * gets loaded when this action is executed.
   *
   * @param loader the ClassLoader that gets used for loading the
   * service provider class.
   *
   * @throws IllegalArgumentException if <code>spi</code>,
   * <code>providerName</code> or <code>loader</code> is
   * <code>null</code>.
   */
  ServiceProviderLoadingAction(Class spi, String providerName,
                               ClassLoader loader)
  {
    if (spi == null || providerName == null || loader == null)
      throw new IllegalArgumentException();

    this.spi = spi;
    this.providerName = providerName;
    this.loader = loader;
  }

  
  /**
   * Loads an implementation class for a service provider, and creates
   * a new instance of the loaded class by invoking its public
   * no-argument constructor.
   *
   * @return a new instance of the class whose name was passed as
   * <code>providerName</code> to the constructor.
   *
   * @throws ClassCastException if the service provider does not
   * implement the <code>spi</code> interface that was passed to the
   * constructor.
   *
   * @throws IllegalAccessException if the service provider class or
   * its no-argument constructor are not <code>public</code>.
   *
   * @throws InstantiationException if the service provider class is
   * <code>abstract</code>, an interface, a primitive type, an array
   * class, or void; or if service provider class does not have a
   * no-argument constructor; or if there some other problem with
   * creating a new instance of the service provider.
   */
  public Object run()
    throws Exception
  {
    Class loadedClass;
    Object serviceProvider;

    loadedClass = loader.loadClass(providerName);
    serviceProvider = loadedClass.newInstance();

    // Ensure that the loaded provider is actually implementing
    // the service provider interface.
    if (!spi.isInstance(serviceProvider))
      throw new ClassCastException(spi.getName());

    return serviceProvider;
  }
}
