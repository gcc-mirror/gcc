/* ServiceLoader.java -- Allows loading of plug-in services.
   Copyright (C) 2006, 2007  Free Software Foundation

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

package java.util;

import gnu.classpath.ServiceFactory;

/**
 * <p>
 * Facilities for loading service providers.  A service is
 * defined by a set of interfaces or abstract classes, and
 * a service provider gives a concrete implementation of this.
 * Service providers may be installed as part of the runtime
 * environment using JAR files in the extension directories,
 * or may be simply supplied on the classpath.
 * </p>
 * <p>
 * In terms of loading a service, the service is defined by
 * a single interface or abstract class which the provider
 * implements.  This may not constitute the entire service,
 * but is simply a mechanism by which a provider of the
 * service can be loaded and its capabilities determined.
 * The variety of possible services means that no more
 * requirements are made of the service provider other than
 * that it must have an accessible zero argument constructor
 * in order to allow an instance to be created.
 * </p>
 * <p>
 * Service providers are listed in a file named after the
 * service type in the directory <code>META-INF/services</code>.
 * The file contains a list of classes, and must be encoded
 * using UTF-8.  Whitespace is ignored.  Comments can be
 * included by using a <code>'#'</code> prefix; anything occurring
 * on the same line after this symbol is ignored. Duplicate classes
 * are ignored.
 * </p>
 * <p>
 * The classes are loaded using the same classloader that was
 * queried in order to locate the configuration file.  As a result,
 * the providers do not need to reside in the same JAR file as the
 * resource; they merely have to be accessible to this classloader,
 * which may differ from the one that loaded the file itself.
 * </p>
 * <p>
 * Providers are located and instantiated lazily, as calls to the
 * {@link #iterator()} are made.  Providers are cached, and those in
 * the cache are returned first.  The cache may be cleared by calling
 * {@link #reload()}.  Service loaders always execute in the security
 * context of the caller, so ideally calls should be made from a trusted
 * source.
 * </p>
 * <p>
 * Note that this class is not thread-safe, and that strange errors may
 * occur as the result of the use of remote URLs occurring on the classpath,
 * which lead to erroneous web pages.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public final class ServiceLoader<S>
  implements Iterable<S>
{

  /**
   * The class of the service provider.
   */
  private Class<S> spi;

  /**
   * The class loader for the service provider.
   */
  private ClassLoader loader;

  /**
   * The cache of service providers.
   */
  private List<S> cache;

  /**
   * The {@link gnu.classpath.ServiceFactory} iterator
   * from which providers are obtained.
   */
  private Iterator<S> serviceIt;

  /**
   * Constructs a new {@link ServiceLoader} with
   * the specified provider and class loader.
   *
   * @param spi the service to load.
   * @param loader the class loader to use.
   */
  private ServiceLoader(Class<S> spi, ClassLoader loader)
  {
    this.spi = spi;
    this.loader = loader;
    cache = new ArrayList<S>();
  }

  /**
   * Lazily loads the available providers.  The iterator first returns
   * providers from the cache, in instantiation order, followed by any
   * remaining providers, which are added to the cache after loading.
   * The actual loading and parsing of the configuration file takes
   * place in the {@link Iterator#hasNext()} and {@link Iterator#next()}
   * methods, which means that they may result in a
   * {@link ServiceConfigurationError} being thrown.  If such an error
   * does occur, subsequent invocations will attempt to recover.
   * The {@link remove()} method is not supported and instead throws
   * an {@link UnsupportedOperationException}.
   *
   * @return an iterator that lazily loads service providers.
   */
  public Iterator<S> iterator()
  {
    return new Iterator<S>()
      {
        /**
         * The cache iterator.
         */
        private Iterator<S> cacheIt = cache.iterator();

        public boolean hasNext()
        {
          if (cacheIt.hasNext())
            return true;
          if (serviceIt == null)
            serviceIt =
              ServiceFactory.lookupProviders(spi, loader, true);
          return serviceIt.hasNext();
        }

        public S next()
        {
          if (cacheIt.hasNext())
            return cacheIt.next();
          if (serviceIt == null)
            serviceIt =
              ServiceFactory.lookupProviders(spi, loader, true);
          S nextService = serviceIt.next();
          cache.add(nextService);
          return nextService;
        }

        public void remove()
        {
          throw new UnsupportedOperationException();
        }
      };
  }

  /**
   * Creates a new service loader for the given service,
   * using the context class loader of the current thread.
   * This is equivalent to calling <code>ServiceLoader.load(service,
   * Thread.currentThread().getContextClassLoader())</code>.
   *
   * @param service the interface or abstract class that represents
   *                the service.
   * @return a new {@link ServiceLoader} instance.
   */
  public static <S> ServiceLoader<S> load(Class<S> service)
  {
    return load(service,
                Thread.currentThread().getContextClassLoader());
  }

  /**
   * Creates a new service loader for the given service,
   * using the specified class loader.  The class loader is
   * used to access the configuration file and the service
   * provider instances themselves.  If the loader is
   * <code>null</code>, the system class loader (or, if
   * this is also <code>null</code>, the bootstrap class
   * loader).
   *
   * @param service the interface or abstract class that represents
   *                the service.
   * @param loader the class loader used to load the configuration
   *               file and service providers.
   * @return a new {@link ServiceLoader} instance.
   */
  public static <S> ServiceLoader<S> load(Class<S> service,
                                          ClassLoader loader)
  {
    if (loader == null)
      loader = ClassLoader.getSystemClassLoader();
    return new ServiceLoader(service, loader);
  }

  /**
   * Creates a new service loader for the given service,
   * using the extension class loader.  If the extension
   * class loader can not be found, the system class loader
   * is used (or, if this is <code>null</code>, the
   * bootstrap class loader).  The primary use of this method
   * is to only obtain installed services, ignoring any which
   * may appear on the classpath.  This is equivalent to calling
   * <code>load(service, extClassLoader)</code> where
   * <code>extClassLoader</code> is the extension class loader
   * (or <code>null</code> if this is unavailable).
   *
   * @param service the interface or abstract class that represents
   *                the service.
   * @return a new {@link ServiceLoader} instance.
   */
  public static <S> ServiceLoader<S> loadInstalled(Class<S> service)
  {
    /* We expect the extension class loader to be the parent
     * of the system class loader, as in
     * ClassLoader.getDefaultSystemClassLoader() */
    return load(service,
                ClassLoader.getSystemClassLoader().getParent());
  }

  /**
   * Clears the cache of the provider, so that all providers
   * are again read from the configuration file and instantiated.
   */
  public void reload()
  {
    cache.clear();
  }

  /**
   * Returns a textual representation of this
   * {@link ServiceLoader}.
   *
   * @return a textual representation of the
   *         service loader.
   */
  public String toString()
  {
    return getClass().getName() +
      "[spi=" + spi +
      ",loader=" + loader +
      "]";
  }

}
