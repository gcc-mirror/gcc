/* ServiceRegistry.java -- A simple registry for service providers.
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

import gnu.classpath.ServiceFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * A registry for service providers.
 *
 * @since 1.4
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class ServiceRegistry
{
  // Package-private to avoid a trampoline.
  /**
   * The service categories of this registry.
   *
   * <p>Note that we expect that only very few categories will
   * typically be used with a registry. The most common case will be
   * one, it seems unlikely that any registry would contain more than
   * five or six categories. Therefore, we intentionally avoid the
   * overhead of a HashMap.
   *
   * @see #providers
   */
  final Class[] categories;


  /**
   * The registered providers for each service category, indexed by
   * the same index as the {@link #categories} array. If no provider
   * is registered for a category, the array entry will be
   * <code>null</code>.
   *
   * <p>Note that we expect that only very few providers will
   * typically be registered for a category. The most common case will
   * be one or two. Therefore, we intentionally avoid the overhead of
   * a HashMap.
   */
  private final LinkedList[] providers;


  /**
   * The ordring constaints for each service category, indexed by the
   * same index as the {@link #categories} array. The constraints for
   * a service category are stored as a <code>Map&lt;Object,
   * Set&lt;Object&gt;&gt;</code>, where the Map&#x2019;s values are
   * those providers that need to come after the key.  If no
   * constraints are imposed on the providers of a category, the array
   * entry will be <code>null</code>. If no constraints have been set
   * whatsoever, <code>constraints</code> will be <code>null</code>.
   *
   * <p>Note that we expect that only very few constraints will
   * typically be imposed on a category. The most common case will
   * be zero.
   */
  private IdentityHashMap[] constraints;


  /**
   * Constructs a <code>ServiceRegistry</code> for the specified
   * service categories.
   *
   * @param categories the categories to support
   *
   * @throws IllegalArgumentException if <code>categories</code> is
   * <code>null</code>, or if its {@link Iterator#next()} method
   * returns <code>null</code>.
   *
   * @throws ClassCastException if <code>categories</code> does not
   * iterate over instances of {@link java.lang.Class}.
   */
  public ServiceRegistry(Iterator<Class<?>> categories)
  {
    ArrayList cats = new ArrayList(/* expected size */ 10);

    if (categories == null)
      throw new IllegalArgumentException();

    while (categories.hasNext())
      {
        Class cat = (Class) categories.next();
        if (cat == null)
          throw new IllegalArgumentException();
        cats.add(cat);
      }

    int numCats = cats.size();
    this.categories = (Class[]) cats.toArray(new Class[numCats]);
    this.providers = new LinkedList[numCats];
  }


  /**
   * Finds service providers that are implementing the specified
   * Service Provider Interface.
   *
   * <p><b>On-demand loading:</b> Loading and initializing service
   * providers is delayed as much as possible. The rationale is that
   * typical clients will iterate through the set of installed service
   * providers until one is found that matches some criteria (like
   * supported formats, or quality of service). In such scenarios, it
   * might make sense to install only the frequently needed service
   * providers on the local machine. More exotic providers can be put
   * onto a server; the server will only be contacted when no suitable
   * service could be found locally.</p>
   *
   * <p><b>Security considerations:</b> Any loaded service providers
   * are loaded through the specified ClassLoader, or the system
   * ClassLoader if <code>classLoader</code> is
   * <code>null</code>. When <code>lookupProviders</code> is called,
   * the current {@link java.security.AccessControlContext} gets
   * recorded. This captured security context will determine the
   * permissions when services get loaded via the <code>next()</code>
   * method of the returned <code>Iterator</code>.</p>
   *
   * @param spi the service provider interface which must be
   * implemented by any loaded service providers.
   *
   * @param loader the class loader that will be used to load the
   * service providers, or <code>null</code> for the system class
   * loader. For using the context class loader, see {@link
   * #lookupProviders(Class)}.
   *
   * @return an iterator over instances of <code>spi</code>.
   *
   * @throws IllegalArgumentException if <code>spi</code> is
   * <code>null</code>.
   */
  public static <T> Iterator<T> lookupProviders(Class<T> spi,
                                                ClassLoader loader)
  {
    return ServiceFactory.lookupProviders(spi, loader);
  }


  /**
   * Finds service providers that are implementing the specified
   * Service Provider Interface, using the context class loader
   * for loading providers.
   *
   * @param spi the service provider interface which must be
   * implemented by any loaded service providers.
   *
   * @return an iterator over instances of <code>spi</code>.
   *
   * @throws IllegalArgumentException if <code>spi</code> is
   * <code>null</code>.
   *
   * @see #lookupProviders(Class, ClassLoader)
   */
  public static <T> Iterator<T> lookupProviders(Class<T> spi)
  {
    return ServiceFactory.lookupProviders(spi);
  }


  /**
   * Returns an iterator over all service categories.
   *
   * @return an unmodifiable {@link
   * java.util.Iterator}&lt;{@link java.lang.Class}&gt;.
   */
  public Iterator<Class<?>> getCategories()
  {
    return new Iterator()
      {
        int index = -1;

        public boolean hasNext()
        {
          return index < categories.length - 1;
        }

        public Object next()
        {
          if (!hasNext())
            throw new NoSuchElementException();

          return categories[++index];
        }

        public void remove()
        {
          throw new UnsupportedOperationException();
        }
      };
  }


  /**
   * Registers a provider for a service category which is specified by
   * the class-internal category ID.
   *
   * @param provider the service provider to be registered.
   *
   * @param cat the service category, which is identified by an index
   * into the {@link #categories} array.
   *
   * @return <code>true</code> if <code>provider</code> is the first
   * provider that gets registered for the specified service category;
   * <code>false</code> if other providers have already been
   * registered for the same servide category.
   *
   * @throws IllegalArgumentException if <code>provider</code> is
   * <code>null</code>.
   *
   * @throws ClassCastException if <code>provider</code> does not
   * implement the specified service provider interface.
   */
  private synchronized boolean registerServiceProvider(Object provider,
                                                       int cat)
  {
    LinkedList provs;
    boolean result;
    Class category;

    if (provider == null)
      throw new IllegalArgumentException();

    category = categories[cat];
    if (!category.isInstance(provider))
      throw new ClassCastException(category.getName());

    provs = providers[cat];
    if (provs == null)
    {
      result = true;
      provs = providers[cat] = new LinkedList();
    }
    else
      result = false;

    provs.add(provider);
    if (provider instanceof RegisterableService)
      ((RegisterableService) provider).onRegistration(this, category);

    return result;
  }


  /**
   * Registers a provider for the specified service category.
   *
   * <p>If <code>provider</code> implements the {@link
   * RegisterableService} interface, its {@link
   * RegisterableService#onRegistration onRegistration} method is
   * invoked in order to inform the provider about the addition to
   * this registry.
   *
   * @param provider the service provider to be registered.
   *
   * @param category the service category under which
   * <code>provider</code> shall be registered.
   *
   * @return <code>true</code> if <code>provider</code> is the first
   * provider that gets registered for the specified service category;
   * <code>false</code> if other providers have already been
   * registered for the same servide category.
   *
   * @throws IllegalArgumentException if <code>provider</code> is
   * <code>null</code>, or if <code>category</code> is not among the
   * categories passed to the {@linkplain #ServiceRegistry(Iterator)
   * constructor} of this ServiceRegistry.
   *
   * @throws ClassCastException if <code>provider</code> does not
   * implement <code>category</code>.
   */
  public synchronized <T> boolean registerServiceProvider(T provider,
                                                          Class<T> category)
  {
    for (int i = 0; i < categories.length; i++)
      if (categories[i] == category)
        return registerServiceProvider(provider, i);
    throw new IllegalArgumentException();
  }


  /**
   * Registers a provider under all service categories it
   * implements.
   *
   * <p>If <code>provider</code> implements the {@link
   * RegisterableService} interface, its {@link
   * RegisterableService#onRegistration onRegistration} method is
   * invoked in order to inform the provider about the addition to
   * this registry. If <code>provider</code> implements several
   * service categories, <code>onRegistration</code> gets called
   * multiple times.
   *
   * @param provider the service provider to be registered.
   *
   * @throws IllegalArgumentException if <code>provider</code> is
   * <code>null</code>, or if <code>provider</code> does not implement
   * any of the service categories passed to the {@linkplain
   * #ServiceRegistry(Iterator) constructor} of this ServiceRegistry.
   */
  public synchronized void registerServiceProvider(Object provider)
  {
    boolean ok = false;

    if (provider == null)
      throw new IllegalArgumentException();

    for (int i = 0; i < categories.length; i++)
      if (categories[i].isInstance(provider))
        {
          ok = true;
          registerServiceProvider(provider, i);
        }

    if (!ok)
      throw new IllegalArgumentException();
  }


  /**
   * Registers a number of providers under all service categories they
   * implement.
   *
   * <p>If a provider implements the {@link RegisterableService}
   * interface, its {@link RegisterableService#onRegistration
   * onRegistration} method is invoked in order to inform the provider
   * about the addition to this registry. If <code>provider</code>
   * implements several service categories,
   * <code>onRegistration</code> gets called multiple times.
   *
   * @throws IllegalArgumentException if <code>providers</code> is
   * <code>null</code>, if any iterated provider is <code>null</code>,
   * or if some iterated provider does not implement any of the
   * service categories passed to the {@linkplain
   * #ServiceRegistry(Iterator) constructor} of this
   * <code>ServiceRegistry</code>.
   */
  public synchronized void registerServiceProviders(Iterator<?> providers)
  {
    if (providers == null)
      throw new IllegalArgumentException();

    while (providers.hasNext())
      registerServiceProvider(providers.next());
  }


  /**
   * De-registers a provider for a service category which is specified
   * by the class-internal category ID.
   *
   * @param provider the service provider to be registered.
   *
   * @param cat the service category, which is identified by an index
   * into the {@link #categories} array.
   *
   * @return <code>true</code> if <code>provider</code> was previously
   * registered for the specified service category; <code>false</code>
   * if if the provider had not been registered.
   *
   * @throws IllegalArgumentException if <code>provider</code> is
   * <code>null</code>.
   *
   * @throws ClassCastException if <code>provider</code> does not
   * implement the specified service provider interface.
   */
  private synchronized boolean deregisterServiceProvider(Object provider,
                                                         int cat)
  {
    LinkedList provs;
    boolean result;
    Class category;

    if (provider == null)
      throw new IllegalArgumentException();

    category = categories[cat];
    if (!category.isInstance(provider))
      throw new ClassCastException(category.getName());

    provs = providers[cat];
    if (provs == null)
      return false;

    result = provs.remove(provider);
    if (provs.isEmpty())
      providers[cat] = null;

    if (result && (provider instanceof RegisterableService))
      ((RegisterableService) provider).onDeregistration(this, category);

    return result;
  }


  /**
   * De-registers a provider for the specified service category.
   *
   * <p>If <code>provider</code> implements the {@link
   * RegisterableService} interface, its {@link
   * RegisterableService#onDeregistration onDeregistration} method is
   * invoked in order to inform the provider about the removal from
   * this registry.
   *
   * @param provider the service provider to be de-registered.
   *
   * @param category the service category from which
   * <code>provider</code> shall be de-registered.
   *
   * @return <code>true</code> if <code>provider</code> was previously
   * registered for the specified service category; <code>false</code>
   * if if the provider had not been registered.
   *
   * @throws IllegalArgumentException if <code>provider</code> is
   * <code>null</code>, or if <code>category</code> is not among the
   * categories passed to the {@linkplain #ServiceRegistry(Iterator)
   * constructor} of this ServiceRegistry.
   *
   * @throws ClassCastException if <code>provider</code> does not
   * implement <code>category</code>.
   */
  public synchronized <T> boolean deregisterServiceProvider(T provider,
                                                            Class<T> category)
  {
    for (int i = 0; i < categories.length; i++)
      if (categories[i] == category)
        return deregisterServiceProvider(provider, i);
    throw new IllegalArgumentException();
  }


  /**
   * De-registers a provider from all service categories it
   * implements.
   *
   * <p>If <code>provider</code> implements the {@link
   * RegisterableService} interface, its {@link
   * RegisterableService#onDeregistration onDeregistration} method is
   * invoked in order to inform the provider about the removal from
   * this registry. If <code>provider</code> implements several
   * service categories, <code>onDeregistration</code> gets called
   * multiple times.</p>
   *
   * @param provider the service provider to be de-registered.
   *
   * @throws IllegalArgumentException if <code>provider</code> is
   * <code>null</code>, or if <code>provider</code> does not implement
   * any of the service categories passed to the {@linkplain
   * #ServiceRegistry(Iterator) constructor} of this
   * <code>ServiceRegistry</code>.
   */
  public synchronized void deregisterServiceProvider(Object provider)
  {
    boolean ok = false;

    if (provider == null)
      throw new IllegalArgumentException();

    for (int i = 0; i < categories.length; i++)
      if (categories[i].isInstance(provider))
        {
          ok = true;
          deregisterServiceProvider(provider, i);
        }

    if (!ok)
      throw new IllegalArgumentException();
  }


  /**
   * De-registers all providers which have been registered for the
   * specified service category.
   *
   * <p>If a provider implements the {@link RegisterableService}
   * interface, its {@link RegisterableService#onDeregistration
   * onDeregistration} method is invoked in order to inform the
   * provider about the removal from this registry. If the provider
   * implements several service categories,
   * <code>onDeregistration</code> gets called multiple times.
   *
   * @param category the category whose registered providers will be
   * de-registered.
   *
   * @throws IllegalArgumentException if <code>category</code> is not
   * among the categories passed to the {@linkplain
   * #ServiceRegistry(Iterator) constructor} of this
   * <code>ServiceRegistry</code>.
   */
  public synchronized void deregisterAll(Class<?> category)
  {
    boolean ok = false;

    for (int i = 0; i < categories.length; i++)
      {
        if (categories[i] != category)
          continue;

        ok = true;
        while (providers[i] != null)
          deregisterServiceProvider(providers[i].get(0), i);
      }

    if (!ok)
      throw new IllegalArgumentException();
  }


  /**
   * De-registers all service providers.
   *
   * <p>If a provider implements the {@link RegisterableService}
   * interface, its {@link RegisterableService#onDeregistration
   * onDeregistration} method is invoked in order to inform the
   * provider about the removal from this registry. If the provider
   * implements several service categories,
   * <code>onDeregistration</code> gets called multiple times.
   */
  public synchronized void deregisterAll()
  {
    for (int i = 0; i < categories.length; i++)
      while (providers[i] != null)
        deregisterServiceProvider(providers[i].get(0), i);
  }


  /**
   * Called by the Virtual Machine when it detects that this
   * <code>ServiceRegistry</code> has become garbage. De-registers all
   * service providers, which will cause those that implement {@link
   * RegisterableService} to receive a {@link
   * RegisterableService#onDeregistration onDeregistration}
   * notification.
   */
  public void finalize()
    throws Throwable
  {
    super.finalize();
    deregisterAll();
  }


  /**
   * Determines whether a provider has been registered with this
   * registry.
   *
   * @return <code>true</code> if <code>provider</code> has been
   * registered under any service category; <code>false</code> if
   * it is not registered.
   *
   * @throws IllegalArgumentException if <code>provider</code> is
   * <code>null</code>.
   */
  public synchronized boolean contains(Object provider)
  {
    if (provider == null)
      throw new IllegalArgumentException();

    // Note that contains is rather unlikely to be ever called,
    // so it would be wasteful to keep a special data structure
    // (such as a HashSet) for making it a fast operation.
    for (int i = 0; i < providers.length; i++)
      {
        // If provider does not implement categories[i],
        // it would not have been possible to register it there.
        // In that case, it would be pointless to look there.
        if (!categories[i].isInstance(provider))
          continue;

        // But if the list of registered providers contains provider,
        // we have found it.
        LinkedList p = providers[i];
        if (p != null && p.contains(provider))
          return true;
      }

    return false;
  }


  /**
   * Returns the index in {@link #categories} occupied by the
   * specified service category.
   *
   * @throws IllegalArgumentException if <code>category</code> is not
   * among the categories passed to the {@linkplain
   * #ServiceRegistry(Iterator) constructor} of this ServiceRegistry.
   */
  private int getCategoryID(Class category)
  {
    for (int i = 0; i < categories.length; i++)
      if (categories[i] == category)
        return i;

    throw new IllegalArgumentException();
  }


  /**
   * Retrieves all providers that have been registered for the
   * specified service category.
   *
   * @param category the service category whose providers are
   * to be retrieved.
   *
   * @param useOrdering <code>true</code> in order to retrieve the
   * providers in an order imposed by the {@linkplain #setOrdering
   * ordering constraints}; <code>false</code> in order to retrieve
   * the providers in any order.
   *
   * @throws IllegalArgumentException if <code>category</code> is not
   * among the categories passed to the {@linkplain
   * #ServiceRegistry(Iterator) constructor} of this
   * <code>ServiceRegistry</code>.
   *
   * @see #getServiceProviders(Class, Filter, boolean)
   */
  public <T> Iterator<T> getServiceProviders(Class<T> category,
                                             boolean useOrdering)
  {
    return getServiceProviders(category, null, useOrdering);
  }


  /**
   * Retrieves all providers that have been registered for the
   * specified service category and that satisfy the criteria
   * of a custom filter.
   *
   * @param category the service category whose providers are
   * to be retrieved.
   *
   * @param filter a custom filter, or <code>null</code> to
   * retrieve all registered providers for the specified
   * category.
   *
   * @param useOrdering <code>true</code> in order to retrieve the
   * providers in an order imposed by the {@linkplain #setOrdering
   * ordering constraints}; <code>false</code> in order to retrieve
   * the providers in any order.
   *
   * @throws IllegalArgumentException if <code>category</code> is not
   * among the categories passed to the {@linkplain
   * #ServiceRegistry(Iterator) constructor} of this
   * <code>ServiceRegistry</code>.
   */
  public synchronized <T> Iterator<T> getServiceProviders(Class<T> category,
                                                          Filter filter,
                                                          boolean useOrdering)
  {
    int catid;
    LinkedList provs;
    ArrayList result;

    catid = getCategoryID(category);
    provs = providers[catid];
    if (provs == null)
      return Collections.EMPTY_LIST.iterator();

    result = new ArrayList(provs.size());
    for (Iterator iter = provs.iterator(); iter.hasNext();)
      {
        Object provider = iter.next();
        if (filter == null || filter.filter(provider))
          result.add(provider);
      }

    // If we are supposed to obey ordering constraints, and
    // if any constraints have been imposed on the specified
    // service category, sort the result.
    if (useOrdering && constraints != null)
      {
        final Map cons = constraints[catid];
        if (cons != null)
          Collections.sort(result, new Comparator()
            {
              public int compare(Object o1, Object o2)
              {
                Set s;

                if (o1 == o2)
                  return 0;

                s = (Set) cons.get(o1);
                if (s != null && s.contains(o2))
                  return -1;  // o1 < o2

                s = (Set) cons.get(o2);
                if (s != null && s.contains(o1))
                  return 1;  // o1 > o2

                return 0; // o1 == o2
              }
            });
      }

    return result.iterator();
  }


  /**
   * Returns one of the service providers that is a subclass of the
   * specified class.
   *
   * @param providerClass a class to search for.
   */
  public synchronized <T> T getServiceProviderByClass(Class<T> providerClass)
  {
    if (providerClass == null)
      throw new IllegalArgumentException();

    // Note that the method getServiceProviderByClass is rather
    // unlikely to be ever called, so it would be wasteful to keep a
    // special data structure for making it a fast operation.
    for (int cat = 0; cat < categories.length; cat++)
      {
        if (!categories[cat].isAssignableFrom(providerClass))
          continue;

        LinkedList provs = providers[cat];
        if (provs == null)
          continue;

        for (Iterator iter = provs.iterator(); iter.hasNext();)
          {
            Object provider = iter.next();
            if (providerClass.isInstance(provider))
              return (T) provider;
          }
      }

    return null;
  }


  /**
   * Adds an ordering constraint on service providers.
   *
   * @param category the service category to which an ordering
   * constraint is to be added.
   *
   * @param firstProvider the provider which is supposed to come before
   * <code>second</code>.
   *
   * @param secondProvider the provider which is supposed to come after
   * <code>first</code>.
   *
   * @throws IllegalArgumentException if <code>first</code> and
   * <code>second</code> are referring to the same object, or if one
   * of them is <code>null</code>.
   *
   * @see #unsetOrdering
   * @see #getServiceProviders(Class, Filter, boolean)
   */
  public synchronized <T> boolean setOrdering(Class<T> category,
                                              T firstProvider,
                                              T secondProvider)
  {
    return addConstraint(getCategoryID(category), firstProvider,
                         secondProvider);
  }


  /**
   * Removes an ordering constraint on service providers.
   *
   * @param category the service category from which an ordering
   * constraint is to be removed.
   *
   * @param firstProvider the provider which is supposed to come before
   * <code>second</code>.
   *
   * @param secondProvider the provider which is supposed to come after
   * <code>first</code>.
   *
   * @throws IllegalArgumentException if <code>first</code> and
   * <code>second</code> are referring to the same object, or if one
   * of them is <code>null</code>.
   *
   * @see #setOrdering
   */
  public synchronized <T> boolean unsetOrdering(Class<T> category,
                                                T firstProvider,
                                                T secondProvider)
  {
    return removeConstraint(getCategoryID(category),
                            firstProvider, secondProvider);
  }


  /**
   * Adds an ordering constraint on service providers.
   *
   * @param catid the service category ID, which is the
   * category&#x2019;s index into the {@link #categories} array.
   *
   * @param first the provider which is supposed to come before
   * <code>second</code>.
   *
   * @param second the provider which is supposed to come after
   * <code>first</code>.
   *
   * @throws IllegalArgumentException if <code>first</code> and
   * <code>second</code> are referring to the same object, or if one
   * of them is <code>null</code>.
   */
  private boolean addConstraint(int catid, Object first, Object second)
  {
    Set s;
    IdentityHashMap cons;

    // Also checks argument validity.
    removeConstraint(catid, second, first);

    if (constraints == null)
      constraints = new IdentityHashMap[categories.length];
    cons = constraints[catid];
    if (cons == null)
      cons = constraints[catid] = new IdentityHashMap();

    s = (Set) cons.get(first);
    if (s == null)
      cons.put(first, s = new HashSet());
    return s.add(second);
  }


  /**
   * Removes an ordering constraint on service providers.
   *
   * @param catid the service category ID, which is the
   * category&#x2019;s index into the {@link #categories} array.
   *
   * @param first the provider which is supposed to come before
   * <code>second</code>.
   *
   * @param second the provider which is supposed to come after
   * <code>first</code>.
   *
   * @throws IllegalArgumentException if <code>first</code> and
   * <code>second</code> are referring to the same object, or if one
   * of them is <code>null</code>.
   */
  private boolean removeConstraint(int catid, Object first, Object second)
  {
    Collection s;
    IdentityHashMap cons;

    if (first == null || second == null || first == second)
      throw new IllegalArgumentException();

    if (constraints == null)
      return false;

    cons = constraints[catid];
    if (cons == null)
      return false;

    s = (Collection) cons.get(first);
    if (s == null)
      return false;

    if (!s.remove(second))
      return false;

    // If we removed the last constraint for a service category,
    // we can get free some memory.
    if (cons.isEmpty())
      {
        constraints[catid] = null;
        boolean anyConstraints = false;
        for (int i = 0; i < constraints.length; i++)
          {
            if (constraints[i] != null)
              {
                anyConstraints = true;
                break;
              }
          }
        if (!anyConstraints)
          constraints = null;
      }

    return true;
  }


  /**
   * A filter for selecting service providers that match custom
   * criteria.
   *
   * @see ServiceRegistry#getServiceProviders(Class, Filter,
   * boolean)
   *
   * @since 1.4
   *
   * @author Michael Koch (konqueror@gmx.de)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static interface Filter
  {
    /**
     * Checks whether the specified service provider matches the
     * constraints of this Filter.
     *
     * @param provider the service provider in question.
     *
     * @return <code>true</code> if <code>provider</code> matches the
     * criteria; <code>false</code> if it does not match.
     */
    boolean filter(Object provider);
  }
}
