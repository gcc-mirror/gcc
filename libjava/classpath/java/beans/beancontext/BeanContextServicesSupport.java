/* BeanContextServicesSupport.java --
   Copyright (C) 2003, 2005  Free Software Foundation, Inc.

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


package java.beans.beancontext;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TooManyListenersException;

/**
 * This is a helper class for implementing a bean context which
 * supplies services.  It is intended to be used either by
 * subclassing or by calling methods of this implementation
 * from another.
 *
 * @author Michael Koch
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.2
 */
public class BeanContextServicesSupport
  extends BeanContextSupport
  implements BeanContextServices
{
  private static final long serialVersionUID = -8494482757288719206L;

  protected class BCSSChild
    extends BeanContextSupport.BCSChild
  {
    private static final long serialVersionUID = -3263851306889194873L;

    BCSSChild(Object targetChild, Object peer)
    {
      super(targetChild, peer);
    }
  }

  protected class BCSSProxyServiceProvider
    implements BeanContextServiceProvider,
    BeanContextServiceRevokedListener
  {
    private static final long serialVersionUID = 7078212910685744490L;

    private BeanContextServiceProvider provider;

    BCSSProxyServiceProvider(BeanContextServiceProvider p)
    {
      provider = p;
    }

    public Iterator getCurrentServiceSelectors (BeanContextServices bcs,
                                                Class serviceClass)
    {
      return provider.getCurrentServiceSelectors(bcs, serviceClass);
    }

    public Object getService (BeanContextServices bcs,
                              Object requestor,
                              Class serviceClass,
                              Object serviceSelector)
    {
      return provider.getService(bcs, requestor, serviceClass,
                                 serviceSelector);
    }

    public void releaseService (BeanContextServices bcs,
                                Object requestor,
                                Object service)
    {
      provider.releaseService(bcs, requestor, service);
    }

    public void serviceRevoked (BeanContextServiceRevokedEvent bcsre)
    {
      if (provider instanceof BeanContextServiceRevokedListener)
        ((BeanContextServiceRevokedListener) provider).serviceRevoked(bcsre);
    }
  }

  protected static class BCSSServiceProvider
    implements Serializable
  {
    private static final long serialVersionUID = 861278251667444782L;

    protected BeanContextServiceProvider serviceProvider;

    private Class serviceClass;

    private BCSSServiceProvider(Class serviceClass,
                                BeanContextServiceProvider provider)
    {
      this.serviceClass = serviceClass;
      serviceProvider = provider;
    }

    protected BeanContextServiceProvider getServiceProvider()
    {
      return serviceProvider;
    }

    private Class getServiceClass()
    {
      return serviceClass;
    }

  }

  /**
   * Represents a request for a service.  This is
   * a common superclass used by the classes which maintain
   * the listener-requestor and service-requestor relationships.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static abstract class Request
  {
    private Object requestor;

    public Request(Object requestor)
    {
      this.requestor = requestor;
    }

    public boolean equals(Object obj)
    {
      if (obj instanceof Request)
        {
          Request req = (Request) obj;
          return req.getRequestor().equals(requestor);
        }
      return false;
    }

    public Object getRequestor()
    {
      return requestor;
    }

  }

  /**
   * Represents a relationship between a service requestor
   * and a revocation listener.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static class ServiceRequest
    extends Request
  {

    private BeanContextServiceRevokedListener listener;

    public ServiceRequest(Object requestor,
                          BeanContextServiceRevokedListener listener)
    {
      super(requestor);
      this.listener = listener;
    }

    public boolean equals(Object obj)
    {
      if (obj instanceof ServiceRequest)
        {
          ServiceRequest sr = (ServiceRequest) obj;
          return (super.equals(obj) &&
                  sr.getListener().equals(listener));
        }
      return false;
    }

    public BeanContextServiceRevokedListener getListener()
    {
      return listener;
    }
  }

  /**
   * Represents a relationship between a service requestor
   * and a service instance.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static class ServiceLease
    extends Request
  {

    private Object service;

    public ServiceLease(Object requestor, Object service)
    {
      super(requestor);
      this.service = service;
    }

    public boolean equals(Object obj)
    {
      if (obj instanceof ServiceLease)
        {
          ServiceLease sl = (ServiceLease) obj;
          return (super.equals(obj) &&
                  sl.getService().equals(service));
        }
      return false;
    }

    public Object getService()
    {
      return service;
    }
  }

  /**
   * A collection of listeners who receive availability
   * and revocation notifications.
   */
  protected transient ArrayList bcsListeners;

  protected transient BCSSProxyServiceProvider proxy;

  /**
   * The number of serializable service providers.
   */
  protected transient int serializable;

  /**
   * A map of registered services, linking the service
   * class to its associated {@link BCSSServiceProvider}.
   */
  protected transient HashMap services;

  /**
   * A map of children to a list of services they
   * have obtained.
   */
  private transient HashMap serviceUsers;

  /**
   * A map of services to {@link ServiceRequest}s.
   */
  private transient HashMap serviceRequests;

  /**
   * A map of {@link ServiceLease}s to providers.
   */
  private transient HashMap serviceLeases;

  /**
   * Construct a {@link BeanContextServicesSupport} instance.
   */
  public BeanContextServicesSupport ()
  {
    super();
  }

  /**
   * Construct a {@link BeanContextServicesSupport} instance.
   *
   * @param peer the bean context services peer (<code>null</code> permitted).
   */
  public BeanContextServicesSupport (BeanContextServices peer)
  {
    super(peer);
  }

  /**
   * Construct a {@link BeanContextServicesSupport} instance.
   *
   * @param peer the bean context peer (<code>null</code> permitted).
   * @param locale the locale (<code>null</code> permitted, equivalent to
   *     the default locale).
   */
  public BeanContextServicesSupport(BeanContextServices peer, Locale locale)
  {
    super(peer, locale);
  }

  /**
   * Construct a {@link BeanContextServicesSupport} instance.
   *
   * @param peer  the bean context peer (<code>null</code> permitted).
   * @param locale  the locale (<code>null</code> permitted, equivalent to
   *     the default locale).
   * @param dtime  a flag indicating whether or not the bean context is in
   *     design time mode.
   */
  public BeanContextServicesSupport(BeanContextServices peer, Locale locale,
                                    boolean dtime)
  {
    super(peer, locale, dtime);
  }

  /**
   * Construct a {@link BeanContextServicesSupport} instance.
   *
   * @param peer  the bean context peer (<code>null</code> permitted).
   * @param locale  the locale (<code>null</code> permitted, equivalent to
   *     the default locale).
   * @param dtime  a flag indicating whether or not the bean context is in
   *     design time mode.
   * @param visible  initial value of the <code>okToUseGui</code> flag.
   */
  public BeanContextServicesSupport(BeanContextServices peer, Locale locale,
                                    boolean dtime, boolean visible)
  {
    super(peer, locale, dtime, visible);
  }

  /**
   * Adds a new listener for service availability and
   * revocation events.
   *
   * @param listener the listener to add.
   */
  public void addBeanContextServicesListener
    (BeanContextServicesListener listener)
  {
    synchronized (bcsListeners)
      {
        if (! bcsListeners.contains(listener))
          bcsListeners.add(listener);
      }
  }

  /**
   * Registers a new service from the specified service provider.
   * The service is internally associated with the service provider
   * and a <code>BeanContextServiceAvailableEvent</code> is fired.  If
   * the service is already registered, then this method instead
   * returns <code>false</code>.  This is equivalent to calling
   * <code>addService(serviceClass, bcsp, true)</code>.
   *
   * @param serviceClass the class of the service to be registered.
   * @param bcsp the provider of the given service.
   * @return true if the service was registered successfully.
   * @see #addService(Class, BeanContextServiceProvider, boolean)
   */
  public boolean addService (Class serviceClass,
                             BeanContextServiceProvider bcsp)
  {
    return addService(serviceClass, bcsp, true);
  }

  /**
   * Registers a new service from the specified service provider.
   * The service is internally associated with the service provider
   * and (if <code>fireEvent</code> is true) a
   * <code>BeanContextServiceAvailableEvent</code> is fired.  If
   * the service is already registered, then this method instead
   * returns <code>false</code>.
   *
   * @param serviceClass the class of the service to be registered.
   * @param bcsp the provider of the given service.
   * @param fireEvent true if a service availability event should
   *                  be fired.
   * @return true if the service was registered successfully.
   */
  protected boolean addService (Class serviceClass,
                                BeanContextServiceProvider bcsp,
                                boolean fireEvent)
  {
    synchronized (globalHierarchyLock)
      {
        synchronized (services)
          {
            if (services.containsKey(serviceClass))
              return false;
            services.put(serviceClass,
                         createBCSSServiceProvider(serviceClass, bcsp));
            if (bcsp instanceof Serializable)
              ++serializable;
            if (fireEvent)
              fireServiceAdded(serviceClass);
            return true;
          }
      }
  }

  /**
   * Deserializes any service providers which are serializable.  This
   * method is called by the <code>readObject</code> method of
   * {@link BeanContextSupport} prior to deserialization of the children.
   * Subclasses may envelope its behaviour in order to read further
   * serialized data to the stream.
   *
   * @param ois the stream from which data is being deserialized.
   * @throws IOException if an I/O error occurs.
   * @throws ClassNotFoundException if the class of a deserialized object
   *                                can not be found.
   */
  protected void bcsPreDeserializationHook (ObjectInputStream ois)
    throws ClassNotFoundException, IOException
  {
    serializable = ois.readInt();
    for (int a = 0; a < serializable; ++a)
      {
        BCSSServiceProvider bcsssp = (BCSSServiceProvider) ois.readObject();
        addService(bcsssp.getServiceClass(), bcsssp.getServiceProvider());
      }
  }

  /**
   * Serializes any service providers which are serializable.  This
   * method is called by the <code>writeObject</code> method of
   * {@link BeanContextSupport} prior to serialization of the children.
   * Subclasses may envelope its behaviour in order to add further
   * serialized data to the stream.
   *
   * @param oos the stream to which data is being serialized.
   * @throws IOException if an I/O error occurs.
   */
  protected void bcsPreSerializationHook (ObjectOutputStream oos)
    throws IOException
  {
    oos.writeInt(serializable);
    synchronized (services)
      {
        Iterator i = services.values().iterator();
        while (i.hasNext())
          {
            BCSSServiceProvider bcsssp = (BCSSServiceProvider) i.next();
            if (bcsssp.getServiceProvider() instanceof Serializable)
              oos.writeObject(bcsssp);
          }
      }
  }

  /**
   * Revokes any services used by a child that has just been removed.
   * The superclass ({@link BeanContextSupport}) calls this method
   * when a child has just been successfully removed.  Subclasses can
   * extend this method in order to perform additional operations
   * on child removal.
   *
   * @param child the child being removed.
   * @param bcsc the support object for the child.
   */
  protected void childJustRemovedHook (Object child,
                                       BeanContextSupport.BCSChild bcsc)
  {
    if (child instanceof BeanContextChild)
      {
        BeanContextChild bcchild = (BeanContextChild) child;
        Iterator childServices = ((List) serviceUsers.get(bcchild)).iterator();
        while (childServices.hasNext())
          releaseService(bcchild, this, childServices.next());
        serviceUsers.remove(bcchild);
      }
  }

  /**
   * Overrides the {@link BeanContextSupport#createBCSChild} method
   * so as to use a {@link BCSSChild} instead.
   *
   * @param targetChild the child to create the child for.
   * @param peer the peer which relates to the child if a proxy is used.
   * @return a new instance of {@link BCSSChild}.
   */
  protected BeanContextSupport.BCSChild createBCSChild (Object targetChild,
                                                        Object peer)
  {
    return new BCSSChild(targetChild, peer);
  }

  /**
   * Provides a hook so that subclasses can replace the
   * {@link BCSSServiceProvider} class, used to store registered
   * service providers, with a subclass without replacing the
   * {@link #addService(Class, BeanContextServiceProvider)} method.
   *
   * @param sc the class of service being registered.
   * @param bcsp the provider of the service.
   * @return a instance of {@link BCSSServiceProvider} wrapping the provider.
   */
  protected BeanContextServicesSupport.BCSSServiceProvider
  createBCSSServiceProvider (Class sc, BeanContextServiceProvider bcsp)
  {
    return new BCSSServiceProvider(sc, bcsp);
  }

  /**
   * Sends a <code>BeanContextServiceAvailableEvent</code> to all
   * registered listeners.
   *
   * @param bcssae the event to send.
   */
  protected final void fireServiceAdded (BeanContextServiceAvailableEvent bcssae)
  {
    synchronized (bcsListeners)
      {
        int size = bcsListeners.size();
        for (int i = 0; i < size; ++i)
          {
            BeanContextServicesListener bcsl
              = (BeanContextServicesListener) bcsListeners.get(i);
            bcsl.serviceAvailable(bcssae);
          }
      }
  }

  /**
   * Sends a <code>BeanContextServiceAvailableEvent</code> to all
   * registered listeners.
   *
   * @param serviceClass the service that is now available.
   * @see #fireServiceAdded(BeanContextServiceAvailableEvent)
   */
  protected final void fireServiceAdded (Class serviceClass)
  {
    fireServiceAdded(new BeanContextServiceAvailableEvent(this,
                                                          serviceClass));
  }

  /**
   * Sends a <code>BeanContextServiceRevokedEvent</code> to all
   * registered listeners.
   *
   * @param event the event to send.
   */
  protected final void fireServiceRevoked(BeanContextServiceRevokedEvent event)
  {
    synchronized (bcsListeners)
      {
        int size = bcsListeners.size();
        for (int i = 0; i < size; ++i)
          {
            BeanContextServicesListener bcsl
              = (BeanContextServicesListener) bcsListeners.get(i);
            bcsl.serviceRevoked(event);
          }
        List requests = (List) serviceRequests.get(event.getServiceClass());
        if (requests != null)
          {
            Iterator i = requests.iterator();
            while (i.hasNext())
              {
                ServiceRequest r = (ServiceRequest) i.next();
                r.getListener().serviceRevoked(event);
              }
          }
      }
  }

  /**
   * Sends a <code>BeanContextServiceRevokedEvent</code> to all
   * registered listeners.
   *
   * @param serviceClass the service that has been revoked.
   * @see #fireServiceRevoked(BeanContextServiceRevokedEvent)
   */
  protected final void fireServiceRevoked (Class serviceClass,
                                           boolean revokeNow)
  {
    fireServiceRevoked(new BeanContextServiceRevokedEvent(this, serviceClass,
                                                          revokeNow));
  }

  /**
   * Returns the services peer given at construction time,
   * or <code>null</code> if no peer was given.
   *
   * @return the {@link BeanContextServices} peer.
   */
  public BeanContextServices getBeanContextServicesPeer ()
  {
    return (BeanContextServices) beanContextChildPeer;
  }

  /**
   * Returns <code>child</code> as an instance of
   * {@link BeanContextServicesListener}, or <code>null</code> if
   * <code>child</code> does not implement that interface.
   *
   * @param child  the child (<code>null</code> permitted).
   *
   * @return The child cast to {@link BeanContextServicesListener}.
   */
  protected static final BeanContextServicesListener
      getChildBeanContextServicesListener(Object child)
  {
    if (child instanceof BeanContextServicesListener)
      return (BeanContextServicesListener) child;
    else
      return null;
  }

  /**
   * Returns an iterator over the currently available
   * services.
   *
   * @return an iterator over the currently available services.
   */
  public Iterator getCurrentServiceClasses ()
  {
    synchronized (globalHierarchyLock)
      {
        synchronized (services)
          {
            return services.keySet().iterator();
          }
      }
  }

  /**
   * Returns an iterator over the service selectors of the service
   * provider for the given service.  The iterator is actually
   * obtained by calling the
   * {@link BeanContextServiceProvider#getCurrentServiceSelectors}
   * of the provider itself.  If the specified service is not available,
   * <code>null</code> is returned.
   *
   * @param serviceClass the service whose provider's selectors should
   *                     be iterated over.
   * @return an {@link Iterator} over the service selectors of the
   *         provider of the given service.
   */
  public Iterator getCurrentServiceSelectors (Class serviceClass)
  {
    synchronized (globalHierarchyLock)
      {
        synchronized (services)
          {
            BeanContextServiceProvider bcsp
              = ((BCSSServiceProvider)
                 services.get(serviceClass)).getServiceProvider();
            if (bcsp == null)
              return null;
            else
              return bcsp.getCurrentServiceSelectors(this, serviceClass);
          }
      }
  }

  /**
   * Retrieves the specified service.  If a provider for the service
   * is registered in this context, then the request is passed on to
   * the provider and the service returned.  Otherwise, the request
   * is delegated to a parent {@link BeanContextServices}, if possible.
   * If the service can not be found at all, then <code>null</code>
   * is returned.
   *
   * @param child the child obtaining the reference.
   * @param requestor the requestor of the service, which may be the
   *                  child itself.
   * @param serviceClass the service being requested.
   * @param serviceSelector an additional service-dependent parameter
   *                        (may be <code>null</code> if not appropriate).
   * @param bcsrl a listener used to notify the requestor that the service
   *              has since been revoked.
   * @return a reference to the service requested, or <code>null</code>.
   * @throws TooManyListenersException according to Sun's documentation.
   */
  public Object getService (BeanContextChild child, Object requestor,
                            Class serviceClass, Object serviceSelector,
                            BeanContextServiceRevokedListener bcsrl)
    throws TooManyListenersException
  {
    synchronized (globalHierarchyLock)
      {
        synchronized (services)
          {
            Object service;
            BeanContextServiceProvider provider = ((BCSSServiceProvider)
              services.get(serviceClass)).getServiceProvider();
            if (provider != null)
              {
                service = provider.getService(this, requestor, serviceClass,
                                              serviceSelector);
                List childServices = (List) serviceUsers.get(child);
                if (childServices == null)
                  {
                    childServices = new ArrayList();
                    serviceUsers.put(child, childServices);
                  }
                childServices.add(serviceClass);
              }
            else
              {
                BeanContextServices peer = getBeanContextServicesPeer();
                if (peer != null)
                  service = peer.getService(child, requestor, serviceClass,
                                            serviceSelector, bcsrl);
                else
                  service = null;
              }
            if (service != null)
              {
                ServiceRequest request = new ServiceRequest(requestor, bcsrl);
                Set requests = (Set) serviceRequests.get(serviceClass);
                if (requests == null)
                  {
                    requests = new HashSet();
                    serviceRequests.put(serviceClass, requests);
                  }
                requests.add(request);
                ServiceLease lease = new ServiceLease(requestor, service);
                serviceLeases.put(lease, provider);
              }
            return service;
          }
      }
  }

  /**
   * Returns true if the specified service is available.
   *
   * @param serviceClass the service to check for.
   * @return true if the service is available.
   */
  public boolean hasService (Class serviceClass)
  {
    synchronized (globalHierarchyLock)
      {
        synchronized (services)
          {
            return services.containsKey(serviceClass);
          }
      }
  }

  public void initialize ()
  {
    super.initialize();

    bcsListeners = new ArrayList();
    services = new HashMap();
    serviceUsers = new HashMap();
    serviceRequests = new HashMap();
    serviceLeases = new HashMap();
  }

  /**
   * Subclasses may override this method to allocate resources
   * from the nesting bean context.
   */
  protected  void initializeBeanContextResources()
  {
    /* Purposefully left empty */
  }

  /**
   * Relinquishes any resources obtained from the parent context.
   * Specifically, those services obtained from the parent are revoked.
   * Subclasses may override this method to deallocate resources
   * from the nesting bean context.
   */
  protected void releaseBeanContextResources()
  {
    /* Purposefully left empty */
  }

  /**
   * Releases the reference to a service held by a
   * {@link BeanContextChild} (or an arbitrary object associated
   * with it).  It simply calls the appropriate method on the
   * underlying provider.
   *
   * @param child the child who holds the reference.
   * @param requestor the object that requested the reference.
   * @param service the service being released.
   */
  public void releaseService (BeanContextChild child, Object requestor,
                              Object service)
  {
    synchronized (globalHierarchyLock)
      {
        synchronized (services)
          {
            ServiceLease lease = new ServiceLease(requestor, service);
            BeanContextServiceProvider provider = (BeanContextServiceProvider)
              serviceLeases.get(lease);
            if (provider != null)
              provider.releaseService(this, requestor, service);
            else
              {
                BeanContextServices peer = getBeanContextServicesPeer();
                if (peer != null)
                  peer.releaseService(child, requestor, service);
              }
            serviceLeases.remove(lease);
          }
      }
  }

  public void removeBeanContextServicesListener
    (BeanContextServicesListener listener)
  {
    synchronized (bcsListeners)
      {
        bcsListeners.remove(listener);
      }
  }

  /**
   * Revokes the given service.  A {@link BeanContextServiceRevokedEvent} is
   * emitted to all registered {@link BeanContextServiceRevokedListener}s
   * and {@link BeanContextServiceListener}s.  If <code>revokeCurrentServicesNow</code>
   * is true, termination of the service is immediate.  Otherwise, prior
   * acquisitions of the service by requestors remain valid.
   *
   * @param serviceClass the service to revoke.
   * @param bcsp the provider of the revoked service.
   * @param revokeCurrentServicesNow true if this is an exceptional circumstance
   *                                 where service should be immediately revoked.
   */
  public void revokeService (Class serviceClass, BeanContextServiceProvider bcsp,
                             boolean revokeCurrentServicesNow)
  {
    synchronized (globalHierarchyLock)
      {
        synchronized (services)
          {
            fireServiceRevoked(serviceClass, revokeCurrentServicesNow);
            services.remove(serviceClass);
            if (bcsp instanceof Serializable)
              --serializable;
          }
      }
  }

  public void serviceAvailable (BeanContextServiceAvailableEvent bcssae)
  {
    synchronized (services)
      {
        Class klass = bcssae.getServiceClass();
        if (services.containsKey(klass))
          return;
        Iterator it = bcsChildren();
        while (it.hasNext())
          {
            Object obj = it.next();
            if (obj instanceof BeanContextServices)
              ((BeanContextServices) obj).serviceAvailable(bcssae);
          }
      }
  }

  public void serviceRevoked (BeanContextServiceRevokedEvent bcssre)
  {
    synchronized (services)
      {
        Class klass = bcssre.getServiceClass();
        if (services.containsKey(klass))
          return;
        Iterator it = bcsChildren();
        while (it.hasNext())
          {
            Object obj = it.next();
            if (obj instanceof BeanContextServices)
              ((BeanContextServices) obj).serviceRevoked(bcssre);
          }
      }
  }
}
