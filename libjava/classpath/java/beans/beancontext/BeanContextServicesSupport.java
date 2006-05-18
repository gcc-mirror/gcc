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

import gnu.classpath.NotImplementedException;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.TooManyListenersException;

/**
 * @author Michael Koch
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

    private BCSSProxyServiceProvider()
    {
    }

    public Iterator getCurrentServiceSelectors (BeanContextServices bcs,
                                                Class serviceClass)
      throws NotImplementedException
    {
      throw new Error ("Not implemented");
    }

    public Object getService (BeanContextServices bcs,
                              Object requestor,
                              Class serviceClass,
                              Object serviceSelector)
      throws NotImplementedException
    {
      throw new Error ("Not implemented");
    }

    public void releaseService (BeanContextServices bcs,
                                Object requestor,
                                Object service)
      throws NotImplementedException
    {
      throw new Error ("Not implemented");
    }

    public void serviceRevoked (BeanContextServiceRevokedEvent bcsre)
      throws NotImplementedException
    {
      throw new Error ("Not implemented");
    }
  }

  protected static class BCSSServiceProvider
    implements Serializable
  {
    private static final long serialVersionUID = 861278251667444782L;

    protected BeanContextServiceProvider serviceProvider;

    private BCSSServiceProvider()
    {
    }

    protected BeanContextServiceProvider getServiceProvider()
    {
      return serviceProvider;
    }
  }

  protected transient ArrayList bcsListeners;

  protected transient BCSSProxyServiceProvider proxy;

  protected transient int serializable;

  protected transient HashMap services;

  public BeanContextServicesSupport ()
  {
    super();
  }

  public BeanContextServicesSupport (BeanContextServices peer)
  {
    super(peer);
  }

  public BeanContextServicesSupport(BeanContextServices peer, Locale locale)
  {
    super(peer, locale);
  }

  public BeanContextServicesSupport(BeanContextServices peer, Locale locale,
                                    boolean dtime)
  {
    super(peer, locale, dtime);
  }

  public BeanContextServicesSupport(BeanContextServices peer, Locale locale,
                                    boolean dtime, boolean visible)
  {
    super(peer, locale, dtime, visible);
  }

  public void addBeanContextServicesListener
    (BeanContextServicesListener listener)
  {
    synchronized (bcsListeners)
      {
        if (! bcsListeners.contains(listener))
          bcsListeners.add(listener);
      }
  }

  public boolean addService (Class serviceClass,
                             BeanContextServiceProvider bcsp)
  {
    return addService(serviceClass, bcsp, true);
  }

  protected boolean addService (Class serviceClass,
                                BeanContextServiceProvider bcsp,
                                boolean fireEvent)
  {
    synchronized (services)
      {
        if (services.containsKey(serviceClass))
          return false;
        services.put(serviceClass, bcsp);
        if (bcsp instanceof Serializable)
          ++serializable;
        fireServiceAdded(serviceClass);
        return true;
      }
  }
  
  protected void bcsPreDeserializationHook (ObjectInputStream ois)
    throws ClassNotFoundException, IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected void bcsPreSerializationHook (ObjectOutputStream oos) 
    throws IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }
  
  protected void childJustRemovedHook (Object child,
                                       BeanContextSupport.BCSChild bcsc)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected BeanContextSupport.BCSChild createBCSChild (Object targetChild,
                                                        Object peer)
  {
    return new BCSSChild(targetChild, peer);
  }

  protected BeanContextServicesSupport.BCSSServiceProvider
  createBCSSServiceProvider (Class sc, BeanContextServiceProvider bcsp)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

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

  protected final void fireServiceAdded (Class serviceClass)
  {
    fireServiceAdded(new BeanContextServiceAvailableEvent(this,
                                                          serviceClass));
  }

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
      }
  }

  protected final void fireServiceRevoked (Class serviceClass,
                                           boolean revokeNow)
  {
    fireServiceRevoked(new BeanContextServiceRevokedEvent(this, serviceClass,
                                                          revokeNow));
  }

  public BeanContextServices getBeanContextServicesPeer ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected static final BeanContextServicesListener
  getChildBeanContextServicesListener (Object child)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public Iterator getCurrentServiceClasses ()
  {
    synchronized (services)
      {
        return services.keySet().iterator();
      }
  }

  public Iterator getCurrentServiceSelectors (Class serviceClass)
  {
    synchronized (services)
      {
        // FIXME: what if service does not exist?  Must write a test.
        BeanContextServiceProvider bcsp
          = (BeanContextServiceProvider) services.get(serviceClass);
        return bcsp.getCurrentServiceSelectors(this, serviceClass);
      }
  }

  public Object getService (BeanContextChild child, Object requestor,
                            Class serviceClass, Object serviceSelector,
                            BeanContextServiceRevokedListener bcsrl)
    throws TooManyListenersException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public boolean hasService (Class serviceClass)
  {
    synchronized (services)
      {
        return services.containsKey(serviceClass);
      }
  }

  public void initialize ()
  {
    super.initialize();

    bcsListeners = new ArrayList();
    services = new HashMap();
  }

  protected  void initializeBeanContextResources ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected  void releaseBeanContextResources ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public void releaseService (BeanContextChild child, Object requestor,
                              Object service)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public void removeBeanContextServicesListener
    (BeanContextServicesListener listener)
  {
    synchronized (bcsListeners)
      {
        int index = bcsListeners.indexOf(listener);
        if (index > -1)
          bcsListeners.remove(index);
      }
  }

  public void revokeService (Class serviceClass, BeanContextServiceProvider bcsp,
                             boolean revokeCurrentServicesNow)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
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
