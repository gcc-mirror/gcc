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


package java.beans.beancontext;

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
    private static final long serialVersionUID = -6848044915271367103L;
  }

  protected class BCSSProxyServiceProvider
    implements BeanContextServiceProvider,
    BeanContextServiceRevokedListener
  {
    private static final long serialVersionUID = 7078212910685744490L;
    
    public Iterator getCurrentServiceSelectors (BeanContextServices bcs,
                                                Class serviceClass)
    {
      throw new Error ("Not implemented");
    }

    public Object getService (BeanContextServices bcs,
                              Object requestor,
                              Class serviceClass,
                              Object serviceSelector)
    {
      throw new Error ("Not implemented");
    }

    public void releaseService (BeanContextServices bcs,
                                Object requestor,
                                Object service)
    {
      throw new Error ("Not implemented");
    }

    public void serviceRevoked (BeanContextServiceRevokedEvent bcsre)
    {
      throw new Error ("Not implemented");
    }
  }

  protected static class BCSSServiceProvider
    implements Serializable
  {
    private static final long serialVersionUID = 861278251667444782L;

    protected BeanContextServiceProvider serviceProvider;

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
    if (! bcsListeners.contains(listener))
      bcsListeners.add(listener);
  }

  public boolean addService (Class serviceClass, BeanContextServiceProvider bcsp)
  {
    throw new Error ("Not implemented");
  }

  protected boolean addService (Class serviceClass,
                                BeanContextServiceProvider bcsp,
                                boolean fireEvent)
  {
    throw new Error ("Not implemented");
  }
  
  protected void bcsPreDeserializationHook (ObjectInputStream ois)
    throws ClassNotFoundException, IOException
  {
    throw new Error ("Not implemented");
  }

  protected void bcsPreSerializationHook (ObjectOutputStream oos) 
    throws IOException
  {
    throw new Error ("Not implemented");
  }
  
  protected void childJustRemovedHook (Object child,
                                       BeanContextSupport.BCSChild bcsc)
  {
    throw new Error ("Not implemented");
  }

  protected BeanContextSupport.BCSChild createBCSChild (Object targetChild,
                                                        Object peer) 
  {
    throw new Error ("Not implemented");
  }

  protected BeanContextServicesSupport.BCSSServiceProvider
  createBCSSServiceProvider (Class sc, BeanContextServiceProvider bcsp)
  {
    throw new Error ("Not implemented");
  }

  protected final void fireServiceAdded (BeanContextServiceAvailableEvent bcssae)
  {
    throw new Error ("Not implemented");
  }

  protected final void fireServiceAdded (Class serviceClass) 
  {
    throw new Error ("Not implemented");
  }

  protected final void fireServiceRevoked(BeanContextServiceRevokedEvent event)
  {
    throw new Error ("Not implemented");
  }

  protected final void fireServiceRevoked (Class serviceClass,
                                           boolean revokeNow)
  {
    throw new Error ("Not implemented");
  }

  public BeanContextServices getBeanContextServicesPeer ()
  {
    throw new Error ("Not implemented");
  }

  protected static final BeanContextServicesListener
  getChildBeanContextServicesListener (Object child) 
  {
    throw new Error ("Not implemented");
  }

  public Iterator getCurrentServiceClasses () 
  {
    throw new Error ("Not implemented");
  }

  public Iterator getCurrentServiceSelectors (Class serviceClass) 
  {
    throw new Error ("Not implemented");
  }

  public Object getService (BeanContextChild child, Object requestor,
                            Class serviceClass, Object serviceSelector,
                            BeanContextServiceRevokedListener bcsrl)
    throws TooManyListenersException
  {
    throw new Error ("Not implemented");
  }

  public boolean hasService (Class serviceClass)
  {
    throw new Error ("Not implemented");
  }

  public void initialize ()
  {
    super.initialize();

    bcsListeners = new ArrayList();
    services = new HashMap();
  }

  protected  void initializeBeanContextResources () 
  {
    throw new Error ("Not implemented");
  }

  protected  void releaseBeanContextResources () 
  {
    throw new Error ("Not implemented");
  }

  public void releaseService (BeanContextChild child, Object requestor,
                              Object service)
  {
    throw new Error ("Not implemented");
  }

  public void removeBeanContextServicesListener
    (BeanContextServicesListener listener)
  {
    int index = bcsListeners.indexOf(listener);

    if (index > -1)
      bcsListeners.remove(index);
  }

  public void revokeService (Class serviceClass, BeanContextServiceProvider bcsp,
                             boolean revokeCurrentServicesNow) 
  {
    throw new Error ("Not implemented");
  }

  public void serviceAvailable (BeanContextServiceAvailableEvent bcssae) 
  {
    throw new Error ("Not implemented");
  }

  public void serviceRevoked (BeanContextServiceRevokedEvent bcssre) 
  {
    throw new Error ("Not implemented");
  }
}
