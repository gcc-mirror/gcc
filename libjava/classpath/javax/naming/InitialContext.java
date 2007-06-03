/* InitialContext.java -- Initial naming context.
   Copyright (C) 2000, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

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


package javax.naming;

import java.applet.Applet;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Properties;

import javax.naming.spi.NamingManager;

/**
 * The starting context for performing naming operations. All naming operations
 * are performed in the scope of some context. The initial context is the
 * starting point for the name resolution.
 */
public class InitialContext implements Context
{
  /**
   * Contains the default initial context. This value is returned by
   * {@link NamingManager#getInitialContext}. It is set by this method
   * when calling it first time. The subsequent calls return the value of
   * this field.
   */
  protected Context defaultInitCtx;
  
  /**
   * Indicates if the initial context was obtained by calling
   * {@link NamingManager#getInitialContext}. 
   */
  protected boolean gotDefault = false;
  
  /**
   * The environment, associated with this initial context.
   */
  protected Hashtable<Object,Object> myProps;
  
  /**
   * The list of the properties, to that the second alternative value must
   * be appended after the colon to the first possible value. Used in
   * {@link #merge(Hashtable, Hashtable)}
   */
  static final HashSet<String> colon_list;
  static
    {
      colon_list = new HashSet<String>();
      colon_list.add(Context.OBJECT_FACTORIES);
      colon_list.add(Context.URL_PKG_PREFIXES);
      colon_list.add(Context.STATE_FACTORIES);
    }  
    
   /**
    * The properties that are searched in the agreed places in the
    * {@link #init(Hashtable)} method.
    */
    static final String[] use_properties = 
      {
        Context.DNS_URL,
        Context.INITIAL_CONTEXT_FACTORY,
        Context.OBJECT_FACTORIES,
        Context.PROVIDER_URL,
        Context.STATE_FACTORIES,
        Context.URL_PKG_PREFIXES,
      };
    
  
  /**
   * Creates the new initial context with the given properties.
   * 
   * @param environment the properties, used by the initial context being
   *          created.
   * @throws NamingException
   */
  public InitialContext(Hashtable<?,?> environment) throws NamingException
  {
    init(environment);
  }
  
  /**
   * Creates the initial context with the possibility to delay its
   * initialisation.
   * 
   * @param lazy specified if the initialization should not be performed by this
   *          constructor (true). If the valueis false, it works the same way as
   *          the parameterless constructor.
   * @throws NamingException
   */
  protected InitialContext(boolean lazy) throws NamingException
  {
    if (! lazy)
      init(null);
  }
  
  /**
   * Creates teh new initial context with no properties. Same as
   * InitialContext(null).
   * 
   * @throws NamingException
   */
  public InitialContext() throws NamingException
  {
    init(null);
  }
 
  /**
   * <p>
   * Initialises the context, using the properties, specified in the passed
   * table.
   * </p>
   * The missing properties are additionally obtained (in order) from the
   * following locations:
   * <ul>
   * <li>If the passed parameter contains the key Context.APPLET, its value
   * must be the instance of the {@link Applet}. Then the properties are
   * requested via {@link Applet#getParameter(String)}.</li>
   * <li>The value of the system property is used.</li>
   * <li>The resource "jndi.properties" is requested from the context class
   * loader of the current thread</li>
   * <li>The property file "jndi.properties" is read from the location,
   * specified by the system property "gnu.classpath.home.url".
   * </ul>
   * </p>
   * 
   * @param environment the table of the properties, may be null. The method
   *          modifies the table and stores the reference to it. The caller must
   *          not later reuse this structure for other purposes.
   * @since 1.3
   */
  protected void init(Hashtable<?, ?> environment) throws NamingException
  {
    // If is documented that the caller should not modify the environment.
    if (environment != null)
      myProps = (Hashtable<Object, Object>) environment;
    else
      myProps = new Hashtable<Object, Object>();

    Applet napplet = (Applet) myProps.get(Context.APPLET);

    Properties pApplet = null;
    if (napplet != null)
      pApplet = new Properties();
    Properties pSystem = new Properties();
    Object value;

    for (int i = use_properties.length - 1; i >= 0; i--)
      {
        String key = use_properties[i];
        if (napplet != null)
          {
            value = napplet.getParameter(key);
            if (value != null)
              pApplet.put(key, value);
          }
        
        value = System.getProperty(key);
        if (value != null)
          pSystem.put(key, value);
      }
    
    merge(myProps, pSystem);
    if (pApplet != null)
      merge(myProps, pApplet);

    try
      {
        Enumeration ep = Thread.currentThread().
          getContextClassLoader().getResources("jndi.properties");
        while (ep.hasMoreElements())
          {
            URL url = (URL) ep.nextElement();
            Properties p = new Properties();

            try
              {
                InputStream is = url.openStream();
                p.load(is);
                is.close();
              }
            catch (IOException e)
              {
                // Ignore.
              }

            merge(myProps, p);
          }
      }
    catch (IOException e)
      {
        // Ignore.
      }

    String home = System.getProperty("gnu.classpath.home.url");
    if (home != null)
      {
        String url = home + "/jndi.properties";
        Properties p = new Properties();

        try
          {
            InputStream is = new URL(url).openStream();
            p.load(is);
            is.close();
          }
        catch (IOException e)
          {
            // Ignore.
          }

        merge(myProps, p);
      }
  }
  
  /**
   * Merge the content of the two tables. If the second table contains the key
   * that is missing in the first table, this key - value pair is copied to the
   * first table. If both first and second tables contain the same key AND the
   * {@link #colon_list} set also contains this key, the value from the second
   * table is appended to the value from the first table after semicolon, and
   * the resulted value replaces the value in the first table.
   * 
   * @param primary the first table to merge. The merged result is also stored
   *          in this table.
   * @param additional the second table, from where additional values are taken
   */  
  static void merge (Hashtable<Object, Object> primary,
                     Hashtable<Object, Object> additional)
  {
    Enumeration en = additional.keys();
    
    while (en.hasMoreElements())
      {
        String key2 = (String) en.nextElement();
        Object value1 = primary.get(key2);
        if (value1 == null)
          primary.put(key2, additional.get(key2));
        else if (colon_list.contains(key2))
          {
            String value2 = (String) additional.get(key2);
            primary.put(key2, (String) value1 + ":" + value2);
          }
      }
  }
  
  /**
   * Get the default initial context. If {@link #gotDefault} == false, this
   * method obtains the initial context from the naming manager and sets
   * gotDefault to true. Otherwise the cached value ({@link #defaultInitCtx} is
   * returned.
   * 
   * @return the default initial context
   * @throws NamingException
   */
  protected Context getDefaultInitCtx() throws NamingException
  {
    if (! gotDefault)
      {
        defaultInitCtx = NamingManager.getInitialContext(myProps);
        gotDefault = true;
      }
    return defaultInitCtx;
  }

  /**
   * Obtains the context for resolving the given name. If the first component of
   * the name is the URL string, this method tries to find the corressponding
   * URL naming context. If it is not an URL string, or the URL context is not
   * found, the default initial context is returned.
   * 
   * @param name the name, for that it is required to obtain the context.
   * @return the context for resolving the name.
   * @throws NamingException
   */
  protected Context getURLOrDefaultInitCtx(Name name) throws NamingException
  {
    if (name.size() > 0)
      return getURLOrDefaultInitCtx(name.get(0));
    else
      return getDefaultInitCtx();
  }

  /**
   * Obtains the context for resolving the given name. If the first component of
   * the name is the URL string, this method tries to find the corressponding
   * URL naming context. If it is not an URL string, or the URL context is not
   * found, the default initial context is returned.
   * 
   * @param name the name, for that it is required to obtain the context.
   * @return the context for resolving the name.
   * @throws NamingException
   */
  protected Context getURLOrDefaultInitCtx(String name) throws NamingException
  {
    String scheme = null;

    if (NamingManager.hasInitialContextFactoryBuilder())
      return getDefaultInitCtx();
    int colon = name.indexOf(':');
    int slash = name.indexOf('/');
    if (colon > 0 && (slash == - 1 || colon < slash))
      scheme = name.substring(0, colon);
    if (scheme != null)
      {
        Context context = NamingManager.getURLContext(scheme, myProps);
        if (context != null)
          return context;
      }

    return getDefaultInitCtx();
  }

  /** @inheritDoc */  
  public void bind (Name name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).bind (name, obj);
  }

  /** @inheritDoc */  
  public void bind (String name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).bind (name, obj);
  }

  /** @inheritDoc */  
  public Object lookup (Name name) throws NamingException
  {
    try
      {
        return getURLOrDefaultInitCtx (name).lookup (name);
      }
    catch (CannotProceedException cpe)
      {
        Context ctx = NamingManager.getContinuationContext (cpe);
        return ctx.lookup (cpe.getRemainingName());
      }
  }

  /** @inheritDoc */  
  public Object lookup (String name) throws NamingException
  {
      try
        {
          return getURLOrDefaultInitCtx (name).lookup (name);
        }
      catch (CannotProceedException cpe)
        {
          Context ctx = NamingManager.getContinuationContext (cpe);
          return ctx.lookup (cpe.getRemainingName());
        }
  }

  /** @inheritDoc */  
  public void rebind (Name name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).rebind (name, obj);
  }
  
  /** @inheritDoc */
  public void rebind (String name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).rebind (name, obj);
  }

  /** @inheritDoc */  
  public void unbind (Name name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).unbind (name);
  }

  /** @inheritDoc */  
  public void unbind (String name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).unbind (name);
  }

  /** @inheritDoc */  
  public void rename (Name oldName, Name newName) throws NamingException
  {
    getURLOrDefaultInitCtx (oldName).rename (oldName, newName);
  }

  /** @inheritDoc */  
  public void rename (String oldName, String newName) throws NamingException
  {
    getURLOrDefaultInitCtx (oldName).rename (oldName, newName);
  }

  /** @inheritDoc */  
  public NamingEnumeration<NameClassPair> list (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).list (name);
  }

  /** @inheritDoc */  
  public NamingEnumeration<NameClassPair> list (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).list (name);
  }

  /** @inheritDoc */  
  public NamingEnumeration<Binding> listBindings (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).listBindings (name);
  }

  /** @inheritDoc */  
  public NamingEnumeration<Binding> listBindings (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).listBindings (name);
  }

  /** @inheritDoc */  
  public void destroySubcontext (Name name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).destroySubcontext (name);
  }

  /** @inheritDoc */  
  public void destroySubcontext (String name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).destroySubcontext (name);
  }

  /** @inheritDoc */  
  public Context createSubcontext (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).createSubcontext (name);
  }

  /** @inheritDoc */  
  public Context createSubcontext (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).createSubcontext (name);
  }

  /** @inheritDoc */  
  public Object lookupLink (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).lookupLink (name);
  }

  /** @inheritDoc */  
  public Object lookupLink (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).lookupLink (name);
  }

  /** @inheritDoc */  
  public NameParser getNameParser (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).getNameParser (name);
  }

  /** @inheritDoc */  
  public NameParser getNameParser (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).getNameParser (name);
  }

  /** @inheritDoc */  
  public Name composeName (Name name, Name prefix) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).composeName (name, prefix);
  }

  /** @inheritDoc */  
  public String composeName (String name, 
                             String prefix) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).composeName (name, prefix);
  }
  
  /** @inheritDoc */
  public Object addToEnvironment (String propName, 
                                  Object propVal) throws NamingException
  {
    return myProps.put (propName, propVal);
  }

  /** @inheritDoc */  
  public Object removeFromEnvironment (String propName) throws NamingException
  {
    return myProps.remove (propName);
  }

  /** @inheritDoc */  
  public Hashtable<?,?> getEnvironment () throws NamingException
  {
    return myProps;
  }

  /** @inheritDoc */  
  public void close () throws NamingException
  {
    myProps = null;
    defaultInitCtx = null;
  }

  /**
   * This operation is not supported for the initial naming context.
   * 
   * @throws OperationNotSupportedException always, unless the method is
   *           overridden in the derived class.
   */
  public String getNameInNamespace () throws NamingException
  {
    throw new OperationNotSupportedException ();
  }
}
