/* NamingManager.java -- Creates contexts and objects
   Copyright (C) 2000, 2001, 2002, 2003, 2004,
   2006 Free Software Foundation, Inc.

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


package javax.naming.spi;

import gnu.classpath.VMStackWalker;

import gnu.java.lang.CPStringBuilder;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;

import javax.naming.CannotProceedException;
import javax.naming.Context;
import javax.naming.Name;
import javax.naming.NamingException;
import javax.naming.NoInitialContextException;
import javax.naming.RefAddr;
import javax.naming.Reference;
import javax.naming.Referenceable;
import javax.naming.StringRefAddr;

/**
 * Contains methods for creating contexts and objects referred to by
 * location information. The location is specified in the scope of the
 * certain naming or directory service. This class only contais static
 * methods and cannot be instantiated.
 */
public class NamingManager
{
  /**
   * The environment property into which getContinuationContext() stores the
   * value of the CannotProceedException parameter. The value of this field
   * is <i>java.naming.spi.CannotProceedException<i>.
   */
  public static final String CPE = "java.naming.spi.CannotProceedException";

  private static InitialContextFactoryBuilder icfb;

  // Package private so DirectoryManager can access it.
  static ObjectFactoryBuilder ofb;

  // This class cannot be instantiated.
  NamingManager ()
  {
  }

  /**
   * Checks if the initial context factory builder has been set.
   * 
   * @return true if the builder has been set
   * 
   * @see #setInitialContextFactoryBuilder(InitialContextFactoryBuilder)
   */
  public static boolean hasInitialContextFactoryBuilder ()
  {
    return icfb != null;
  }
  
  /**
   * Creates the initial context. If the initial object factory builder has
   * been set with {@link #setObjectFactoryBuilder(ObjectFactoryBuilder)},
   * the work is delegated to this builder. Otherwise, the method searches
   * for the property Context.INITIAL_CONTEXT_FACTORY first in the passed
   * table and then in the system properties. The value of this property is
   * uses as a class name to install the context factory. The corresponding
   * class must exist, be public and have the public parameterless constructor. 
   * 
   * @param environment the properties, used to create the context.
   * 
   * @return the created context
   * 
   * @throws NoInitialContextException if the initial builder is not set,
   *           the property Context.INITIAL_CONTEXT_FACTORY is missing of the
   *           class, named by this property, cannot be instantiated. 
   * @throws NamingException if throws by the context factory
   */
  public static Context getInitialContext (Hashtable<?, ?> environment)
    throws NamingException
  {
    InitialContextFactory icf = null;
    
    if (icfb != null)
      icf = icfb.createInitialContextFactory(environment);
    else
      {	 
	String java_naming_factory_initial = null;
	if (environment != null)
	  java_naming_factory_initial
	    = (String) environment.get (Context.INITIAL_CONTEXT_FACTORY);
	if (java_naming_factory_initial == null)
	  java_naming_factory_initial =
	    System.getProperty (Context.INITIAL_CONTEXT_FACTORY);
	if (java_naming_factory_initial == null)
	  throw new
	    NoInitialContextException ("Can't find property: "
				       + Context.INITIAL_CONTEXT_FACTORY);

	try
	  {
	    icf = (InitialContextFactory)Class.forName
		(java_naming_factory_initial, true,
		 Thread.currentThread().getContextClassLoader())
		.newInstance ();
	  }
	catch (Exception exception)
	  {
	    NoInitialContextException e
	      = new NoInitialContextException
	      ("Can't load InitialContextFactory class: "
	       + java_naming_factory_initial);
	    e.setRootCause(exception);
	    throw e;
	  }
      }

    return icf.getInitialContext (environment);
  }

  /**
   * <p>
   * Creates the URL context for the given URL scheme id.
   * </p>
   * <p>
   * The class name of the factory that creates the context has the naming
   * pattern scheme-idURLContextFactory. For instance, the factory for the "ftp"
   * sheme should be named "ftpURLContextFactory".
   * </p>
   * <p>
   * The Context.URL_PKG_PREFIXES environment property contains the
   * colon-separated list of the possible package prefixes. The package name is
   * constructed concatenating the package prefix with the scheme id. This
   * property is searched in the passed <i>environment</i> parameter and later
   * in the system properties.
   * </p>
   * <p>
   * If the factory class cannot be found in the specified packages, system will
   * try to use the default internal factory for the given scheme.
   * </p>
   * <p>
   * After the factory is instantiated, its method
   * {@link ObjectFactory#getObjectInstance(Object, Name, Context, Hashtable)}
   * is called to create and return the object instance.
   * 
   * @param refInfo passed to the factory
   * @param name passed to the factory
   * @param nameCtx passed to the factory
   * @param scheme the url scheme that must be supported by the given context
   * @param environment the properties for creating the factory and context (may
   *          be null)
   * @return the created context
   * @throws NamingException if thrown by the factory when creating the context.
   */
  static Context getURLContext(Object refInfo, Name name, Context nameCtx,
                               String scheme, Hashtable<?,?> environment)
      throws NamingException
  {
    // Doc specifies com.sun.jndi.url as the final destination, but we cannot
    // put our classes into such namespace.
    String defaultPrefix = "gnu.javax.naming.jndi.url";

    // The final default location, as specified in the documentation.
    String finalPrefix = "com.sun.jndi.url";
  
    CPStringBuilder allPrefixes = new CPStringBuilder();

    String prefixes;
      if (environment != null)
        {
        prefixes = (String) environment.get(Context.URL_PKG_PREFIXES);
        if (prefixes != null)
          allPrefixes.append(prefixes);
        }
  
    prefixes = System.getProperty(Context.URL_PKG_PREFIXES);
    if (prefixes != null)
      {
        if (allPrefixes.length() > 0)
          allPrefixes.append(':');
        allPrefixes.append(prefixes);
      }

    if (allPrefixes.length() > 0)
      allPrefixes.append(':');
    allPrefixes.append(defaultPrefix);
    allPrefixes.append(':');
    allPrefixes.append(finalPrefix);

      scheme = scheme + "." + scheme + "URLContextFactory";
  
    StringTokenizer tokens = new StringTokenizer(allPrefixes.toString(), ":");
    while (tokens.hasMoreTokens())
        {
        String aTry = tokens.nextToken();
        try
          {
            String tryClass = aTry + "." + scheme;
            Class factoryClass = forName(tryClass);
            if (factoryClass != null)
              {
                Object obj;
                try
                  {
                    ObjectFactory factory = (ObjectFactory) factoryClass.newInstance();
                    obj = factory.getObjectInstance(refInfo, name, nameCtx,
                                                    environment);
                    Context ctx = (Context) obj;
                    if (ctx != null)
                      return ctx;
                  }
                catch (RuntimeException e)
                  {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                  }
              }
          }
        catch (ClassNotFoundException _1)
          {
            // Ignore it.
          }
        catch (ClassCastException _2)
          {
            // This means that the class we found was not an
            // ObjectFactory or that the factory returned something
            // which was not a Context.
          }
        catch (InstantiationException _3)
          {
            // If we couldn't instantiate the factory we might get
            // this.
          }
        catch (IllegalAccessException _4)
          {
            // Another possibility when instantiating.
          }
        catch (NamingException _5)
          {
            throw _5;
          }
        catch (Exception _6)
          {
            // Anything from getObjectInstance.
          }
	}
    
    return null;
  }

  /**
   * Load the class with the given name. This method tries to use the context
   * class loader first. If this fails, it searches for the suitable class
   * loader in the caller stack trace. This method is a central point where all
   * requests to find a class by name are delegated.
   */
  static Class forName(String className)
  {
    try
      {
        return Class.forName(className, true,
                             Thread.currentThread().getContextClassLoader());
      }
    catch (ClassNotFoundException nex)
      {
        /**
         * Returns the first user defined class loader on the call stack, or
         * null when no non-null class loader was found.
         */
        Class[] ctx = VMStackWalker.getClassContext();
        for (int i = 0; i < ctx.length; i++)
          {
            // Since we live in a class loaded by the bootstrap
            // class loader, getClassLoader is safe to call without
            // needing to be wrapped in a privileged action.
            ClassLoader cl = ctx[i].getClassLoader();
            try
              {
                if (cl != null)
                  return Class.forName(className, true, cl);
              }
            catch (ClassNotFoundException nex2)
              {
                // Try next.
              }
          }
      }
    return null;
  }  
  
  
  /**
   * <p>
   * Creates the URL context for the given URL scheme id.
   * </p>
   * <p>
   * The class name of the factory that creates the context has the naming
   * pattern scheme-idURLContextFactory. For instance, the factory for the
   * "ftp" scheme should be named "ftpURLContextFactory".
   * The Context.URL_PKG_PREFIXES environment property contains the
   * colon-separated list of the possible package prefixes. The package name
   * is constructed by concatenating the package prefix with the scheme id.
   * </p>
   * <p>
   * If the factory class cannot be found in the specified packages, the
   * system will try to use the default internal factory for the given scheme.
   * </p>
   * <p>
   * After the factory is instantiated, its method
   * {@link ObjectFactory#getObjectInstance(Object, Name, Context, Hashtable)}
   * is called to create and return the object instance.
   * 
   * @param scheme the url scheme that must be supported by the given context
   * @param environment the properties for creating the factory and context
   *                    (may be null)
   * @return the created context
   * @throws NamingException if thrown by the factory when creating the
   *                         context.
   */
  public static Context getURLContext (String scheme,
				       Hashtable<?, ?> environment) 
       throws NamingException
  {
    return getURLContext (null, null, null, scheme, environment);
  }

  /**
   * Sets the initial object factory builder.
   * 
   * @param builder the builder to set
   * 
   * @throws SecurityException if the builder cannot be installed due
   *           security restrictions.
   * @throws NamingException if the builder cannot be installed due other 
   *           reasons
   * @throws IllegalStateException if setting the builder repeatedly
   */
  public static void setObjectFactoryBuilder (ObjectFactoryBuilder builder)
    throws NamingException
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkSetFactory ();
    // Once the builder is installed it cannot be replaced.
    if (ofb != null)
      throw new IllegalStateException ("object factory builder already installed");
    if (builder != null)
      ofb = builder;
  }

  static StringTokenizer getPlusPath (String property, Hashtable env,
				      Context nameCtx)
    throws NamingException
  {
    String path = (String) env.get (property);
    if (nameCtx == null)
      nameCtx = getInitialContext (env);
    String path2 = (String) nameCtx.getEnvironment ().get (property);
    if (path == null)
      path = path2;
    else if (path2 != null)
      path += ":" + path2;
    return new StringTokenizer (path != null ? path : "", ":");
  }
  
  /**
   * <p>Creates an object for the specified name context, environment and
   * referencing context object.</p>
   * <p>
   * If the builder factory is set by 
   * {@link #setObjectFactoryBuilder(ObjectFactoryBuilder)}, the call is
   * delegated to that factory. Otherwise, the object is created using the
   * following rules:
   * <ul>
   * <li>If the referencing object (refInfo) contains the factory class name,
   *       the object is created by this factory. If the creation fails,
   *       the parameter refInfo is returned as the method return value.</li>
   * <li>If the referencing object has no factory class name, and the addresses
   *       are StringRefAddrs having the address type "URL", the object is
   *       created by the URL context factory. The used factory corresponds the
   *       the naming schema of the each URL. If the attempt to create
   *       the object this way is not successful, the subsequent rule is 
   *       tried.</li>
   * <li>  If the refInfo is not an instance of Reference or Referencable
   *       (for example, null), the object is created by the factories,
   *       specified in the Context.OBJECT_FACTORIES property of the 
   *       environment and the provider resource file, associated with the
   *       nameCtx. The value of this property is the colon separated list
   *       of the possible factories. If none of the factories can be
   *       loaded, the refInfo is returned.            
   * </ul>
   * </p>
   * <p>The object factory must be public and have the public parameterless
   * constructor.</p>
   *  
   * @param refInfo the referencing object, for which the new object must be
   *          created (can be null). If not null, it is usually an instance of
   *          the {@link Reference} or {@link Referenceable}.
   * @param name the name of the object. The name is relative to
   *          the nameCtx naming context. The value of this parameter can be
   *          null if the name is not specified.
   * @param nameCtx the naming context, in which scope the name of the new
   *          object is specified. If this parameter is null, the name is
   *          specified in the scope of the initial context.
   * @param environment contains additional information for creating the object.
   *          This paramter can be null if there is no need to provide any
   *          additional information.
   *        
   * @return  the created object. If the creation fails, in some cases
   *          the parameter refInfo may be returned.
   * 
   * @throws NamingException if the attempt to name the new object has failed
   * @throws Exception if the object factory throws it. The object factory
   *           only throws an exception if it does not want other factories
   *           to be used to create the object.
   */
  public static Object getObjectInstance (Object refInfo,
					  Name name,
					  Context nameCtx,
					  Hashtable<?, ?> environment)
    throws Exception
  {
    ObjectFactory factory = null;

    if (ofb != null)
      factory = ofb.createObjectFactory (refInfo, environment);
    else
      {
	// First see if we have a Reference or a Referenceable.  If so
	// we do some special processing.
	Object ref2 = refInfo;
	if (refInfo instanceof Referenceable)
	  ref2 = ((Referenceable) refInfo).getReference ();
	if (ref2 instanceof Reference)
	  {
	    Reference ref = (Reference) ref2;

	    // If we have a factory class name then we use that.
	    String fClass = ref.getFactoryClassName ();
	    if (fClass != null)
	      {
		// Exceptions here are passed to the caller.
		Class k = Class.forName (fClass,
					 true,
					 Thread.currentThread().getContextClassLoader());
		factory = (ObjectFactory) k.newInstance ();
	      }
	    else
	      {
		// There's no factory class name.  If the address is a
		// StringRefAddr with address type `URL', then we try
		// the URL's context factory.
		Enumeration e = ref.getAll ();
		while (e.hasMoreElements ())
		  {
		    RefAddr ra = (RefAddr) e.nextElement ();
		    if (ra instanceof StringRefAddr
			&& "URL".equals (ra.getType ()))
		      {
			factory
			  = (ObjectFactory) getURLContext (refInfo,
							   name,
							   nameCtx,
							   (String) ra.getContent (),
							   environment);
			Object obj = factory.getObjectInstance (refInfo,
								name,
								nameCtx,
								environment);
			if (obj != null)
			  return obj;
		      }
		  }

		// Have to try the next step.
		factory = null;
	      }
	  }

	// Now look at OBJECT_FACTORIES to find the factory.
	if (factory == null)
	  {
	    StringTokenizer tokens = getPlusPath (Context.OBJECT_FACTORIES,
						  environment, nameCtx);

	    while (tokens.hasMoreTokens ())
	      {
		String klassName = tokens.nextToken ();
		Class k = Class.forName (klassName,
					 true,
					 Thread.currentThread().getContextClassLoader());
		factory = (ObjectFactory) k.newInstance ();
		Object obj = factory.getObjectInstance (refInfo, name,
							nameCtx, environment);
		if (obj != null)
		  return obj;
	      }

	    // Failure.
	    return refInfo;
	  }
      }

    if (factory == null)
      return refInfo;
    Object obj = factory.getObjectInstance (refInfo, name,
					    nameCtx, environment);
    return obj == null ? refInfo : obj;
  }

  /**
   * Sets the initial context factory builder.
   * 
   * @param builder the builder to set
   * 
   * @throws SecurityException if the builder cannot be installed due
   *           security restrictions.
   * @throws NamingException if the builder cannot be installed due other 
   *           reasons
   * @throws IllegalStateException if setting the builder repeatedly
   * 
   * @see #hasInitialContextFactoryBuilder()
   */
  public static void setInitialContextFactoryBuilder 
    (InitialContextFactoryBuilder builder)
    throws NamingException
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkSetFactory ();
    // Once the builder is installed it cannot be replaced.
    if (icfb != null)
      throw new IllegalStateException ("ctx factory builder already installed");
    if (builder != null)
      icfb = builder;
  }
  
  /**
   * Creates a context in which the context operation must be continued.
   * This method is used by operations on names that span multiple namespaces.
   * 
   * @param cpe the exception that triggered this continuation. This method
   * obtains the environment ({@link CannotProceedException#getEnvironment()}
   * and sets the environment property {@link #CPE} = cpe.
   * 
   * @return a non null context for continuing the operation
   * 
   * @throws NamingException if the naming problems have occured
   */
  public static Context getContinuationContext (CannotProceedException cpe)
    throws NamingException
  {
    Hashtable env = cpe.getEnvironment ();
    if (env != null)
      env.put (CPE, cpe);

    // TODO: Check if this implementation matches the API specification
    try
      {
	Object obj = getObjectInstance (cpe.getResolvedObj(),
					cpe.getAltName (),
					cpe.getAltNameCtx (), 
					env);
	if (obj != null)
	  return (Context) obj;
      }
    catch (Exception _)
      {
      }

    // fix stack trace for re-thrown exception (message confusing otherwise)
    cpe.fillInStackTrace();

    throw cpe;
  }
  
  /**
   * Get the object state for binding.
   * 
   * @param obj the object, for that the binding state must be retrieved. Cannot
   *          be null.
   * @param name the name of this object, related to the nameCtx. Can be null if
   *          not specified.
   * @param nameCtx the naming context, to that the object name is related. Can
   *          be null if the name is related to the initial default context.
   * @param environment the properties for creating the object state. Can be
   *          null if no properties are provided.
   * @return the object state for binding, may be null if no changes are
   *         returned by the factory
   * @throws NamingException
   */ 
  public static Object getStateToBind (Object obj, Name name,
				       Context nameCtx, Hashtable<?, ?> environment)
    throws NamingException
  {
    StringTokenizer tokens = getPlusPath (Context.STATE_FACTORIES,
					  environment, nameCtx);
    while (tokens.hasMoreTokens ())
      {
	String klassName = tokens.nextToken ();
	try
	  {
	    Class k = Class.forName (klassName,
				     true,
				     Thread.currentThread().getContextClassLoader());
	    StateFactory factory = (StateFactory) k.newInstance ();
	    Object o = factory.getStateToBind (obj, name, nameCtx,
					       environment);
	    if (o != null)
	      return o;
	  }
	catch (ClassNotFoundException _1)
	  {
	    // Ignore it.
	  }
	catch (ClassCastException _2)
	  {
	    // This means that the class we found was not an
	    // ObjectFactory or that the factory returned something
	    // which was not a Context.
	  }
	catch (InstantiationException _3)
	  {
	    // If we couldn't instantiate the factory we might get
	    // this.
	  }
	catch (IllegalAccessException _4)
	  {
	    // Another possibility when instantiating.
	  }
      }

    return obj;
  }
}
