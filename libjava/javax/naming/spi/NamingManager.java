/* NamingManager.java --
   Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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


package javax.naming.spi;

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

public class NamingManager
{
  public static final String CPE = "java.naming.spi.CannotProceedException";

  private static InitialContextFactoryBuilder icfb = null;

  // Package private so DirectoryManager can access it.
  static ObjectFactoryBuilder ofb = null;

  // This class cannot be instantiated.
  NamingManager ()
  {
  }

  public static boolean hasInitialContextFactoryBuilder ()
  {
    return icfb != null;
  }
  
  public static Context getInitialContext (Hashtable environment)
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

  static Context getURLContext (Object refInfo,
				Name name,
				Context nameCtx,
				String scheme,
				Hashtable environment) 
    throws NamingException
  {
    String prefixes = null;
    if (environment != null)
      prefixes = (String) environment.get (Context.URL_PKG_PREFIXES);
    if (prefixes == null)
      prefixes = System.getProperty (Context.URL_PKG_PREFIXES);
    if (prefixes == null)
      {
	// Specified as the default in the docs.  Unclear if this is
	// right for us.
	prefixes = "com.sun.jndi.url";
      }

    scheme = scheme + "." + scheme + "URLContextFactory";

    StringTokenizer tokens = new StringTokenizer (prefixes, ":");
    while (tokens.hasMoreTokens ())
      {
	String aTry = tokens.nextToken ();
	try
	  {
	    Class factoryClass = Class.forName (aTry + "." + scheme,
						true,
						Thread.currentThread().getContextClassLoader());
	    ObjectFactory factory =
	      (ObjectFactory) factoryClass.newInstance ();
	    Object obj = factory.getObjectInstance (refInfo, name,
						    nameCtx, environment);
	    Context ctx = (Context) obj;
	    if (ctx != null)
	      return ctx;
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

  public static Context getURLContext (String scheme,
				       Hashtable environment) 
       throws NamingException
  {
    return getURLContext (null, null, null, scheme, environment);
  }

  public static void setObjectFactoryBuilder (ObjectFactoryBuilder builder)
    throws NamingException
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkSetFactory ();
    // Once the builder is installed it cannot be replaced.
    if (ofb != null)
      throw new IllegalStateException ("builder already installed");
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

  public static Object getObjectInstance (Object refInfo,
					  Name name,
					  Context nameCtx,
					  Hashtable environment)
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

  public static void setInitialContextFactoryBuilder (InitialContextFactoryBuilder builder)
    throws NamingException
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkSetFactory ();
    // Once the builder is installed it cannot be replaced.
    if (icfb != null)
      throw new IllegalStateException ("builder already installed");
    if (builder != null)
      icfb = builder;
  }

  public static Context getContinuationContext (CannotProceedException cpe)
    throws NamingException
  {
    Hashtable env = cpe.getEnvironment ();
    if (env != null)
      env.put (CPE, cpe);

    // It is really unclear to me if this is right.
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

  public static Object getStateToBind (Object obj, Name name,
				       Context nameCtx, Hashtable environment)
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
