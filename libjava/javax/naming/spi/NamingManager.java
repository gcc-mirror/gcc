/* Copyright (C) 2000 Free Software Foundation
   
   This file is part of libgcj.
   
   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package javax.naming.spi;

import java.util.Hashtable;
import javax.naming.*;

public class NamingManager
{
  private static InitialContextFactoryBuilder icfb = null;

  public static boolean hasInitialContextFactoryBuilder ()
  {
    return icfb != null;
  }
  
  public static Context getInitialContext (Hashtable environment) throws NamingException
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
	  throw new NoInitialContextException ("Can't find property: " + Context.INITIAL_CONTEXT_FACTORY);
	
	try {
	  icf = (InitialContextFactory) Thread.currentThread().getContextClassLoader().loadClass(java_naming_factory_initial).newInstance();
	} catch (Exception exception) {
	  NoInitialContextException e
	    = new NoInitialContextException("Can't load InitialContextFactory class: " + java_naming_factory_initial);
	  e.setRootCause(exception);
	  throw e;
	}
      }
    
    return icf.getInitialContext (environment);
  }

  public static Context getURLContext(String scheme,
				      Hashtable environment) 
       throws NamingException
  {
    throw new Error ("javax.naming.spi.NamingManager.getURLContext not implemented");
  }
}
