/* Copyright (C) 2000 Free Software Foundation
   
   This file is part of libgcj.
   
   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package javax.naming;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Properties;
import java.applet.Applet;
import java.util.Hashtable;
import javax.naming.spi.NamingManager;

public class InitialContext implements Context
{
  protected Context defaultInitCtx;
  protected boolean gotDefault = false;
  protected Hashtable myProps;
  
  public InitialContext (Hashtable environment)
    {
      init (environment);
    }
  
  protected InitialContext (boolean lazy)
    {
      if (! lazy)
	init (null);
    }
  
  public InitialContext ()
    {
      init (null);
    }
  
  protected void init (Hashtable environment)
    {
      // FIXME: Is this enough?
      final String[] properties = {
	Context.DNS_URL,
	Context.INITIAL_CONTEXT_FACTORY,
	Context.OBJECT_FACTORIES,
	Context.PROVIDER_URL,
	Context.STATE_FACTORIES,
	Context.URL_PKG_PREFIXES,
      };
      
      // Create myProps, cloning environment if needed.
      if (environment != null)
	myProps = (Hashtable) environment.clone ();
      else
	myProps = new Hashtable ();
      
      Applet napplet = (Applet) myProps.get (Context.APPLET);
      
      for (int i = properties.length - 1; i >= 0; i--)
	{
	  Object o = myProps.get (properties[i]);
	  
	  if (o == null)
	    {
	      if (napplet != null)
		o = napplet.getParameter (properties[i]);
	      if (o == null)
		o = System.getProperty (properties[i]);
	      if (o != null)
		myProps.put (properties[i], o);
	    }
	}
      
      try
	{
	    Enumeration ep = Thread.currentThread().getContextClassLoader().getResources("jndi.naming");
	    while (ep.hasMoreElements ())
		{
		    URL url = (URL) ep.nextElement ();
		    Properties p = new Properties ();
		    
		    try {
			InputStream is = url.openStream ();
			p.load (is);
			is.close ();
		    } catch (IOException e) {}

		    merge (myProps, p);
		}
	}
      catch (IOException e) {}

      String home = System.getProperty("java.home");
      if (home != null)
	{
	  String fileName = home + File.separator
	    + "lib" + File.separator + "jndi.properties";
	  Properties p = new Properties ();
	
	  try {
	    InputStream is = new FileInputStream (fileName);
	    p.load (is);
	    is.close ();
	  } catch (IOException e) {}

	  merge (myProps, p);
	}
    }

  // FIXME: Is this enough?
  private static final String[] colon_list = 
  {
    Context.OBJECT_FACTORIES,
    Context.URL_PKG_PREFIXES,
    Context.STATE_FACTORIES
  };

  private static void merge (Hashtable h1, Hashtable h2)
    {
      Enumeration e2 = h2.keys();
    
      while (e2.hasMoreElements())
	{
	  String key2 = (String) e2.nextElement();
	  Object value1 = h1.get(key2);
	  if (value1 == null)
	    h1.put(key2, h2.get(key2));
	  else if (key2.compareTo(colon_list[0]) == 0
		   || key2.compareTo(colon_list[1]) == 0
		   || key2.compareTo(colon_list[2]) == 0
		   || key2.compareTo(colon_list[3]) == 0)
	    {
	      String value2 = (String) h2.get(key2);
	      h1.put(key2, (String) value1 + ":" + value2);
	    }
	}
    }

  protected Context getDefaultInitCtx () throws NamingException
    {
      if (! gotDefault)
	{
	  defaultInitCtx = NamingManager.getInitialContext (myProps);
	  gotDefault = true;
	}
      return defaultInitCtx;
    }


  protected Context getURLOrDefaultInitCtx (Name name) 
      throws NamingException
    {
      if (name.size () > 0)
	  return getURLOrDefaultInitCtx (name.get (0));
      else
	  return getDefaultInitCtx ();
    }

  protected Context getURLOrDefaultInitCtx (String name) 
      throws NamingException
    {
      String scheme = null;

      if (NamingManager.hasInitialContextFactoryBuilder())
	return getDefaultInitCtx();
      int colon = name.indexOf(':');
      int slash = name.indexOf('/');
      if (colon > 0 && (slash == -1 || colon < slash))
	scheme = name.substring(0, colon);
      if (scheme != null) 
	{
	  Context context = 
	      NamingManager.getURLContext(scheme, myProps);
	  if (context != null)
	    return context;
	}
	
      return getDefaultInitCtx();
    }

  public void bind (Name name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).bind (name, obj);
  }

  public void bind (String name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).bind (name, obj);
  }

  public Object lookup (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).lookup (name);
  }

  public Object lookup (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).lookup (name);
  }

  public void rebind (Name name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).rebind (name, obj);
  }

  public void rebind (String name, Object obj) throws NamingException
  {
    getURLOrDefaultInitCtx (name).rebind (name, obj);
  }

  public void unbind (Name name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).unbind (name);
  }

  public void unbind (String name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).unbind (name);
  }

  public void rename (Name oldName, Name newName) throws NamingException
  {
    getURLOrDefaultInitCtx (oldName).rename (oldName, newName);
  }

  public void rename (String oldName, String newName) throws NamingException
  {
    getURLOrDefaultInitCtx (oldName).rename (oldName, newName);
  }

  public NamingEnumeration list (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).list (name);
  }

  public NamingEnumeration list (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).list (name);
  }

  public NamingEnumeration listBindings (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).listBindings (name);
  }

  public NamingEnumeration listBindings (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).listBindings (name);
  }

  public void destroySubcontext (Name name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).destroySubcontext (name);
  }

  public void destroySubcontext (String name) throws NamingException
  {
    getURLOrDefaultInitCtx (name).destroySubcontext (name);
  }

  public Context createSubcontext (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).createSubcontext (name);
  }

  public Context createSubcontext (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).createSubcontext (name);
  }

  public Object lookupLink (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).lookupLink (name);
  }

  public Object lookupLink (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).lookupLink (name);
  }

  public NameParser getNameParser (Name name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).getNameParser (name);
  }

  public NameParser getNameParser (String name) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).getNameParser (name);
  }

  public Name composeName (Name name, Name prefix) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).composeName (name, prefix);
  }

  public String composeName (String name, 
                           String prefix) throws NamingException
  {
    return getURLOrDefaultInitCtx (name).composeName (name, prefix);
  }

  public Object addToEnvironment (String propName, 
                                Object propVal) throws NamingException
  {
    return myProps.put (propName, propVal);
  }

  public Object removeFromEnvironment (String propName) throws NamingException
  {
    return myProps.remove (propName);
  }

  public Hashtable getEnvironment () throws NamingException
  {
    return myProps;
  }

  public void close () throws NamingException
  {
    throw new OperationNotSupportedException ();
  }

  public String getNameInNamespace () throws NamingException
  {
    throw new OperationNotSupportedException ();
  }
}
