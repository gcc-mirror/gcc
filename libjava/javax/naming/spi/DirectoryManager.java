/* Copyright (C) 2000, 2001 Free Software Foundation
   
   This file is part of libgcj.
   
   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package javax.naming.spi;

import javax.naming.*;
import javax.naming.directory.*;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Enumeration;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date June 25, 2001
 */
public class DirectoryManager extends NamingManager
{
  // Can't instantiate this class.
  DirectoryManager ()
  {
  }

  public static DirContext getContinuationDirContext (CannotProceedException c)
    throws NamingException
  {
    return (DirContext) getContinuationContext (c);
  }

  // Try to create an object using the factory.  Return null on
  // failure.
  private static Object tryCreateObject (ObjectFactory factory,
					 Object refInfo,
					 Name name,
					 Context nameCtx,
					 Hashtable environment,
					 Attributes attrs)
    throws Exception
  {
    if (factory instanceof DirObjectFactory)
      {
	DirObjectFactory dof = (DirObjectFactory) factory;
	return dof.getObjectInstance (refInfo, name, nameCtx,
				      environment, attrs);
      }
    else
      return factory.getObjectInstance (refInfo, name, nameCtx,
					environment);
  }

  public static Object getObjectInstance (Object refInfo, Name name,
					  Context nameCtx,
					  Hashtable environment,
					  Attributes attrs)
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
		Class k = Class.forName (fClass);
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
			Object obj = tryCreateObject (factory,
						      refInfo,
						      name,
						      nameCtx,
						      environment,
						      attrs);
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
		Class k = Class.forName (klassName);
		factory = (ObjectFactory) k.newInstance ();
		Object obj = tryCreateObject (factory, refInfo, name,
					      nameCtx, environment, attrs);
		if (obj != null)
		  return obj;
	      }

	    // Failure.
	    return refInfo;
	  }
      }

    if (factory == null)
      return refInfo;
    Object obj = tryCreateObject (factory, refInfo, name,
				  nameCtx, environment, attrs);
    return obj == null ? refInfo : obj;
  }

  public static DirStateFactory.Result getStateToBind (Object obj,
						       Name name,
						       Context nameCtx,
						       Hashtable environment,
						       Attributes attrs)
    throws NamingException
  {
    StringTokenizer tokens = getPlusPath (Context.STATE_FACTORIES,
					  environment, nameCtx);
    while (tokens.hasMoreTokens ())
      {
	String klassName = tokens.nextToken ();
	try
	  {
	    Class k = Class.forName (klassName);
	    StateFactory factory = (StateFactory) k.newInstance ();

	    DirStateFactory.Result result = null;
	    if (factory instanceof DirStateFactory)
	      {
		DirStateFactory dsf = (DirStateFactory) factory;
		result = dsf.getStateToBind (obj, name, nameCtx, environment,
					     attrs);
	      }
	    else
	      {
		Object o = factory.getStateToBind (obj, name, nameCtx,
						   environment);
		if (o != null)
		  result = new DirStateFactory.Result (o, attrs);
	      }
	    if (result != null)
	      return result;
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

    return new DirStateFactory.Result (obj, attrs);
  }
}
