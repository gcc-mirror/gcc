/* DirectoryManager.java --
   Copyright (C) 2000, 2001, 2004, 2005  Free Software Foundation, Inc.

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

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;

import javax.naming.CannotProceedException;
import javax.naming.Context;
import javax.naming.Name;
import javax.naming.NamingException;
import javax.naming.RefAddr;
import javax.naming.Reference;
import javax.naming.Referenceable;
import javax.naming.StringRefAddr;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;

/**
 * @author Tom Tromey (tromey@redhat.com)
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
