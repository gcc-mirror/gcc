/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.*;
import java.util.jar.*;
import java.util.Vector;

public class URLClassLoader extends ClassLoader 
{
  // The URLStreamHandlerFactory
  URLStreamHandlerFactory factory = null;

  // `path' contains simply the URL's we're using for the searching.
  private Vector path; 

  // If path[n] is a zip/jar, then this holds a JarURLConnection for that thing,
  // otherwise, path[n] is null.
  private Vector info; 

  private URLStreamHandler getHandler0 (String protocol)
  {
    if (factory != null)
      return factory.createURLStreamHandler(protocol);
    else
      return null;
  }

  public URLClassLoader (URL[] urls)
  { 
    this (urls, null, null);
  }
  
  public URLClassLoader (URL[] urls, ClassLoader parent)
  { 
    this (urls, parent, null);
  }

  public URLClassLoader (URL[] urls, ClassLoader parent,
			 URLStreamHandlerFactory fac)
  { 
    super (parent);

    factory = fac;

    if (urls == null || urls.length == 0)
      {
	path = new Vector (1);
	info = new Vector (1);
	return;
      }

    path = new Vector (urls.length);
    info = new Vector (urls.length);

    for (int i = 0; i < urls.length; i++)
      {
	URL u = urls[i];

	// If it is a jar url, then we'll search it as is.  
	if (! u.getProtocol ().equals ("jar"))
	  {
	    String f = u.getFile ();

	    // If it ends with '/' we'll take it for a directory,
	    // otherwise it's a jar file.  This is how JDK 1.2 defines
	    // it, so we will not try to be smart here.
	    if (f.charAt (f.length ()-1) != '/')
	      {
		try
		  {
		    u = new URL ("jar", "", -1, (u.toExternalForm ())+"!/", 
				 getHandler0 ("jar"));
		  } 
		catch (MalformedURLException x)
		  {
		    /* ignore */
		  }
	      }
	  }

	path.insertElementAt (u, i);

	if (u.getProtocol ().equals ("jar"))
	  {
	    JarURLConnection conn = null;
	    try
	      {
		conn = (JarURLConnection) u.openConnection ();
	      }
	    catch (java.io.IOException x)
	      {
		/* ignore */
	      }
	    info.insertElementAt (conn, i);
	  }
	else
	  {
	    info.insertElementAt (null, i);
	  }
      }
  }
  
  public URL getResource (String name)
  {
    for (int i = 0; i < path.size(); i++)
      {
	URL u    = (URL)path.elementAt (i);
	
	try {
	  JarURLConnection conn = (JarURLConnection) info.elementAt (i);
	  
	  if (conn != null)
	    {
	      if (conn.getJarEntry (name) != null)
		return new URL(u, name, getHandler0 (u.getProtocol()));
	    }
	  else
	    {
	      URL p = new URL (u, name, getHandler0 (u.getProtocol()));

	      InputStream is = p.openStream();
	      if (is != null)
		{
		  is.close();
		  return p;
		}
	    }
	
	  // if we get an exception ... try the next path element
	} catch (IOException x) {
	  continue;
	}
      }

    return null;
  }

  /** IN jdk 1.2 this method is not overridden, but we gain performance
      by doing so.
   */

  public InputStream getResourceAsStream (String name)
  {
    for (int i = 0; i < path.size(); i++)
      {
	URL u    = (URL)path.elementAt (i);
	
	try {
	  JarURLConnection conn = (JarURLConnection) info.elementAt (i);
	  
	  if (conn != null)
	    {
	      JarFile file = conn.getJarFile ();
	      JarEntry ent = file.getJarEntry (name);
	      if (ent != null)
		return file.getInputStream(ent);
	    }
	  else
	    {
	      InputStream is = new URL(u, name, getHandler0 (u.getProtocol())).openStream();
	      if (is != null)
		return is;
	    }
	
	  // if we get an exception ... try the next path element
	} catch (IOException x) {
	  continue;
	}
      }

    return null;
  }

  // and finally, we can implement our class loader functionality.
  protected Class findClass (String name)
    throws ClassNotFoundException
  {
    if (name == null)
      throw new ClassNotFoundException ("null");

    try 
      {
	InputStream is = getResourceAsStream (name.replace ('.', '/') + ".class");
	
	if (is == null)
	  throw new ClassNotFoundException (name);
	
	// Here we have to rely on available() to provide the length of
	// the class; which might not be exactly right in some cases...
	
	int len = is.available ();
	byte[] data = new byte[len];

	int left = len;
	int off  = 0;
	while (left > 0)
	  {
	    int c = is.read (data, off, len-off);
	    if (c == -1 || c == 0)
	      throw new InternalError ("premature end of file");
	    left -= c;
	    off += c;
	  }

	return defineClass (name, data, 0, len);
      } 
    catch (java.io.IOException x)
      {
	throw new ClassNotFoundException(name);
      }
  }

}

