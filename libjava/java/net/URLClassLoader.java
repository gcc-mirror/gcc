/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.*;
import java.util.jar.*;
import java.util.Enumeration;
import java.util.Vector;

public class URLClassLoader extends ClassLoader 
{
  // The URLStreamHandlerFactory
  URLStreamHandlerFactory factory = null;

  // `path' contains simply the URL's we're using for the searching.
  private Vector path; 

  // If path[n] is a zip/jar, then this holds a JarURLConnection for
  // that thing, otherwise, path[n] is null.
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

  // A File URL may actually be a Jar URL.  Convert if possible.
  private URL jarFileize (URL url)
  {
    if (! url.getProtocol ().equals ("jar"))
      {
	String f = url.getFile ();

	// If it ends with '/' we'll take it for a directory,
	// otherwise it's a jar file.  This is how JDK 1.2 defines
	// it, so we will not try to be smart here.
	if (f.charAt (f.length ()-1) != '/')
	  {
	    try
	      {
		url = new URL ("jar", "", -1, (url.toExternalForm ())+"!/", 
			       getHandler0 ("jar"));
	      } 
	    catch (MalformedURLException x)
	      {
		/* ignore */
	      }
	  }
      }
    return url;
  }

  protected void addURL (URL url)
  {
    JarURLConnection conn = null;
    
    // Convert a Jar File URL into Jar URL if possible.
    url = jarFileize (url);

    path.addElement (url);

    if (url.getProtocol ().equals ("jar"))
      {
	try
	  {
	    conn = (JarURLConnection) url.openConnection ();
	  }
	catch (java.io.IOException x)
	  {
	    /* ignore */
	  }
      }

    info.addElement (conn);
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
	// Convert a Jar File URL into a Jar URL is possible. 
	URL u = jarFileize(urls[i]);

	path.addElement (u);

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
	    info.addElement (conn);
	  }
	else
	  {
	    info.addElement (null);
	  }
      }
  }

  public URL[] getURLs ()
  {
    URL[] urls = new URL[path.size()];
    path.copyInto (urls);
    return urls;
  }
  
  public Enumeration findResources (String name)
  {
    Vector results = new Vector ();

    for (int i = 0; i < path.size(); i++)
      {
	URL u = (URL)path.elementAt (i);
		
	try {
	  JarURLConnection conn = (JarURLConnection) info.elementAt (i);
	  
	  if (conn != null)
	    {
	      if (conn.getJarFile().getJarEntry (name) != null)
		results.addElement (new URL(u, name, getHandler0 (u.getProtocol())));
	    }
	  else
	    {
	      URL p = new URL (u, name, getHandler0 (u.getProtocol()));
			    
	      InputStream is = p.openStream();
	      if (is != null)
		{
		  is.close();
		  results.addElement (p);
		}
	    }
		    
	  // if we get an exception ... try the next path element
	} catch (IOException x) {
	  continue;
	}
      }
	
    return results.elements ();
  }

  public URL findResource (String name)
  {
    for (int i = 0; i < path.size(); i++)
      {
	URL u = (URL)path.elementAt (i);
	
	try {
	  JarURLConnection conn = (JarURLConnection) info.elementAt (i);
 	  
	  if (conn != null)
	    {
	      if (conn.getJarFile().getJarEntry (name) != null)
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

  // and finally, we can implement our class loader functionality.
  protected Class findClass (String name)
    throws ClassNotFoundException
  {
    if (name == null)
      throw new ClassNotFoundException ("null");

    try 
      {
	URL u = getResource (name.replace ('.', '/') + ".class");

	if (u == null)
	  throw new ClassNotFoundException (name);

	URLConnection connection = u.openConnection ();
	InputStream is = connection.getInputStream ();

	int len = connection.getContentLength ();
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

