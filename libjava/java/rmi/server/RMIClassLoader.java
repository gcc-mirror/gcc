/* RMIClassLoader.java
  Copyright (c) 1996, 1997, 1998, 1999, 2002 Free Software Foundation, Inc.

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

package java.rmi.server;

import java.net.URL;
import java.net.URLConnection;
import java.net.URLClassLoader;
import java.io.IOException;
import java.io.DataInputStream;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.WeakHashMap;

public class RMIClassLoader
{

  static private class MyClassLoader extends URLClassLoader
  {

    private MyClassLoader(URL[] urls, ClassLoader parent, String annotation)
    {
      super(urls, parent);
      this.annotation = annotation;
    }

    private MyClassLoader(URL[] urls, ClassLoader parent)
    {
      super (urls, parent);
      this.annotation = urlToAnnotation(urls);
    }

    public static String urlToAnnotation(URL[] urls)
    {
      if (urls.length == 0)
	return null;

      StringBuffer annotation = new StringBuffer(64*urls.length);
      for(int i = 0; i < urls.length; i++)
	{
	  annotation.append(urls[i].toExternalForm());
	  annotation.append(' ');
	}

      return annotation.toString();
    }

    public final String getClassAnnotation(){
      return annotation;
    }

    private final String annotation;

  }

  private static Map cacheLoaders; //map annotations to loaders
  private static Map cacheAnnotations; //map loaders to annotations

  //defaultAnnotation is got from system property
  // "java.rmi.server.defaultAnnotation"
  private static String defaultAnnotation;
  //URL object for defaultAnnotation
  private static URL defaultCodebase;
  //class loader for defaultAnnotation
  private static MyClassLoader defaultLoader;
  
  static
  {
    // 89 is a nice prime number for Hashtable initial capacity
    cacheLoaders = new Hashtable(89);
    cacheAnnotations = new Hashtable(89);
    
    defaultAnnotation = System.getProperty("java.rmi.server.defaultAnnotation");
    try 
      {
	if (defaultAnnotation != null)
	  defaultCodebase = new URL(defaultAnnotation);
      }
    catch(Exception _)
      {
	defaultCodebase = null;
      }
    if (defaultCodebase != null)
      {
        defaultLoader = new MyClassLoader(new URL[]{ defaultCodebase },
					  null, defaultAnnotation);
        cacheLoaders.put(defaultAnnotation, defaultLoader);
      }
  }
  
  /**
   * @deprecated
   */
  public static Class loadClass(String name)
    throws MalformedURLException, ClassNotFoundException
  {
    return (loadClass("", name));
  }

  public static Class loadClass(String codebases, String name) 
    throws MalformedURLException, ClassNotFoundException 
  {
    Class c = null;
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    //try context class loader first
    try 
      {
	    c = loader.loadClass(name);       
      }
    catch(ClassNotFoundException e) {}

    if (c != null)
      return c;

    if (codebases.length() == 0) //==""
      loader = defaultLoader;
    else 
      {
	loader = (ClassLoader)cacheLoaders.get(codebases);
	if (loader == null)
	  {
	    //create an entry in cacheLoaders mapping a loader to codebases.
            
	    // codebases are separated by " "
	    StringTokenizer tok = new StringTokenizer(codebases, " "); 
	    ArrayList urls = new ArrayList();
	    while (tok.hasMoreTokens())
	      urls.add(new URL(tok.nextToken()));
  
	    loader = new MyClassLoader((URL[])urls.toArray(new URL[urls.size()]),
					null, codebases);
	    cacheLoaders.put(codebases, loader);
	  }
      }

    return loader.loadClass(name);
  }
  
  public static String getClassAnnotation(Class cl)
  {
    ClassLoader loader = cl.getClassLoader();
    if (loader == null || loader == ClassLoader.getSystemClassLoader())
      {
	return null; //??
      }
	
    if (loader instanceof MyClassLoader)
      {
	return ((MyClassLoader)loader).getClassAnnotation();
      }
	
    String s = (String)cacheAnnotations.get(loader);
    if (s != null)
      return s;
	    
    if (loader instanceof URLClassLoader)
      {
	URL[] urls = ((URLClassLoader)loader).getURLs();
	if(urls.length == 0)
	  return null;

	StringBuffer annotation = new StringBuffer(64*urls.length);
	for(int i = 0; i < urls.length; i++)
	  {
	    annotation.append(urls[i].toExternalForm());
	    annotation.append(' ');
	  }
	s = annotation.toString();
	cacheAnnotations.put(loader, s);
      }
    return null;
  }
  
  /**
   * @deprecated
   */
  public static Object getSecurityContext(ClassLoader loader)
  {
    throw new Error("Not implemented");
  }

}
