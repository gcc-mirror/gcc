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
import java.util.StringTokenizer;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.ArrayList;

public class RMIClassLoader
{

  static private class MyClassLoader extends URLClassLoader
  {
    private MyClassLoader(URL[] urls, ClassLoader parent)
    {
      super (urls, parent);
    }

    Class defineClass(String name, byte[] data)
    {
      return defineClass(name, data, 0, data.length);
    }
  }

  private static Map cacheLoaders; //map annotations to loaders
  private static Map cacheClasses; //map loader to classes that the loader loaded+
  private static String defaultAnnotation;
  private static URL defaultCodebase;
  private static MyClassLoader defaultLoader;
  
  static
  {
    cacheLoaders = Collections.synchronizedMap(new WeakHashMap(5)); 
    cacheClasses = Collections.synchronizedMap(new WeakHashMap(5));
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
					  Thread.currentThread().getContextClassLoader());
        cacheLoaders.put(defaultAnnotation, defaultLoader);
        cacheClasses.put(defaultLoader, Collections.synchronizedMap(new WeakHashMap())); 
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

  public static Class loadClass(URL codebase, String name) 
    throws MalformedURLException, ClassNotFoundException 
  {
    URL u = new URL(codebase, name + ".class");
    try 
      {
	URLConnection conn = u.openConnection();
	DataInputStream strm = new DataInputStream(conn.getInputStream());
	byte data[] = new byte[conn.getContentLength()];
	strm.readFully(data);
	return (defaultLoader.defineClass(name, data));
      }
    catch (IOException _) 
      {
	throw new ClassNotFoundException(name);
      }
  }
  
  public static Class loadClass(String codebases, String name) 
    throws MalformedURLException, ClassNotFoundException 
  {
    ClassLoader loader = (ClassLoader)cacheLoaders.get(codebases);
    if (loader == null)
      {
	if (codebases != "")
	  {
	    //codebases are separated by " "
	    StringTokenizer tok = new StringTokenizer(codebases, " "); 
	    ArrayList urls = new ArrayList();
	    while (tok.hasMoreTokens())
	      urls.add(new URL(tok.nextToken()));
	    
	    loader = new MyClassLoader((URL[])urls.toArray(new URL[urls.size()]),
				       Thread.currentThread().getContextClassLoader());
	    cacheLoaders.put(codebases, loader);
	    cacheClasses.put(loader, Collections.synchronizedMap(new WeakHashMap())); 
	  }
	else
	  {
	    //if codebases is empty, construct a classloader 
	    // based on current context classloader,
	    // and we won't cache classloader for empty codebases
	    loader = new MyClassLoader(new URL[]{ defaultCodebase },
				       Thread.currentThread().getContextClassLoader());
	  }
      }

    Class c = null;
    Map classes = (Map)cacheClasses.get(loader);
    if (classes != null)
      {
        c = (Class)classes.get(name);
        if (c == null)
	  {
            c = loader.loadClass(name);
            classes.put(name, c); 
	  }
      }else
        c = loader.loadClass(name);
    
    return c;
  }
  
  public static String getClassAnnotation(Class cl)
  {
    ClassLoader loader = cl.getClassLoader();
    if (loader == null)
      {
	if (defaultCodebase != null)
	  return defaultCodebase.toExternalForm();
	else
	  return null;
      }
    if (loader instanceof URLClassLoader)
      {
	URL[] urls = ((URLClassLoader)loader).getURLs();
	if(urls.length == 0)
	  return null;
	StringBuffer annotation = new StringBuffer(urls[0].toExternalForm());
	for(int i = 1; i < urls.length; i++)
	  {
	    annotation.append(' ');
	    annotation.append(urls[i].toExternalForm());
	  }
	return annotation.toString();
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
