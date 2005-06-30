/* RMIClassLoader.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2003, 2004
   Free Software Foundation, Inc.

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

package java.rmi.server;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Map;
import java.util.StringTokenizer;


/**
 * This class provides a set of public static utility methods for supporting
 * network-based class loading in RMI. These methods are called by RMI's
 * internal marshal streams to implement the dynamic class loading of types for
 * RMI parameters and return values.
 */
public class RMIClassLoader
{
  /**
   * This class isn't intended to be instantiated.
   */
  private RMIClassLoader() {}

  private static class MyClassLoader extends URLClassLoader
  {
    // Package-private to avoid a trampoline constructor.
    MyClassLoader (URL[] urls, ClassLoader parent, String annotation)
    {
      super (urls, parent);
      this.annotation = annotation;
    }

    private MyClassLoader (URL[] urls, ClassLoader parent)
    {
      super (urls, parent);
      this.annotation = urlToAnnotation (urls);
    }

    public static String urlToAnnotation (URL[] urls)
    {
      if (urls.length == 0)
        return null;

      StringBuffer annotation = new StringBuffer (64 * urls.length);

      for (int i = 0; i < urls.length; i++)
      {
        annotation.append (urls [i].toExternalForm());
        annotation.append (' ');
      }

      return annotation.toString();
    }

    public final String getClassAnnotation()
    {
      return annotation;
    }

    private final String annotation;
  }
  
  /** 
   * This class is used to identify a cached classloader by its codebase and 
   * the context classloader that is its parent.
   */  
  private static class CacheKey
  {
     private String mCodeBase;
     private ClassLoader mContextClassLoader;
  	
     public CacheKey (String theCodebase, ClassLoader theContextClassLoader)
     {
       mCodeBase = theCodebase;
       mContextClassLoader = theContextClassLoader;
     }
  	
    /**
     * @return true if the codebase and the context classloader are equal
     */
    public boolean equals (Object theOther)
    {
      if (theOther instanceof CacheKey)
      {
      	CacheKey key = (CacheKey) theOther;
	
      	return (equals (this.mCodeBase,key.mCodeBase)
                && equals (this.mContextClassLoader, key.mContextClassLoader));
        }
      return false;
    }
    
    /**
     * Test if the two objects are equal or both null.
     * @param theOne
     * @param theOther
     * @return
     */
    private boolean equals (Object theOne, Object theOther)
    {
      return theOne != null ? theOne.equals (theOther) : theOther == null;
    }

    /**
     * @return hashCode  
     */
    public int hashCode()
    {
      return ((mCodeBase != null           ? mCodeBase.hashCode()           :  0) 
              ^(mContextClassLoader != null ? mContextClassLoader.hashCode() : -1));
    }

    public String toString()
    {
      return "[" + mCodeBase + "," + mContextClassLoader + "]"; 
    }

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
    cacheLoaders = new Hashtable (89);
    cacheAnnotations = new Hashtable (89);

    defaultAnnotation = System.getProperty ("java.rmi.server.defaultAnnotation");

    try
      {
        if (defaultAnnotation != null)
          defaultCodebase = new URL (defaultAnnotation);
      }
    catch (Exception _)
      {
        defaultCodebase = null;
      }

    if (defaultCodebase != null)
      {
        defaultLoader = new MyClassLoader (new URL[] { defaultCodebase }, null,
                                           defaultAnnotation);
        cacheLoaders.put (new CacheKey (defaultAnnotation,
                                        Thread.currentThread().getContextClassLoader()),
                                        defaultLoader);
      }
    }

  /**
   * @deprecated
   */
  public static Class loadClass (String name)
    throws MalformedURLException, ClassNotFoundException
  {
    return loadClass ("", name);
  }

  public static Class loadClass (String codebases, String name)
    throws MalformedURLException, ClassNotFoundException
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();

    //try context class loader first
    try 
      {
        return loader.loadClass (name);
      }
    catch (ClassNotFoundException e)
      {
        // class not found in the local classpath
      }

    if (codebases.length() == 0) //==""
      {
        loader = defaultLoader;
      }
    else
      {
        loader = getClassLoader(codebases);
      }

    if (loader == null)
      {
        //do not throw NullPointerException
        throw new ClassNotFoundException ("Could not find class (" + name +
                                          ") at codebase (" + codebases + ")");
      }
      
    return loader.loadClass (name);
  }

  /**
   * Gets a classloader for the given codebase and with the current
   * context classloader as parent.
   * 
   * @param codebases
   * 
   * @return a classloader for the given codebase
   * 
   * @throws MalformedURLException if the codebase contains a malformed URL
   */
  public static ClassLoader getClassLoader (String codebases) 
    throws MalformedURLException
  {
    ClassLoader loader;
    CacheKey loaderKey = new CacheKey
      (codebases, Thread.currentThread().getContextClassLoader());
    loader = (ClassLoader) cacheLoaders.get (loaderKey);
      
    if (loader == null)
      {
        //create an entry in cacheLoaders mapping a loader to codebases.
        // codebases are separated by " "
        StringTokenizer tok = new StringTokenizer (codebases, " ");
        ArrayList urls = new ArrayList();
      
        while (tok.hasMoreTokens())
          urls.add (new URL (tok.nextToken()));
      
        loader = new MyClassLoader ((URL[]) urls.toArray (new URL [urls.size()]),
                                    Thread.currentThread().getContextClassLoader(),
                                    codebases);
        cacheLoaders.put (loaderKey, loader);
      }
           
    return loader;
  }
 
  /**
   * Returns a string representation of the network location where a remote
   * endpoint can get the class-definition of the given class.
   *
   * @param cl
   *
   * @return a space seperated list of URLs where the class-definition
   * of cl may be found
   */
  public static String getClassAnnotation (Class cl)
  {
    ClassLoader loader = cl.getClassLoader();
    
    if (loader == null
        || loader == ClassLoader.getSystemClassLoader())
      {
        return System.getProperty ("java.rmi.server.codebase");
      }

    if (loader instanceof MyClassLoader)
      {
        return ((MyClassLoader) loader).getClassAnnotation();
      }

    String s = (String) cacheAnnotations.get (loader);

    if (s != null)
      return s;

    if (loader instanceof URLClassLoader)
      {
        URL[] urls = ((URLClassLoader) loader).getURLs();

        if (urls.length == 0)
          return null;

        StringBuffer annotation = new StringBuffer (64 * urls.length);

        for (int i = 0; i < urls.length; i++)
          {
            annotation.append (urls [i].toExternalForm());
            annotation.append (' ');
          }

        s = annotation.toString();
        cacheAnnotations.put (loader, s);
        return s;
      }

    return System.getProperty ("java.rmi.server.codebase");
  }

  /**
   * @deprecated
   */
  public static Object getSecurityContext (ClassLoader loader)
  {
    throw new Error ("Not implemented");
  }
}
