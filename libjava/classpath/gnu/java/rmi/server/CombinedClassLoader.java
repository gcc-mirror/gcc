/* CombinedClassLoader.java -- Multiple class loader support for proxy.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.rmi.server;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.ArrayList;

/**
 * This class supports the multiple class loaders to load the resources. It is
 * used for constructing proxy classes that implement interfaces, loaded by
 * the several different class loaders.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class CombinedClassLoader extends ClassLoader
{
  /**
   * The class loader array.
   */
  ClassLoader[] loaders;

  /**
   * Create a new combined class loader that uses the given collection of
   * loaders to load the classes and resources. The loader order is equal to
   * the order, returned by the collection interator. The duplicate loaders
   * are discarded and the system class loader is added as the last loader.
   *
   * @param a_loaders the loadery collection (may contain duplicate instances
   * that will be discarded.
   */
  public CombinedClassLoader(Collection a_loaders)
  {
    ArrayList sLoaders = new ArrayList(a_loaders.size());

    Iterator iter = a_loaders.iterator();
    Object cl;
    while (iter.hasNext())
      {
        cl = iter.next();
        if (cl!=null && !sLoaders.contains(cl))
          sLoaders.add(cl);
      }

    loaders = new ClassLoader[sLoaders.size()];

    for (int i = 0; i < loaders.length; i++)
      loaders[i] = (ClassLoader) sLoaders.get(i);
  }

  /**
   * Find the class with the given name.
   */
  protected Class findClass(String name) throws ClassNotFoundException
  {
    for (int i = 0; i < loaders.length; i++)
      {
        try
          {
            return loaders[i].loadClass(name);
          }
        catch (ClassNotFoundException e)
          {
            // try another.
          }
      }
    return super.findClass(name);
  }

  /**
   * Find resource with the given name.
   */
  protected URL findResource(String name)
  {
    for (int i = 0; i < loaders.length; i++)
      {
        URL resource = loaders[i].getResource(name);
        if (resource != null)
          return resource;
      }
    return super.findResource(name);
  }

  /**
   * Find resources with the given name.
   */
  protected Enumeration findResources(String name) throws IOException
  {
    for (int i = 0; i < loaders.length; i++)
      {
        Enumeration resource = loaders[i].getResources(name);
        if (resource != null)
          return resource;
      }
    return super.findResources(name);  }
}
