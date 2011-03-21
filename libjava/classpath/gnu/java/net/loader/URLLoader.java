/* URLLoader.java --  base helper class for URLClassLoader
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


package gnu.java.net.loader;

import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLStreamHandlerFactory;
import java.security.CodeSource;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.jar.Manifest;

/**
 * A <code>URLLoader</code> contains all logic to load resources from a
 * given base <code>URL</code>.
 */
public abstract class URLLoader
{
  /**
   * Our classloader to get info from if needed.
   */
  final URLClassLoader classloader;

  /**
   * The base URL from which all resources are loaded.
   */
  final URL baseURL;

  /**
   * The stream handler factory.
   */
  final URLStreamHandlerFactory factory;

  /**
   * The source for stream handlers.
   */
  final URLStreamHandlerCache cache;

  /**
   * A <code>CodeSource</code> without any associated certificates.
   * It is common for classes to not have certificates associated
   * with them.  If they come from the same <code>URLLoader</code>
   * then it is safe to share the associated <code>CodeSource</code>
   * between them since <code>CodeSource</code> is immutable.
   */
  final CodeSource noCertCodeSource;

  public URLLoader(URLClassLoader classloader, URLStreamHandlerCache cache,
                   URLStreamHandlerFactory factory,
                   URL baseURL)
  {
    this(classloader, cache, factory, baseURL, baseURL);
  }

  public URLLoader(URLClassLoader classloader, URLStreamHandlerCache cache,
                   URLStreamHandlerFactory factory,
                   URL baseURL, URL overrideURL)
  {
    this.classloader = classloader;
    this.baseURL = baseURL;
    this.factory = factory;
    this.cache = cache;
    this.noCertCodeSource = new CodeSource(overrideURL, (Certificate[]) null);
  }

  /**
   * Return the base URL of this loader.
   */
  public final URL getBaseURL()
  {
    return baseURL;
  }

  /**
   * Returns a <code>Class</code> loaded by this
   * <code>URLLoader</code>, or <code>null</code> when this loader
   * either can't load the class or doesn't know how to load classes
   * at all.  Most subclasses do not need to override this; it is only
   * useful in situations where the subclass has a more direct way of
   * making <code>Class</code> objects.
   */
  public Class getClass(String className)
  {
    return null;
  }

  /**
   * Returns a <code>Resource</code> loaded by this
   * <code>URLLoader</code>, or <code>null</code> when no
   * <code>Resource</code> with the given name exists.
   */
  public abstract Resource getResource(String s);

  /**
   * Returns the <code>Manifest</code> associated with the
   * <code>Resource</code>s loaded by this <code>URLLoader</code> or
   * <code>null</code> there is no such <code>Manifest</code>.
   */
  public Manifest getManifest()
  {
    return null;
  }

  /**
   * Return a list of new URLLoader objects representing any
   * class path entries added by this container.
   */
  public ArrayList<URLLoader> getClassPath()
  {
    return null;
  }
}
