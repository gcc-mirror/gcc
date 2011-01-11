/* Resource.java -- a resource for URLLoader
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


import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.CodeSource;
import java.security.cert.Certificate;

/**
 * A <code>Resource</code> represents a resource in some
 * <code>URLLoader</code>. It also contains all information (e.g.,
 * <code>URL</code>, <code>CodeSource</code>, <code>Manifest</code> and
 * <code>InputStream</code>) that is necessary for loading resources
 * and creating classes from a <code>URL</code>.
 */
public abstract class Resource
{
  final URLLoader loader;

  public Resource(URLLoader loader)
  {
    this.loader = loader;
  }

  /**
   * Returns the non-null <code>CodeSource</code> associated with
   * this resource.
   */
  public CodeSource getCodeSource()
  {
    Certificate[] certs = getCertificates();
    if (certs == null)
      return loader.noCertCodeSource;
    else
      return new CodeSource(loader.baseURL, certs);
  }

  /**
   * Returns <code>Certificates</code> associated with this
   * resource, or null when there are none.
   */
  public Certificate[] getCertificates()
  {
    return null;
  }

  /**
   * Return the URLLoader for this resource.
   */
  public final URLLoader getLoader()
  {
    return loader;
  }

  /**
   * Return a <code>URL</code> that can be used to access this resource.
   */
  public abstract URL getURL();

  /**
   * Returns the size of this <code>Resource</code> in bytes or
   * <code>-1</code> when unknown.
   */
  public abstract int getLength();

  /**
   * Returns the non-null <code>InputStream</code> through which
   * this resource can be loaded.
   */
  public abstract InputStream getInputStream() throws IOException;
}
