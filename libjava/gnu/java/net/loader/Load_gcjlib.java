/* Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.java.net.loader;

import gnu.gcj.runtime.SharedLibHelper;
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLConnection;
import java.net.URLStreamHandlerFactory;

/**
 * A <code>Load_gcjlib</code> is a type of <code>URLLoader</code>
 * that loads classes and resources from a shared library.
 */
public final class Load_gcjlib extends URLLoader
{
  private SharedLibHelper helper;

  public Load_gcjlib(URLClassLoader classloader,
		     URLStreamHandlerCache cache,
		     URLStreamHandlerFactory factory,
		     URL url, URL absoluteUrl)
  {
    super(classloader, cache, factory, url, absoluteUrl);
    helper = SharedLibHelper.findHelper(classloader, url.getFile(),
					noCertCodeSource, true);
  }

  public Class getClass(String className)
  {
    return helper.findClass(className);
  }

  public Resource getResource(String name)
  {
    URL url = helper.findResource(name);
    if (url == null)
      return null;
    return new SoResource(this, url);
  }

  final static class SoResource extends Resource
  {
    private final URL url;

    SoResource(Load_gcjlib loader, URL url)
    {
      super(loader);
      this.url = url;
    }

    public InputStream getInputStream() throws IOException
    {
      URLConnection conn = url.openConnection();
      return conn.getInputStream();
    }

    public int getLength()
    {
      // FIXME we could find this by asking the core object.
      return -1;
    }

    public URL getURL ()
    {
      return url;
    }
  }
}
