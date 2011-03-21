/* RemoteURLLoader.java -- a URLLoader for "remote" objects
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
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLConnection;
import java.net.URLStreamHandlerFactory;

/**
 * Loader for remote directories.
 */
public final class RemoteURLLoader extends URLLoader
{
  private final String protocol;

  public RemoteURLLoader(URLClassLoader classloader,
                         URLStreamHandlerCache cache,
                         URLStreamHandlerFactory factory,
                         URL url)
  {
    super(classloader, cache, factory, url);
    protocol = url.getProtocol();
  }

  /**
   * Get a remote resource.
   * Returns null if no such resource exists.
   */
  public Resource getResource(String name)
  {
    try
      {
        URL url = new URL(baseURL, name, cache.get(factory, protocol));
        URLConnection connection = url.openConnection();

        // Open the connection and check the stream
        // just to be sure it exists.
        int length = connection.getContentLength();
        InputStream stream = connection.getInputStream();

        // We can do some extra checking if it is a http request
        if (connection instanceof HttpURLConnection)
          {
            int response =
              ((HttpURLConnection) connection).getResponseCode();
            if (response / 100 != 2)
              return null;
          }

        if (stream != null)
          return new RemoteResource(this, name, url, stream, length);
        else
          return null;
      }
    catch (IOException ioe)
      {
        return null;
      }
  }
}
