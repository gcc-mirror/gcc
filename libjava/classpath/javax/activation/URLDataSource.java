/* URLDataSource.java -- Data source for a URL object.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package javax.activation;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;

/**
 * DataSource implementation that retrieves its data from a URL.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public class URLDataSource
    implements DataSource
{

  private final URL url;
  private URLConnection connection;

  /**
   * Constructor.
   * This will not open the connection to the URL.
   */
  public URLDataSource(URL url)
  {
    this.url = url;
  }

  /**
   * Returns the Content-Type header for the URL.
   * In the case of failure or lack of such a header,
   * returns "application/octet-stream".
   */
  public String getContentType()
  {
    try
      {
        if (connection == null)
          {
            connection = url.openConnection();
          }
      }
    catch (IOException e)
      {
      }
    String contentType = null;
    if (connection != null)
      {
        contentType = connection.getContentType();
      }
    if (contentType == null)
      {
        contentType = "application/octet-stream";
      }
    return contentType;
  }

  /**
   * Returns the result of <code>getFile</code> of the underlying URL.
   */
  public String getName()
  {
    return url.getFile();
  }

  public InputStream getInputStream()
    throws IOException
  {
    connection = url.openConnection();
    if (connection != null)
      {
        connection.setDoInput(true);
        return connection.getInputStream();
      }
    return null;
  }

  public OutputStream getOutputStream()
    throws IOException
  {
    connection = url.openConnection();
    if (connection != null)
      {
        connection.setDoOutput(true);
        return connection.getOutputStream();
      }
    return null;
  }

  /**
   * Returns the underlying URL.
   */
  public URL getURL()
  {
    return url;
  }

}
