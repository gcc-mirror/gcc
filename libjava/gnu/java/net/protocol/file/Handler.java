/* Handler.java -- "file" protocol handler for java.net
   Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004 Free Software Foundation, Inc.

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

package gnu.java.net.protocol.file;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/**
 * This is the protocol handler for the "file" protocol.
 * It implements the abstract openConnection() method from
 * URLStreamHandler by returning a new FileURLConnection object (from
 * this package).  All other methods are inherited
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy <warrenl@cygnus.com>
 */
public class Handler extends URLStreamHandler
{
  /**
   * A do nothing constructor
   */
  public Handler()
  {
  }

  /**
   * This method returs a new FileURLConnection for the specified URL
   *
   * @param url The URL to return a connection for
   *
   * @return The URLConnection
   *
   * @exception IOException If an error occurs
   */
  protected URLConnection openConnection(URL url) throws IOException
  {
    // If a hostname is set, then we need to switch protocols to ftp
    // in order to transfer this from the remote host.
    String host = url.getHost();
    if ((host != null) && (! host.equals("")))
      {
	// Reset the protocol (and implicitly the handler) for this URL.
	// Then have the URL attempt the connection again, as it will
	// get the changed handler the next time around.
	// If the ftp protocol handler is not installed, an 
	// exception will be thrown from the new openConnection() call.
	setURL (url, "ftp", url.getHost(), url.getPort(), url.getFile(),
	        url.getRef());
	return url.openConnection();
      }

    return new Connection(url);
  }
} // class Handler
