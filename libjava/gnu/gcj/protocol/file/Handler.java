// Handler.java - URLStreamHandler for file protocol.

/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.file;

import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.io.IOException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date April 13, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Minimal functionality.
 */

public class Handler extends URLStreamHandler
{
  protected URLConnection openConnection(URL url) throws IOException
  {
    // If a hostname is set, then we need to switch protocols to ftp
    // in order to transfer this from the remote host.
    if (! url.getHost().equals(""))
      {
	// Reset the protocol (and implicitly the handler) for this URL.
	// Then have the URL attempt the connection again, as it will
	// get the changed handler the next time around.
	setURL (url, "ftp", url.getHost(), url.getPort(), url.getFile(),
		url.getRef());
	// Until the ftp protocol handler is written, this will cause
	// a NullPointerException.
	return url.openConnection();
      }

    return new Connection(url);
  }
}
