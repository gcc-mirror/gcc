// Handler.java - URLStreamHandler for http protocol.

/* Copyright (C) 1999, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.java.net.protocol.http;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Minimal functionality.
 */

/**
 * @author Warren Levy
 * @author Anthony Green <green@redhat.com>
 * @date March 26, 1999.
 */
public class Handler extends URLStreamHandler
{
  protected URLConnection openConnection (URL url) throws IOException
  {
    return new Connection (url);
  }

  protected int getDefaultPort()
  {
    return 80;
  }
}
