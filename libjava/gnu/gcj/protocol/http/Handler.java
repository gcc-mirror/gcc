// Handler.java - URLStreamHandler for http protocol.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.http;

import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.io.IOException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 26, 1999.
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
    return new Connection(url);
  }
}
