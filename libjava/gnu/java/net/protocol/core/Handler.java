// Handler.java - URLStreamHandler for core protocol.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.java.net.protocol.core;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/**
 * @author Anthony Green <green@redhat.com>
 * @date August 13, 2001.
 */
public class Handler extends URLStreamHandler
{
  protected URLConnection openConnection(URL url) throws IOException
  {
    return new Connection(url);
  }
}
