// Handler.java - URLStreamHandler for file protocol.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.java.net.protocol.jar;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/**
 * @author Kresten Krab Thorup <krab@gnu.org>
 * @date August 13, 1999.
 */
public class Handler extends URLStreamHandler
{
  protected URLConnection openConnection(URL url) throws IOException
  {
    return new Connection(url);
  }
}
