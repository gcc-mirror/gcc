// Handler.java - URLStreamHandler for file protocol.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.jar;

import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.io.IOException;

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
