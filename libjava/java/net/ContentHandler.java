// ContentHandler.java - Superclass of classes that read from a URLConnection.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.IOException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 5, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public abstract class ContentHandler
{
  public abstract Object getContent(URLConnection urlc) throws IOException;
}
