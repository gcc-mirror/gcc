// SocketImplFactory.java - Abstract socket implementation factory.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

/**
  * @author Per Bothner <bothner@cygnus.com>
  * @date January 6, 1999.
  */

/** Written using on-line Java Platform 1.2 API Specification.
  * Status:  Believed complete and correct.
  */

public abstract interface SocketImplFactory
{
  public SocketImpl createSocketImpl ();
}
