// SocketOptions.java - Interface for get/set socket options.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date May 3, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */

public abstract interface SocketOptions
{
  public static final int TCP_NODELAY = 0x1;
  public static final int SO_BINDADDR = 0xF;
  public static final int SO_REUSEADDR = 0x4;
  public static final int IP_MULTICAST_IF = 0x10;
  public static final int SO_LINGER = 0x80;
  public static final int SO_TIMEOUT = 0x1006;

  // JDK1.2
  public static final int SO_SNDBUF = 0x1001;

  // JDK1.2
  public static final int SO_RCVBUF = 0x1002;

  public void setOption(int optID, Object value) throws SocketException;
  public Object getOption(int optID) throws SocketException;
}
