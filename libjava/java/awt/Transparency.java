/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 15, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public interface Transparency
{
  public static final int OPAQUE = 1;
  public static final int BITMASK = 2;
  public static final int TRANSLUCENT = 3;

  public int getTransparency();
}
