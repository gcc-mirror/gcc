/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 16, 2000.
 */
/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */

// JDK1.2
public interface ListIterator extends Iterator
{
  public boolean hasNext();
  public Object next();
  public boolean hasPrevious();
  public Object previous();
  public int nextIndex();
  public int previousIndex();
  public void remove();
  public void set(Object o);
  public void add(Object o);
}
