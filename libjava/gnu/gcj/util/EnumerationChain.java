/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.util;

import java.util.Enumeration;
import java.util.NoSuchElementException;

public class EnumerationChain implements Enumeration
{
  private Enumeration first_;
  private Enumeration second_;

  public EnumerationChain (Enumeration first, Enumeration second)
    {
      if (first == null
          || second == null)
        throw new NullPointerException();

      first_ = first;
      second_ = second;
    }

  public synchronized boolean hasMoreElements()
    {
      if (first_ == null)
        return false;
      else
        return first_.hasMoreElements();
    }

  public synchronized Object nextElement() throws NoSuchElementException
    {
      while (first_ != null)
        {
          if (! first_.hasMoreElements())
            {
              first_ = second_;
              second_ = null;
            }
          else 
            return first_.nextElement();
        }
      
      throw new NoSuchElementException();
    }
}
