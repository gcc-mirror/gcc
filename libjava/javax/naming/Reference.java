/* Copyright (C) 2000 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming;

import java.io.Serializable;

public class Reference implements Cloneable, Serializable
{
  public Reference (String className, String factory, String factoryLocation)
  {
    this.className = className;
  }

  void add (RefAddr addr)
  {
    throw new Error ("javax.naming.Reference.add not implemented");
  }

  RefAddr get (String addrType)
  {
    throw new Error ("javax.naming.Reference.get not implemented");
  }

  public String getClassName ()
  {
    return className;
  }

  private String className;
}
