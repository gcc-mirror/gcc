/* Copyright (C) 2000 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming;

import java.io.Serializable;

public class StringRefAddr extends RefAddr
{
  public StringRefAddr (String addrType, String addr)
  {
    throw new Error ("javax.naming.StringRefAddr not implemented");
  }
  
  public Object getContent ()
  {
    throw new Error ("javax.naming.StringRefAddr.getContent not implemented");
  }
}
