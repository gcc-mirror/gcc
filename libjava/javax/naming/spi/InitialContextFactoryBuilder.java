/* Copyright (C) 2000 Free Software Foundation
   
   This file is part of libgcj.
   
   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package javax.naming.spi;

import java.util.Hashtable;
import javax.naming.NamingException;

public interface InitialContextFactoryBuilder
{
  public InitialContextFactory createInitialContextFactory (Hashtable environment) throws NamingException;
}
