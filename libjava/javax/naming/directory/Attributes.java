/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;

import javax.naming.*;
import java.io.Serializable;

/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 14, 2001
 */

public interface Attributes extends Cloneable, Serializable
{
  public boolean isCaseIgnored();
  public int size();
  public Attribute get(String attrID);
  public NamingEnumeration getAll();
  public NamingEnumeration getIDs();
  public Attribute put(String attrID, Object val);
  public Attribute put(Attribute attr);
  public Attribute remove(String attrID);
  public Object clone();
}

