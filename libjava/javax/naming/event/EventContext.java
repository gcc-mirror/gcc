/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.event;
import javax.naming.*;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public interface EventContext extends Context
{
  public static final int OBJECT_SCOPE = 0;
  public static final int ONELEVEL_SCOPE = 1;
  public static final int SUBTREE_SCOPE = 2;

  public void addNamingListener(Name target, int scope, NamingListener l)
    throws NamingException;
  public void addNamingListener(String target, int scope, NamingListener l)
    throws NamingException;
  public void removeNamingListener(NamingListener l) throws NamingException;
  public boolean targetMustExist() throws NamingException;
}
