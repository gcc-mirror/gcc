/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.event;
import javax.naming.*;
import javax.naming.directory.*;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public interface EventDirContext extends EventContext, DirContext
{
  public void addNamingListener(Name target, String filter, SearchControls ctls,
  				NamingListener l) throws NamingException;
  public void addNamingListener(String target, String filter,
  				SearchControls ctls, NamingListener l)
				throws NamingException;
  public void addNamingListener(Name target, String filter, Object[] filterArgs,
  				SearchControls ctls, NamingListener l)
				throws NamingException;
  public void addNamingListener(String target, String filter,
  				Object[] filterArgs, SearchControls ctls,
				NamingListener l) throws NamingException;
}
