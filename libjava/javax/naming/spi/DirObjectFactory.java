/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.spi;
import javax.naming.*;
import javax.naming.directory.*;
import java.util.Hashtable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public interface DirObjectFactory extends ObjectFactory
{
  public Object getObjectInstance(Object obj, Name name, Context nameCtx,
  				  Hashtable environment, Attributes attrs)
				  throws Exception;
}
