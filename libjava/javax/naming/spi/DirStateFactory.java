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

public interface DirStateFactory extends StateFactory
{
  // Inner class

  public static class Result
  {
    private Object obj;
    private Attributes outAttrs;

    public Result(Object obj, Attributes outAttrs)
    {
      this.obj = obj;
      this.outAttrs = outAttrs;
    }

    public Object getObject()
    {
      return obj;
    }

    public Attributes getAttributes()
    {
      return outAttrs;
    }
  }

  public DirStateFactory.Result getStateToBind(Object obj, Name name,
  					       Context nameCtx,
					       Hashtable environment,
					       Attributes inAttrs)
					       throws NamingException;
}
