/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.spi;
import javax.naming.*;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public interface Resolver
{
  public ResolveResult resolveToClass(Name name, Class contextType)
    throws NamingException;
  public ResolveResult resolveToClass(String name, Class contextType)
    throws NamingException;
}
