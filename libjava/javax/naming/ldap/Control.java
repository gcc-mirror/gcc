/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.ldap;
import java.io.Serializable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public interface Control extends Serializable
{
  public static final boolean CRITICAL = true;
  public static final boolean NONCRITICAL = false;

  public String getID();
  public boolean isCritical();
  public byte[] getEncodedValue();
}
