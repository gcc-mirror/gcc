/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.ldap;
import java.io.Serializable;
import javax.naming.*;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public interface ExtendedRequest extends Serializable
{
  public String getID();
  public byte[] getEncodedValue();
  public ExtendedResponse createExtendedResponse(String id,
  						 byte[] berValue, int offset,
						 int length)
						 throws NamingException;
}
