/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.ldap;
import javax.naming.*;
import javax.naming.directory.*;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public interface LdapContext extends DirContext
{
  public static final String CONTROL_FACTORIES = "java.naming.factory.control";

  public ExtendedResponse extendedOperation(ExtendedRequest request)
    throws NamingException;
  public LdapContext newInstance(Control[] requestControls)
    throws NamingException;
  public void reconnect(Control[] connCtls) throws NamingException;
  public Control[] getConnectControls() throws NamingException;
  public void setRequestControls(Control[] requestControls)
    throws NamingException;
  public Control[] getRequestControls() throws NamingException;
  public Control[] getResponseControls() throws NamingException;
}
