/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.ldap;
import javax.naming.*;
import java.util.Hashtable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 1, 2001
 */

public abstract class LdapReferralException extends ReferralException
{
  protected LdapReferralException()
  {
    super();
  }

  protected LdapReferralException(String explanation)
  {
    super(explanation);
  }

  public abstract Context getReferralContext() throws NamingException;
  public abstract Context getReferralContext(Hashtable env)
    throws NamingException;
  public abstract Context getReferralContext(Hashtable env, Control[] reqCtls)
    throws NamingException;
}
