/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming;

import java.lang.Exception;
import java.util.Hashtable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 14, 2001
 */

public abstract class ReferralException extends NamingException
{
  protected ReferralException ()
  {
    super ();
  }

  protected ReferralException (String msg)
  {
    super (msg);
  }

  public abstract Object getReferralInfo();
  public abstract Context getReferralContext() throws NamingException;
  public abstract Context getReferralContext(Hashtable env)
    throws NamingException;
  public abstract boolean skipReferral();
  public abstract void retryReferral();
}
