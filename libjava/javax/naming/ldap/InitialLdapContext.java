/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.ldap;

import javax.naming.*;
import javax.naming.directory.InitialDirContext;
import java.util.Hashtable;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date June 21, 2001
 */
public class InitialLdapContext
  extends InitialDirContext
  implements LdapContext
{
  public InitialLdapContext ()
    throws NamingException
  {
    this (null, null);
  }

  public InitialLdapContext (Hashtable environment, Control[] connControls)
    throws NamingException
  {
    super (false);

    if (connControls != null)
      {
	if (environment == null)
	  environment = new Hashtable ();
	else
	  environment = (Hashtable) environment.clone ();
	environment.put ("java.naming.ldap.control.connect",
			 connControls);
      }

    init (environment);
  }

  private LdapContext getDefaultInitLdapCtx ()
    throws NamingException
  {
    Context c = getDefaultInitCtx ();
    if (c == null)
      throw new NoInitialContextException ();
    else if (! (c instanceof LdapContext))
      throw new NotContextException ();
    return (LdapContext) c;
  }

  public ExtendedResponse extendedOperation (ExtendedRequest request)
    throws NamingException
  {
    return getDefaultInitLdapCtx ().extendedOperation (request);
  }

  public Control[] getConnectControls ()
    throws NamingException
  {
    return getDefaultInitLdapCtx ().getConnectControls ();
  }

  public Control[] getRequestControls ()
    throws NamingException
  {
    return getDefaultInitLdapCtx ().getRequestControls ();
  }

  public Control[] getResponseControls ()
    throws NamingException
  {
    return getDefaultInitLdapCtx ().getResponseControls ();
  }

  public LdapContext newInstance (Control[] reqControls)
    throws NamingException
  {
    return getDefaultInitLdapCtx ().newInstance (reqControls);
  }

  public void reconnect (Control[] connControls)
    throws NamingException
  {
    getDefaultInitLdapCtx ().reconnect (connControls);
  }

  public void setRequestControls (Control[] reqControls)
    throws NamingException
  {
    getDefaultInitLdapCtx ().setRequestControls (reqControls);
  }
}
