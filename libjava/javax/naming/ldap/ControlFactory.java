/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.ldap;

import javax.naming.*;
import java.util.StringTokenizer;
import java.util.Hashtable;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date June 22, 2001
 */
public abstract class ControlFactory
{
  protected ControlFactory ()
  {
  }

  public abstract Control getControlInstance (Control control)
    throws NamingException;

  public static Control getControlInstance (Control control,
					    Context ctx,
					    Hashtable env)
    throws NamingException
  {
    String path = (String) env.get (LdapContext.CONTROL_FACTORIES);
    String path2 = null;
    if (ctx != null)
      path2 = (String) ctx.getEnvironment ().get (LdapContext.CONTROL_FACTORIES);
    if (path == null)
      path = path2;
    else if (path2 != null)
      path += ":" + path2;

    StringTokenizer tokens = new StringTokenizer (path, ":");
    while (tokens.hasMoreTokens ())
      {
	String name = tokens.nextToken ();
	try
	  {
	    Class k = Class.forName (name);
	    ControlFactory cf = (ControlFactory) k.newInstance ();
	    Control ctrl = cf.getControlInstance (control);
	    if (ctrl != null)
	      return ctrl;
	  }
	catch (ClassNotFoundException _1)
	  {
	    // Ignore it.
	  }
	catch (ClassCastException _2)
	  {
	    // Ignore it.
	  }
	catch (InstantiationException _3)
	  {
	    // If we couldn't instantiate the factory we might get
	    // this.
	  }
	catch (IllegalAccessException _4)
	  {
	    // Another possibility when instantiating.
	  }
      }

    return control;
  }
}
