/* ControlFactory.java --
   Copyright (C) 2001, 2004  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package javax.naming.ldap;

import java.util.Hashtable;
import java.util.StringTokenizer;

import javax.naming.Context;
import javax.naming.NamingException;

/**
 * @author Tom Tromey (tromey@redhat.com)
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
