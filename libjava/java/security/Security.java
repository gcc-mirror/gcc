/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.security;

import java.util.Enumeration;
import java.util.Properties;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date February 8, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification.
 * Status:  Still missing the deprecated getAlgorithmProperty method.
 */

public final class Security
{
  public static int insertProviderAt (Provider provider, int position)
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      {
	// FIXME: need SecurityPermission.
	// sm.checkSecurityAccess ("insertProvider." + provider.getName ());
      }
    if (providers.indexOf (provider) != -1)
      return -1;
    if (position > providers.size ())
      position = providers.size ();
    providers.insertElementAt (provider, position);
    return providers.indexOf (provider);
  }

  public static int addProvider (Provider provider)
  {
    return insertProviderAt (provider, providers.size ());
  }

  public static void removeProvider (String name)
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      {
	// FIXME: need SecurityPermission.
	// sm.checkSecurityAccess ("removeProvider." + name);
      }
    Provider p = getProvider (name);
    if (p != null)
      providers.removeElement (p);
  }

  public static Provider[] getProviders ()
  {
    Provider[] r = new Provider[providers.size ()];
    providers.copyInto (r);
    return r;
  }

  public static Provider getProvider (String name)
  {
    Enumeration e = providers.elements ();
    while (e.hasMoreElements ())
      {
	Provider p = (Provider) e.nextElement ();
	if (name.equals (p.getName ()))
	  return p;
      }
    return null;
  }

  public static String getProperty (String key)
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      {
	// FIXME: need SecurityPermission.
	// sm.checkSecurityAccess ("getProperty." + key);
      }
    return props.getProperty (key);
  }

  public static void setProperty (String key, String value)
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      {
	// FIXME: need SecurityPermission.
	// sm.checkSecurityAccess ("setProperty." + key);
      }
    props.setProperty (key, value);
  }

  // The providers we list.
  private static Vector providers = new Vector ();

  // Security propertiesl
  private static Properties props = new Properties ();
}
