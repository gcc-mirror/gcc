/* Configuration.java
   Copyright (C) 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package javax.security.auth.login;

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.Security;

import javax.security.auth.AuthPermission;

public abstract class Configuration
{

  // Fields.
  // -------------------------------------------------------------------------

  private static Configuration config;

  // Constructors.
  // -------------------------------------------------------------------------

  protected Configuration()
  {
  }

  // Class methods.
  // -------------------------------------------------------------------------

  public static synchronized Configuration getConfiguration()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission (new AuthPermission ("getLoginConfiguration"));
    if (config == null)
      {
        String conf = (String) AccessController.doPrivileged
          (new PrivilegedAction()
            {
              public Object run()
              {
                return Security.getProperty ("login.configuration.provider");
              }
            });
        try
          {
            if (conf != null)
              config = (Configuration) Class.forName (conf).newInstance();
            else
              config = new NullConfiguration();
          }
        catch (Exception x)
          {
            config = new NullConfiguration();
          }
      }
    return config;
  }

  public static synchronized void setConfiguration (Configuration config)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission (new AuthPermission ("setLoginConfiguration"));
    Configuration.config = config;
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  public abstract AppConfigurationEntry[] getAppConfigurationEntry (String applicationName);

  public abstract void refresh();

  // Package-private methods.
  // -------------------------------------------------------------------------

  /**
   * Get the current configuration, bypassing security checks.
   */
  static Configuration getConfig()
  {
    return config;
  }
}
