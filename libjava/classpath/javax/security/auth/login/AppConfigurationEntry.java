/* AppConfigurationEntry.java
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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class AppConfigurationEntry
{

  // Fields.
  // -------------------------------------------------------------------------

  private final String loginModuleName;
  private final LoginModuleControlFlag controlFlag;
  private final Map options;

  // Constructor.
  // -------------------------------------------------------------------------

  public AppConfigurationEntry (final String loginModuleName,
                                final LoginModuleControlFlag controlFlag,
                                final Map options)
  {
    if (loginModuleName == null || loginModuleName.length() == 0)
      throw new IllegalArgumentException ("module name cannot be null nor empty");
    if (LoginModuleControlFlag.OPTIONAL != controlFlag &&
        LoginModuleControlFlag.REQUIRED != controlFlag &&
        LoginModuleControlFlag.REQUISITE != controlFlag &&
        LoginModuleControlFlag.SUFFICIENT != controlFlag)
      throw new IllegalArgumentException ("invalid controlFlag");
    if (options == null)
      throw new IllegalArgumentException ("options cannot be null");
    this.loginModuleName = loginModuleName;
    this.controlFlag = controlFlag;
    this.options = Collections.unmodifiableMap (new HashMap (options));
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public LoginModuleControlFlag getControlFlag()
  {
    return controlFlag;
  }

  public String getLoginModuleName()
  {
    return loginModuleName;
  }

  public Map getOptions()
  {
    return options;
  }

// Inner class.
  // -------------------------------------------------------------------------

  public static class LoginModuleControlFlag
  {

    // Constants.
    // -----------------------------------------------------------------------

    public static final LoginModuleControlFlag OPTIONAL = new LoginModuleControlFlag();
    public static final LoginModuleControlFlag REQUIRED = new LoginModuleControlFlag();
    public static final LoginModuleControlFlag REQUISITE = new LoginModuleControlFlag();
    public static final LoginModuleControlFlag SUFFICIENT = new LoginModuleControlFlag();

    // Constructor.
    // -----------------------------------------------------------------------

    private LoginModuleControlFlag()
    {
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public String toString()
    {
      StringBuffer buf = new StringBuffer (LoginModuleControlFlag.class.getName());
      buf.append ('.');
      if (this == OPTIONAL)
        buf.append ("OPTIONAL");
      else if (this == REQUIRED)
        buf.append ("REQUIRED");
      else if (this == REQUISITE)
        buf.append ("REQUISITE");
      else if (this == SUFFICIENT)
        buf.append ("SUFFICIENT");
      else
        buf.append ("HARVEY_THE_RABBIT");
      return buf.toString();
    }
  }
}
