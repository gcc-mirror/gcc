/* LoginContext.java
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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

import gnu.java.security.action.GetSecurityPropertyAction;

import java.security.AccessController;

import java.util.HashMap;
import java.util.Map;

import javax.security.auth.Subject;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.spi.LoginModule;

public class LoginContext
{

  private static final String OTHER = "other";

  private final String name;
  private final CallbackHandler cbHandler;
  private final Subject subject;
  private final AppConfigurationEntry[] entries;
  private final LoginModule[] modules;
  private final Map sharedState;

  public LoginContext (final String name) throws LoginException
  {
    this (name, new Subject(), defaultHandler());
  }

  public LoginContext (final String name, final CallbackHandler cbHandler)
    throws LoginException
  {
    this (name, new Subject(), cbHandler);
  }

  public LoginContext (final String name, final Subject subject)
    throws LoginException
  {
    this (name, subject, defaultHandler());
  }

  public LoginContext (final String name, final Subject subject,
                       final CallbackHandler cbHandler)
    throws LoginException
  {
    this (name, subject, cbHandler, null);
  }

  /** @since 1.5 */
  public LoginContext (final String name, final Subject subject,
                       final CallbackHandler cbHandler,
                       Configuration config)
    throws LoginException
  {
    this.name = name;
    this.subject = subject;
    this.cbHandler = cbHandler;
    if (config == null)
      config = Configuration.getConfig();
    AppConfigurationEntry[] entries = config.getAppConfigurationEntry (name);
    if (entries == null)
      entries = config.getAppConfigurationEntry (OTHER);
    if (entries == null)
      throw new LoginException ("no configured modules for application "
                                + name);
    this.entries = entries;
    modules = new LoginModule[entries.length];
    sharedState = new HashMap();
    for (int i = 0; i < entries.length; i++)
      modules[i] = lookupModule (entries[i], subject, sharedState);
  }

  /**
   * Returns the authenticated subject, or the parameter passed to one
   * of the constructors. <code>null</code> is returned if the previous
   * login attempt failed and there was no subject provided.
   *
   * @return The subject, or null.
   */
  public Subject getSubject()
  {
    return subject;
  }

  /**
   * Logs a subject in, using all login modules configured for this
   * application. This method will call the {@link LoginModule#login()}
   * method of each module configured for this application, stopping
   * if a REQUISITE module fails or if a SUFFICIENT module succeeds. If
   * the overall login attempt fails, a {@link LoginException} will be
   * thrown.
   *
   * @throws LoginException If logging in fails.
   */
  public void login() throws LoginException
  {
    boolean failure = false;
    for (int i = 0; i < modules.length; i++)
      {
        try
          {
            boolean result = modules[i].login();
            if (!result)
              {
                if (entries[i].getControlFlag() ==
                    AppConfigurationEntry.LoginModuleControlFlag.REQUISITE)
                  throw new LoginException ("REQUISITE module " + entries[i].getLoginModuleName()
                                            + " failed");
                else if (entries[i].getControlFlag() ==
                         AppConfigurationEntry.LoginModuleControlFlag.REQUIRED)
                  failure = true;
              }
            else
              {
                if (entries[i].getControlFlag() ==
                    AppConfigurationEntry.LoginModuleControlFlag.SUFFICIENT)
                  break;
              }
          }
        catch (LoginException le)
          {
            if (entries[i].getControlFlag() !=
                AppConfigurationEntry.LoginModuleControlFlag.REQUISITE)
              continue;
            for (int j = 0; j < modules.length; j++)
              modules[i].abort();
            throw le;
          }
      }
    if (failure)
      throw new LoginException ("not all REQUIRED modules succeeded");

    for (int i = 0; i < modules.length; i++)
      modules[i].commit();
  }

  /**
   * Logs a subject out, cleaning up any state that may be in memory.
   *
   * @throws LoginException If logging out fails.
   */
  public void logout() throws LoginException
  {
    for (int i = 0; i < modules.length; i++)
      modules[i].logout();
  }

  // Own methods.

  /**
   * Fetch the default callback handler, based on the
   * auth.login.defaultCallbackHandler property, or null if it is not
   * set.
   */
  private static CallbackHandler defaultHandler()
  {
    GetSecurityPropertyAction act =
      new GetSecurityPropertyAction ("auth.login.defaultCallbackHandler");
    String classname = (String) AccessController.doPrivileged (act);
    if (classname != null)
      {
        try
          {
            return (CallbackHandler) Class.forName (classname).newInstance();
          }
        catch (ClassNotFoundException cnfe)
          {
            return null;
          }
        catch (ClassCastException cce)
          {
            return null;
          }
        catch (IllegalAccessException iae)
          {
            return null;
          }
        catch (InstantiationException ie)
          {
            return null;
          }
      }
    return null;
  }

  private LoginModule lookupModule (AppConfigurationEntry entry,
                                    Subject subject, Map sharedState)
    throws LoginException
  {
    LoginModule module = null;
    Exception cause = null;
    try
      {
        ClassLoader cl = Thread.currentThread().getContextClassLoader();
        Class c = Class.forName(entry.getLoginModuleName(), true, cl);
        module = (LoginModule) c.newInstance();
      }
    catch (ClassNotFoundException cnfe)
      {
        cause = cnfe;
      }
    catch (ClassCastException cce)
      {
        cause = cce;
      }
    catch (IllegalAccessException iae)
      {
        cause = iae;
      }
    catch (InstantiationException ie)
      {
        cause = ie;
      }

    if (cause != null)
      {
        LoginException le = new LoginException ("could not load module "
                                                + entry.getLoginModuleName());
        le.initCause (cause);
        throw le;
      }

    module.initialize (subject, cbHandler, sharedState, entry.getOptions());
    return module;
  }
}
