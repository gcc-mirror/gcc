/* CallbackUtil.java -- Callback related utilities
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.tools.common;

import gnu.javax.security.auth.callback.ConsoleCallbackHandler;

import java.security.Provider;
import java.security.Security;
import java.util.logging.Logger;

import javax.security.auth.callback.CallbackHandler;

/**
 * A <i>Helper</i> class containing general purpose utlity methods dealing with
 * callback handlers and their <i>Security Provider</i>.
 */
public abstract class CallbackUtil
{
  private static final Logger log = Logger.getLogger(CallbackUtil.class.getName());

  // default 0-arguments constructor

  // Class methods
  // --------------------------------------------------------------------------

  /**
   * Return an implementation of the {@link CallbackHandler}, from any
   * {@link Provider}, capable of handling callbacks through the <i>console</i>;
   * i.e. <code>System.in</code> and <code>System.out</code>.
   * <p>
   * If no <i>Security Provider</i> for this type of callback was found, this
   * method returns the default GNU implementation.
   *
   * @return a console {@link CallbackHandler} implementation.
   */
  public static final CallbackHandler getConsoleHandler()
  {
    CallbackHandler result = getHandler("Console");
    if (result == null)
      {
        log.fine("No console callback handler found. Will use ours");
        result = new ConsoleCallbackHandler();
      }
    return result;
  }

  /**
   * Return a {@link CallbackHandler}, of a designated type, for interacting
   * with the user.
   * <p>
   * This method first finds all currently installed <i>Security Providers</i>
   * capable of providing such service and then in turn attempts to instantiate
   * the handler from those providers. As soon as one provider returns a non-
   * null instance of the callback handler, the search stops and that instance
   * is returned.
   *
   * @return a {@link CallbackHandler} of the designated type, or
   *         <code>null</code> if no provider was found for theis type of
   *         callback.
   */
  private static final CallbackHandler getHandler(String handlerType)
  {
    log.entering(CallbackUtil.class.getName(), "getHandler", handlerType);

    CallbackHandler result = null;
    String service = "CallbackHandler." + handlerType;
    Provider[] providers = Security.getProviders(service);
    if (providers != null)
      for (int i = 0; i < providers.length; i++)
        {
          Provider p = providers[i];
          String className = p.getProperty(service);
          if (className != null)
            try
              {
                result = (CallbackHandler) Class.forName(className.trim()).newInstance();
              }
            catch (InstantiationException x)
              {
                log.fine("InstantiationException while creating ["
                         + className + "] from provider [" + p.getName()
                         + "]. Ignore");
              }
            catch (IllegalAccessException x)
              {
                log.fine("IllegalAccessException while creating ["
                         + className + "] from provider [" + p.getName()
                         + "]. Ignore");
              }
            catch (ClassNotFoundException x)
              {
                log.fine("ClassNotFoundException while creating ["
                         + className + "] from provider [" + p.getName()
                         + "]. Ignore");
              }

            if (result != null)
              {

                log.fine("Will use [" + result.getClass().getName()
                         + "] from [" + p.getName() + "]");
                break;
              }
        }

    log.exiting(CallbackUtil.class.getName(), "getHandler", result);
    return result;
  }
}
