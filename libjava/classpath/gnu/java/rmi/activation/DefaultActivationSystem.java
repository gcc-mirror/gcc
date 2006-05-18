/* DefaultActivationSystem.java -- Default RMI activation system
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


package gnu.java.rmi.activation;

import java.rmi.activation.ActivationSystem;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

/**
 * Finds and returns the default activation system for this jre.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public abstract class DefaultActivationSystem
{
  /**
   * The activation system (assigned if once found).
   */
  static ActivationSystem system;
  
  /**
   * The default activation registry port.
   */
  static int ACTIVATION_REGISTRY_PORT;
  
  /**
   * The name of the activation system registry port property.
   */
  static String AS_PORT_PROPERTY = "java.rmi.activation.port";
  
  /**
   * The defalut name of the activation system in the activation registry.
   */
  static String ACTIVATION_SYSTEM_NAME = "java.rmi.activation.ActivationSystem";

  /**
   * Get the activation system, default for this jre. If no external activation
   * system exists, the internal activation system will be activated. This
   * internal system is limited in capabilities and should be used exclusively
   * for automated testing, to avoid necessity of starting rmi daemon during
   * testing process.
   */
  public static ActivationSystem get()
  {
    if (system == null)
      try
        {
          // Obtain the port:
          String asr = System.getProperty("java.rmi.activation.port");

          if (asr != null)
            {
              try
                {
                  ACTIVATION_REGISTRY_PORT = Integer.parseInt(asr);
                  if (ACTIVATION_REGISTRY_PORT <= 0)
                    throw new InternalError("Invalid " + asr + " value, "
                                            + ACTIVATION_REGISTRY_PORT);
                }
              catch (NumberFormatException e)
                {
                  throw new InternalError("Unable to parse " + asr
                                          + " to integer");
                }
            }
          else
            ACTIVATION_REGISTRY_PORT = ActivationSystem.SYSTEM_PORT;

          // Expect the naming service running first.
          // The local host may want to use the shared registry
          Registry r = LocateRegistry.getRegistry(ACTIVATION_REGISTRY_PORT);
          ActivationSystem system = (ActivationSystem) r.lookup(ACTIVATION_SYSTEM_NAME);
          return system;
        }
      catch (Exception ex)
        {
          system = ActivationSystemTransient.getInstance();
        }

    return system;
  }
}
