/* NamingServiceTransient.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.NamingService;

import gnu.CORBA.OrbFunctional;
import gnu.CORBA.IOR;

import org.omg.CosNaming.NamingContextExt;

import java.io.FileOutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

/**
 * The server for the gnu classpath naming service. This is an executable class
 * that must be started to launch the GNU Classpath CORBA transient naming
 * service.
 *
 * GNU Classpath currently works with this naming service and is also
 * interoperable with the Sun Microsystems naming services from releases 1.3 and
 * 1.4, both transient <i>tnameserv</i> and persistent <i>orbd</i>.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class NamingServiceTransient
{
  /**
   * The default port (900), on that the naming service starts if no
   * -ORBInitialPort is specified in the command line.
   */
  public static final int PORT = 900;

  /**
   * Get the object key for the naming service. The default key is the string
   * "NameService" in ASCII.
   *
   * @return the byte array.
   */
  public static byte[] getDefaultKey()
  {
    try
      { // NameService
        return "NameService".getBytes("UTF-8");
      }
    catch (UnsupportedEncodingException ex)
      {
        throw new InternalError("UTF-8 unsupported");
      }
  }

  /**
   * Start the naming service on the current host at the given port.
   *
   * @param portArgument the port on which the service will be
   * started, or -1 to use the default port, 900
   * @param fileArgument if non-null, store the IOR string of this
   * naming service in a file by this name
   */
  public static void start(int portArgument, String fileArgument)
  {
    int port = PORT;

    if (portArgument > -1)
      port = portArgument;

    String iorf = fileArgument;
    try
      {
        // Create and initialize the ORB
        final OrbFunctional orb = new OrbFunctional();

        OrbFunctional.setPort(port);

        // Create the servant and register it with the ORB
        NamingContextExt namer = new Ext(new TransientContext());

        // Case with the key "NameService".
        orb.connect(namer, "NameService".getBytes());

        // Storing the IOR reference.
        String ior = orb.object_to_string(namer);
        IOR iorr = IOR.parse(ior);
        if (iorf != null)
          {
            FileOutputStream f = new FileOutputStream(iorf);
            PrintStream p = new PrintStream(f);
            p.print(ior);
            p.close();
          }

        new Thread()
        {
          public void run()
          {
            // Wait for invocations from clients.
            orb.run();
          }
        }.start();
      }
    catch (Exception e)
      {
        System.err.println("ERROR: " + e);
        e.printStackTrace(System.err);
      }

    // Restore the default value for allocating ports for the subsequent
    // objects.
    OrbFunctional.setPort(OrbFunctional.DEFAULT_INITIAL_PORT);
  }
}
