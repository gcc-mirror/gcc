/* Server.java --
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

import gnu.CORBA.Functional_ORB;
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
   * Start the naming service on the current host at the given port. The
   * parameter -org.omg.CORBA.ORBInitialPort NNN or -ORBInitialPort NNN, if
   * present, specifies the port, on that the service must be started. If this
   * key is not specified, the service starts at the port 900.
   * 
   * The parameter -ior FILE_NAME, if present, forces to store the ior string of
   * this naming service to the specified file.
   * 
   * @param args the parameter string.
   */
  public static void main(String[] args)
  {
    int port = PORT;
    String iorf = null;
    try
      {
        // Create and initialize the ORB
        final Functional_ORB orb = new Functional_ORB();

        if (args.length > 1)
          for (int i = 0; i < args.length - 1; i++)
            {
              if (args[i].endsWith("ORBInitialPort"))
                port = Integer.parseInt(args[i + 1]);

              if (args[i].equals("-ior"))
                iorf = args[i + 1];
            }

        Functional_ORB.setPort(port);

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

        System.out.println("GNU Classpath transient naming service "
          + "started at " + iorr.Internet.host + ":" + iorr.Internet.port
          + " key 'NameService'.\n\n"
          + "Copyright (C) 2005 Free Software Foundation\n"
          + "This tool comes with ABSOLUTELY NO WARRANTY. "
          + "This is free software, and you are\nwelcome to "
          + "redistribute it under conditions, defined in "
          + "GNU Classpath license.\n\n" + ior);

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
        e.printStackTrace(System.out);
      }

    // Restore the default value for allocating ports for the subsequent
    // objects.
    Functional_ORB.setPort(Functional_ORB.DEFAULT_INITIAL_PORT);
  }
}
