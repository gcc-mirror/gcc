/* NamingServicePersistent.java -- The persistent naming service.
   Copyright (C) 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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

package gnu.classpath.tools.orbd;

import gnu.CORBA.OrbFunctional;
import gnu.CORBA.IOR;
import gnu.CORBA.NamingService.Ext;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

import org.omg.CosNaming.NamingContextExt;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

/**
 * The server for the GNU Classpath persistent naming service.
 *
 * GNU Classpath currently works with this naming service and is also
 * interoperable with the Sun Microsystems naming services from releases 1.3 and
 * 1.4, both transient <i>tnameserv</i> and persistent <i>orbd</i>.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Main
{
  /**
   * The default port (900), on that the naming service starts if no
   * -ORBInitialPort is specified in the command line.
   */
  public static final int PORT = 900;

  private int port = PORT;
  private String iorf;
  private boolean cold;
  private String directory = "";

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

  private Parser initializeParser()
  {
    Parser parser = new ClasspathToolParser("orbd", true); //$NON-NLS-1$
    parser.setHeader(Messages.getString("Main.Usage")); //$NON-NLS-1$

    parser.add(new Option("ORBInitialPort", //$NON-NLS-1$
                          Messages.getString("Main.ORBInitialPort"), //$NON-NLS-1$
                          Messages.getString("Main.Port")) //$NON-NLS-1$
      {
        public void parsed(String portArgument) throws OptionException
        {
          port = Integer.parseInt(portArgument);
        }
      });

    parser.add(new Option("ior", //$NON-NLS-1$
                          Messages.getString("Main.IOR"), //$NON-NLS-1$
                          Messages.getString("Main.IORFile")) //$NON-NLS-1$
      {
        public void parsed(String fileArgument) throws OptionException
        {
          iorf = fileArgument;
        }
      });
    parser.add(new Option("directory", //$NON-NLS-1$
                          Messages.getString("Main.Directory"), //$NON-NLS-1$
                          Messages.getString("Main.DirectoryArgument")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          directory = argument;
        }
      });
    parser.add(new Option("restart", //$NON-NLS-1$
                          Messages.getString("Main.Restart")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          cold = true;
        }
      });

    return parser;
  }

  private void run(String[] args)
  {
    Parser parser = initializeParser();
    parser.parse(args);

    try
      {
        // Create and initialize the ORB
        final OrbFunctional orb = new OrbFunctional();
        OrbFunctional.setPort(port);

        // Create the servant and register it with the ORB
        File dataDirectory = new File(directory);
        System.out.println("Persistent data stored at "
                           + dataDirectory.getAbsolutePath());
        dataDirectory.mkdirs();

        // / TODO support more starting modes.
        NamingContextExt namer = new Ext(
                                         new PersistentContext(
                                                               orb,
                                                               dataDirectory,
                                                               cold));

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

        System.out.println("GNU Classpath persistent naming service "
                           + "started at " + iorr.Internet.host + ":"
                           + iorr.Internet.port + " key 'NameService'.\n\n"
                           + "Copyright (C) 2011 Free Software Foundation\n"
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
    catch (FileNotFoundException e)
      {
        throw new RuntimeException(e);
      }
    finally
      {
        // Restore the default value for allocating ports for the subsequent
        // objects.
        OrbFunctional.setPort(OrbFunctional.DEFAULT_INITIAL_PORT);
      }
  }

  /**
   * The persistent naming service entry point.
   */
  public static void main(String[] args)
  {
    Main orbdprogram = new Main();
    try
      {
        orbdprogram.run(args);
      }
    catch (Exception e)
      {
        System.err.println(Messages.getString("Main.InternalError")); //$NON-NLS-1$
        e.printStackTrace(System.err);
        System.exit(1);
      }
  }
}
