/* Main.java -- Transient GIOP naming service.
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

package gnu.classpath.tools.tnameserv;

import gnu.CORBA.NamingService.NamingServiceTransient;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

/**
 * The implementation of the transient naming service. The naming service
 * is a kind of the network server that registers local and remote objects
 * by name, and later can provide the object reference in response to the
 * given name.
 *
 * GNU Classpath currently works with this naming service and is also
 * interoperable with the Sun Microsystems naming services from releases 1.3 and
 * 1.4, both transient <i>tnameserv</i> and persistent <i>orbd</i>.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Main
{
  private int port = -1;
  private String iorf;

  private Parser initializeParser()
  {
    Parser parser = new ClasspathToolParser("tnameserv", true); //$NON-NLS-1$
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

    return parser;
  }

  private void run(String[] args)
  {
    Parser p = initializeParser();
    p.parse(args);
    NamingServiceTransient.start(port, iorf);
  }

  /**
   * The naming service entry point.
   */
  public static void main(String[] args)
  {
    Main tnameservprogram = new Main();
    try
      {
        tnameservprogram.run(args);
      }
    catch (Exception e)
      {
        System.err.println(Messages.getString("Main.InternalError")); //$NON-NLS-1$
        e.printStackTrace(System.err);
        System.exit(1);
      }
  }
}
