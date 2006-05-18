/* HelpPrinter -- help message printer
   Copyright (C) 2006 Free Software Foundation

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

package gnu.classpath.tools;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * The shared class to print the help message and exit, when applicable. 
 * Support the --help key.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class HelpPrinter
{
  /**
   * Check for the --help, -help and -? keys. If one is found, print help and
   * exit the program.
   * 
   * @param args the program arguments.
   * @param helpResourcePath the path to the help resource, related to the
   *          HelpPrinter class.
   */
  public static void checkHelpKey(String[] args, String helpResourcePath)
  {
    for (int i = 0; i < args.length; i++)
      {
        String a = args[i];
        if (a.equals("-?") || a.equalsIgnoreCase("-help")
            || a.equalsIgnoreCase("--help"))
          printHelpAndExit(helpResourcePath);
      }
  }

  /**
   * Prints the contents of the resource specified by the designated path.
   * 
   * @param helpResourcePath the path to a help resource, related to the
   *          HelpPrinter class.
   */
  public static void printHelp(String helpResourcePath)
  {
    InputStream in = HelpPrinter.class.getResourceAsStream(helpResourcePath);
    BufferedReader br = new BufferedReader(new InputStreamReader(in));
    try
      {
        String s;
        while ((s = br.readLine()) != null)
          System.out.println(s);
      }
    catch (IOException x)
      {
        System.err.print("Resource loading is broken:");
        x.printStackTrace(System.err);
      }
    finally
      {
        try
          {
            br.close();
          }
        catch (IOException ignored)
          {
          }
      }
  }

  /**
   * Prints the help message and terminates.
   * 
   * @param helpResourcePath the path to the help resource, related to the
   *          HelpPrinter class.
   */
  public static void printHelpAndExit(String helpResourcePath)
  {
    printHelp(helpResourcePath);
    System.exit(0);
  }
}
