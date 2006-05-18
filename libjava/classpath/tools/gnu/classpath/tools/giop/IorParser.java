/* IorParser.java -- IOR parser.
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
*/


package gnu.classpath.tools.giop;

import gnu.CORBA.IOR;
import gnu.classpath.tools.HelpPrinter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.omg.CORBA.BAD_PARAM;

/**
 * Parses the information, encoded in the Interoperable Object References
 * (IORs).
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)  
 */
public class IorParser
{
  /**
   * Parse and print IOR reference. The system exit code is 0 if the parsed
   * IOR was correct, 1 if it was invalid or missing.
   * 
   * @param args supports -f file to read IOR from the file.
   */
  public static void main(String[] args)
  {
    boolean ok = false;
    String HelpResource = "giop/IorParser.txt";
    HelpPrinter.checkHelpKey(args, HelpResource);
    if (args.length == 0)
      HelpPrinter.printHelpAndExit(HelpResource);
    else if (args[0].equals("-f") && args.length==2)
      {        
        File file = new File(args[1]);
        if (!file.exists())
          System.err.println("The file "+file.getAbsolutePath()+" is missing.");
        // Read IOR reference from file.
        String ior = null;        
    try
      {
        FileReader fr = new FileReader(file);
        BufferedReader br = new BufferedReader(fr);
        ior = br.readLine();
        br.close();
        ok = parseAndPrint(ior);
      }
    catch (IOException e)
      {
        System.err.print("Unable to read the file "+file);
        e.printStackTrace();
      }
        
      }
    else if (args.length == 1)
      ok = parseAndPrint(args[0]);
    else
      HelpPrinter.printHelpAndExit(HelpResource);
    
    if (ok)
      System.exit(0);
    else
      System.exit(1);
  }
  
  /**
   * Parse and print IOR.
   * 
   * @param ior the IOR string to anlyse.
   * @return true if the passed value is a valid IOR, false otherwise.
   */
  public static boolean parseAndPrint(String ior)
  {
    try
      {
        IOR gior = IOR.parse(ior);
        System.out.println(gior.toStringFormatted());
        return true;
      }
    catch (BAD_PARAM e)
      {
        System.out.println("Invalid ior: "+e.getMessage());
        return false;
      }
  }
}
