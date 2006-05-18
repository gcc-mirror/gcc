/* RMIC.java -- RMI stub generator.
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


package gnu.classpath.tools.rmi;

import gnu.classpath.tools.HelpPrinter;
import gnu.classpath.tools.giop.GRMIC;
import gnu.classpath.tools.rmi.rmic.RmicCompiler;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Generates the ordinary stubs (not GIOP based) for java.rmi.*  package.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class RMIC
{
  /**
   * The version of the compiler.
   */
  public static String VERSION = "0.0 alpha pre";
  
  /**
   * The GRMIC compiler methods
   * 
   * @param args the compiler parameters.
   */
  public static void main(String[] args)
  {
    // Check for the -iiop or -giop keys. If one of these keys is present,
    // forward all call to GRMIC.
    for (int i = 0; i < args.length; i++)
      {
        if (args[i].equals("-giop") || args[i].equals("-iiop"))
          {
            GRMIC.main(args);
            return;
          }
      }
    
    boolean noWrite = false;
    boolean verbose = false;

    String HelpPath = "rmi/RMIC.txt";

    HelpPrinter.checkHelpKey(args, HelpPath);

    File output = new File(".");

    if (args.length == 0)
      {
        HelpPrinter.printHelpAndExit(HelpPath);
      }
    else
      {
        RmicCompiler compiler = new RmicCompiler();

        int cl = - 1;

        Options: for (int i = 0; i < args.length; i++)
          {
            String c = args[i];
            if (c.equals("-v"))
              {
                printVersion();
                System.exit(0);
              }
            else if (c.equals("-nowrite"))
              noWrite = true;
            else if (c.equals("-nowarn"))
              compiler.setWarnings(false);
            else if (c.equals("-verbose"))
              {
                verbose = true;
                compiler.setVerbose(true);
              }
            else if (c.equals("-force"))
              {
                compiler.setForce(true);
              }            
            else if (c.equals("-d"))
              {
                int f = i + 1;
                if (f < args.length)
                  {
                    output = new File(args[f]);
                    i++;
                  }
                else
                  HelpPrinter.printHelpAndExit(HelpPath);
              }
            else if (c.charAt(0) != '-')
            // No more options - start of class list.
              {
                cl = i;
                break Options;
              }
          }

        if (cl < 0)
          HelpPrinter.printHelpAndExit(HelpPath);

        if (verbose)
          System.out.println("Compiling to " + output.getAbsolutePath());

        // Compile classes
        Compile: for (int i = cl; i < args.length; i++)
          {
            if (args[i].charAt(0) != '-')
              {
                compiler.reset();
                Class c = null;
                try
                  {
                    c = Thread.currentThread().getContextClassLoader().loadClass(
                                                                                 args[i]);
                  }
                catch (ClassNotFoundException e)
                  {
                    System.err.println(args[i] + " class not found.");
                    System.exit(1);
                  }

                compiler.compile(c);
                String packag = compiler.getPackageName().replace('.', '/');
                File fw = new File(output, packag);

                // Generate stub.
                String stub = compiler.generateStub();
                String subName = compiler.getStubName() + "_Stub.java";

                if (noWrite)
                  continue Compile;

                try
                  {
                    fw.mkdirs();
                    OutputStream out = new FileOutputStream(new File(fw,
                                                                     subName));
                    out.write(stub.getBytes());
                    out.close();
                  }
                catch (IOException ioex)
                  {
                    System.err.println("Output path not accessible");
                    ioex.printStackTrace();
                    System.exit(1);
                  }
              }
          }
      }
  }
  
  /**
   * Print the version information.
   */
  public static void printVersion()
  {
    System.out.println
      ("rmic v "+VERSION+" - RMI stub generator for java.rmi.* ");
  }
}
