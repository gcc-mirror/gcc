/* GRMIC.java -- GIOP support for RMIC.
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

package gnu.classpath.tools.giop;

import gnu.classpath.tools.HelpPrinter;
import gnu.classpath.tools.giop.grmic.GiopRmicCompiler;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * The main class of the GIOP compiler to generate stubs and ties for 
 * javax.rmi package.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)  
 */
public class GRMIC
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
    boolean noWrite = false;
    boolean verbose = false;

    String HelpPath = "giop/GRMIC.txt";

    HelpPrinter.checkHelpKey(args, HelpPath);

    File output = new File(".");

    if (args.length == 0)
      {
        HelpPrinter.printHelpAndExit(HelpPath);
      }
    else
      {
        GiopRmicCompiler compiler = new GiopRmicCompiler();

        int cl = - 1;

        Options: for (int i = 0; i < args.length; i++)
          {
            String c = args[i];
            if (c.equals("-poa"))
              compiler.setPoaMode(true);
            else if (c.equals("-impl"))
              compiler.setPoaMode(false);
            else if (c.equals("-v"))
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
                String subName = "_" + compiler.getStubName() + "_Stub.java";

                compiler.reset();
                compiler.compile(c);

                // Generate tie
                String tie = compiler.generateTie();
                String tieName = "_" + compiler.name(c) + "_Tie.java";

                if (noWrite)
                  continue Compile;

                try
                  {
                    fw.mkdirs();
                    OutputStream out = new FileOutputStream(new File(fw,
                                                                     subName));
                    out.write(stub.getBytes());
                    out.close();

                    out = new FileOutputStream(new File(fw, tieName));
                    out.write(tie.getBytes());
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
      ("grmic v "+VERSION+" - GIOP stub and tie generator for javax.rmi.* ");
  }
}
