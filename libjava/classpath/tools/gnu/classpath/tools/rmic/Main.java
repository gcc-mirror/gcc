/* Main.java -- RMI stub generator.
   Copyright (C) 2006, 2012 Free Software Foundation, Inc.

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


package gnu.classpath.tools.rmic;

import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

import java.util.ArrayList;

/**
 * Generates the ordinary stubs (not GIOP based) for java.rmi.*  package.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Main
{
  private boolean noWrite;
  private boolean warnings = true;
  private boolean verbose;
  private boolean force;
  private String classpath = ".";
  private String outputDirectory = ".";
  private boolean poa;
  private boolean need11Stubs = false;
  private boolean need12Stubs = true;
  private boolean keep;
  private boolean iiop;
  /**
   * Specifies whether or not JRMP mode was explicitly requested.
   */
  private boolean jrmp;

  private Parser initializeParser()
  {
    Parser parser = new ClasspathToolParser("rmic", true); //$NON-NLS-1$
    parser.setHeader(Messages.getString("Main.Usage")); //$NON-NLS-1$

    parser.add(new Option("nowarn", //$NON-NLS-1$
                          Messages.getString("Main.NoWarn")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          warnings = false;
        }
      });
    parser.add(new Option("nowrite", //$NON-NLS-1$
                          Messages.getString("Main.NoWrite")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          noWrite = true;
        }
      });
    parser.add(new Option("verbose", //$NON-NLS-1$
                          Messages.getString("Main.Verbose")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          verbose = true;
        }
      });
    parser.add(new Option("d", //$NON-NLS-1$
                          Messages.getString("Main.DirOpt"), //$NON-NLS-1$
                          Messages.getString("Main.DirArg")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          outputDirectory = argument;
        }
      });
    parser.add(new Option("classpath", //$NON-NLS-1$
                          Messages.getString("Main.ClasspathOpt"), //$NON-NLS-1$
                          Messages.getString("Main.ClasspathArg")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          classpath = argument;
        }
      });
    parser.add(new Option("bootclasspath", //$NON-NLS-1$
                          Messages.getString("Main.BootclasspathOpt"), //$NON-NLS-1$
                          Messages.getString("Main.BootclasspathArg")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
        }
      });
    parser.add(new Option("extdirs", //$NON-NLS-1$
                          Messages.getString("Main.ExtdirsOpt"), //$NON-NLS-1$
                          Messages.getString("Main.ExtdirsArg")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
        }
      });
    parser.add(new Option("iiop", //$NON-NLS-1$
                          Messages.getString("Main.IIOP")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          iiop = true;
        }
      });
    parser.add(new Option("always", //$NON-NLS-1$
                          Messages.getString("Main.Always")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          force = true;
        }
      });
    parser.add(new Option("alwaysgenerate", //$NON-NLS-1$
                          Messages.getString("Main.AlwaysGenerate")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          force = true;
        }
      });
    parser.add(new Option("nolocalstubs", //$NON-NLS-1$
                          Messages.getString("Main.NoLocalStubs")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
        }
      });
    parser.add(new Option("poa", //$NON-NLS-1$
                          Messages.getString("Main.POA")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          poa = true;
        }
      });
    parser.add(new Option("keep", //$NON-NLS-1$
                          Messages.getString("Main.Keep")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          keep = true;
        }
      });
    parser.add(new Option("keepgenerated", //$NON-NLS-1$
                          Messages.getString("Main.KeepGenerated")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          keep = true;
        }
      });
    parser.add(new Option("v1.1", //$NON-NLS-1$
                          Messages.getString("Main.v11")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          need11Stubs = true;
          need12Stubs = false;
          jrmp = true;
        }
      });
    parser.add(new Option("v1.2", //$NON-NLS-1$
                          Messages.getString("Main.v12")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          jrmp = true;
        }
      });
    parser.add(new Option("vcompat", //$NON-NLS-1$
                          Messages.getString("Main.vcompat")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          need11Stubs = true;
          need12Stubs = true;
          jrmp = true;
        }
      });
    parser.add(new Option("g", //$NON-NLS-1$
                          Messages.getString("Main.DebugInfo")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
        }
      });

    return parser;
  }

  private void run(String[] args)
  {
    Parser p = initializeParser();
    String[] files = p.parse(args);

    if (files.length == 0)
      {
        p.printHelp();
        System.exit(1);
      }

    ArrayList<RmicBackend> backends = new ArrayList<RmicBackend>();

    // FIXME: need an IDL RmicBackend
    // FIXME: need a ClassGiopRmicCompiler RmicBackend
    if (iiop)
      {
        backends.add(new SourceGiopRmicCompiler());

        if (jrmp)
          {
            // Both IIOP and JRMP stubs were requested.
            backends.add(new ClassRmicCompiler());
            // FIXME: SourceRmicCompiler should support v1.1
            if (keep)
              backends.add(new SourceRmicCompiler());
          }
      }
    else
      {
        backends.add(new ClassRmicCompiler());
        if (keep)
          backends.add(new SourceRmicCompiler());
      }

    for (int i = 0; i < backends.size(); i++)
      {
        RmicBackend b = backends.get(i);
        b.setup(keep, need11Stubs, need12Stubs,
                iiop, poa, false, warnings,
                noWrite,  verbose, force, classpath,
                null, null, outputDirectory);
        if (!b.run(files))
          System.exit(1);
      }
  }

  /**
   * The RMI compiler entry point.
   */
  public static void main(String[] args)
  {
    Main rmicprogram = new Main();
    try
      {
        rmicprogram.run(args);
      }
    catch (Exception e)
      {
        System.err.println(Messages.getString("Main.InternalError")); //$NON-NLS-1$
        e.printStackTrace(System.err);
        System.exit(1);
      }
  }

}
