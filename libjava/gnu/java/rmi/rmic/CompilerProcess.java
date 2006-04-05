/*
  Copyright (c) 2001, 2003 Free Software Foundation, Inc.

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

package gnu.java.rmi.rmic;

import java.io.InputStream;

/**
 * Subclass of Compiler that can be subclassed to invoke a process to
 * do its work.
 */
public abstract class CompilerProcess extends Compiler
{
  /** This is used to compute the command line for the process.  */
  public abstract String[] computeArguments (String filename);

   /**
    * This is used to compute the command line for the process.
    * Most compilers typically arrange their arguments as in
    * &lt;compiler name and arguments&gt; &lt;optional destination&gt; &lt;filename&gt;.
    * This method builds an argument array out that. It should be used
    * to define computeArguments for those compilers that follow the
    * argument convention described above.
    */
   public static String[] computeTypicalArguments(String[] compilerArgs,
	String destination, String filename)
   {
     return computeTypicalArguments(compilerArgs, null, destination, filename);
   }
   /**
    * This is used to compute the command line for the process.
    * Most compilers typically arrange their arguments as in
    * &lt;compiler name and arguments&gt; &lt;optional destination&gt; &lt;filename&gt;.
    * This method builds an argument array out that. It should be used
    * to define computeArguments for those compilers that follow the
    * argument convention described above.
    */
   public static String[] computeTypicalArguments(String[] compilerArgs,
                                                  String classpath,
                                                  String destination,
                                                  String filename)
   {
     /* length of compiler specific arguments */
     int len = compilerArgs.length;

     /* length of returned array of arguments */
     final int arglen = len + (classpath == null ? 0 : 2) +
       (destination == null ? 0 : 2) + 1;

     /* Allocate String array for computed arguments. */
     String [] args = new String[arglen];

     /* Fill in compiler arguments. */
     System.arraycopy(compilerArgs, 0, args, 0, len);

     /* Fill in classpath argument if necessary. */
     if (classpath != null)
       {
         args[len++] = "-classpath";
         args[len++] = classpath;
       }

     /* Fill in destination argument if necessary. */
     if (destination != null)
      {
	args[len++] = "-d";
	args[len++] = destination;
      }

     /* Fill in filename */
     args[arglen - 1] = filename;

     return args;
   }

  public void compile (String name) throws Exception
  {
    String[] args = computeArguments (name);
    Process p = Runtime.getRuntime ().exec (args);

    /* Print compiler output to System.out.  Do this asynchronously so
       that the compiler never blocks writing to its stdout.  */
    {
      final InputStream procin = p.getInputStream();
      final Thread copier = new Thread() 
	{
	  public void run()
	  {
	    try
	      {
		for (int ch = procin.read(); ch != -1; ch = procin.read())
		  System.out.print((char) ch);
	      }
	    catch (java.io.IOException _)
	      {
	      }
	  }
	};

      copier.start();
    }

    /* Collect compiler error output in a buffer.
     * If compilation fails, it will be used for an error message.
     */
    StringBuffer stderr = new StringBuffer();
    InputStream procerr = p.getErrorStream();
    for (int ch = procerr.read(); ch != -1; ch = procerr.read())
      stderr.append((char) ch);

    int result;
    while (true)
      {
	try
	  {
	    result = p.waitFor ();
	    break;
	  }
	catch (InterruptedException _)
	  {
	  }
      }
    if (result != 0)
      {
	// FIXME: wrong exception class.
	throw new Exception ("compiler exited with status: " + result,
			     new RMICException(stderr.toString()));
      }
  }
}
