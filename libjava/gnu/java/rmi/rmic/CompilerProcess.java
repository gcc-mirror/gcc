/*
  Copyright (c) 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License.
 */

package gnu.java.rmi.rmic;

/** Subclass of Compiler that can be subclassed to invoke a process to
 * do its work.  */
public abstract class CompilerProcess extends Compiler
{
  /** This is used to compute the command line for the process.  */
  public abstract String[] computeArguments (String filename);

  public void compile (String name) throws Exception
  {
    String[] args = computeArguments (name);
    Process p = Runtime.getRuntime ().exec (args);
    // FIXME: probably should collect compiler output here and then
    // put it into the exception message.
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
	throw new Exception ("compiler exited with status: " + result);
      }
  }
}
