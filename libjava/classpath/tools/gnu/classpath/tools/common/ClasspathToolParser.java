/* ClasspathToolParser.java -- Parser subclass for classpath tools
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


package gnu.classpath.tools.common;

import java.text.MessageFormat;

import gnu.classpath.Configuration;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

/**
 * This is like the Parser class, but is specialized for use by
 * tools distributed with GNU Classpath.  In particular it automatically
 * computes the version string using the program's name.
 */
public class ClasspathToolParser
    extends Parser
{
  private static String getVersionString(String programName)
  {
    String fmt = (Messages.getString("ClasspathToolParser.VersionFormat")); //$NON-NLS-1$
    return MessageFormat.format(fmt, 
                                new Object[]
                                  {
                                    programName,
                                    Configuration.CLASSPATH_VERSION
                                  });
  }

  public ClasspathToolParser(String programName)
  {
    this(programName, false);
  }

  public ClasspathToolParser(String programName, boolean longOnly)
  {
    super(programName, getVersionString(programName), longOnly);
    addFinal(new Option('J',
                        Messages.getString("ClasspathToolParser.JArgument"),//$NON-NLS-1$
                        Messages.getString("ClasspathToolParser.JName"), //$NON-NLS-1$
                        true)
             {
               public void parsed(String argument) throws OptionException
               {
                 // -J should be handled by the wrapper binary.
                 // We add it here so that it shows up in the --help output.
               }
             });
  }
}
