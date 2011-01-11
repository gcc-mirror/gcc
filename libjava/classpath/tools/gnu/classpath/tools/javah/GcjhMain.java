/* GcjhMain.java - gcjh main program
 Copyright (C) 2007 Free Software Foundation, Inc.

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


package gnu.classpath.tools.javah;

import gnu.classpath.tools.common.ClasspathToolParser;

import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;

import java.io.IOException;
import java.util.ArrayList;

public class GcjhMain extends Main
{
  ArrayList<Text> commands = new ArrayList<Text>();

  public GcjhMain()
  {
    cni = true;
  }

  protected String getName()
  {
    return "gcjh";
  }

  protected ClasspathToolParser getParser()
  {
    ClasspathToolParser result = super.getParser();

    result.setHeader("usage: gcjh [OPTION]... CLASS...");

    OptionGroup text = new OptionGroup("CNI text options");
    text.add(new Option("add", "Insert TEXT into class body", "TEXT")
      {
        public void parsed(String arg) throws OptionException
        {
          commands.add(new Text(Text.ADD, arg));
        }
      });
    text.add(new Option("append", "Append TEXT after class declaration",
                        "TEXT")
      {
        public void parsed(String arg) throws OptionException
        {
          commands.add(new Text(Text.APPEND, arg));
        }
      });
    text.add(new Option("friend", "Insert TEXT as a 'friend' declaration",
                        "TEXT")
      {
        public void parsed(String arg) throws OptionException
        {
          commands.add(new Text(Text.FRIEND, arg));
        }
      });
    text.add(new Option("prepend", "Insert TEXT before start of class", "TEXT")
      {
        public void parsed(String arg) throws OptionException
        {
          commands.add(new Text(Text.PREPEND, arg));
        }
      });
    result.add(text);

    OptionGroup compat = new OptionGroup("Compatibility options (unused)");
    // gcjh itself had compatibility options -old and -trace.  I
    // didn't add them here since they should really be unused by now.
    compat.add(new Option("td", "Unused compatibility option", "DIRECTORY")
      {
        public void parsed(String arg) throws OptionException
        {
        }
      });
    // I don't believe anyone ever used these options.
    compat.add(new Option("M", "Unused compatibility option")
      {
        public void parsed(String arg) throws OptionException
        {
        }
      });
    compat.add(new Option("MM", "Unused compatibility option")
      {
        public void parsed(String arg) throws OptionException
        {
        }
      });
    compat.add(new Option("MD", "Unused compatibility option")
      {
        public void parsed(String arg) throws OptionException
        {
        }
      });
    compat.add(new Option("MMD", "Unused compatibility option")
      {
        public void parsed(String arg) throws OptionException
        {
        }
      });

    result.add(compat);

    return result;
  }

  protected void postParse(String[] names)
  {
    for (int i = 0; i < names.length; ++i)
      textMap.put(names[i].replace('.', '/'), commands);
  }

  public static void main(String[] args) throws IOException
  {
    new GcjhMain().run(args);
  }
}
