/* Main.java -- a standalone viewer for Java applets
   Copyright (C) 2003, 2004, 2006  Free Software Foundation, Inc.

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

package gnu.classpath.tools.appletviewer;

import gnu.classpath.tools.getopt.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;
import java.applet.Applet;
import java.awt.Dimension;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ResourceBundle;


class Main
{
  /**
   * The localized strings are kept in a separate file.
   */
  public static final ResourceBundle messages = ResourceBundle.getBundle
    ("gnu.classpath.tools.appletviewer.MessagesBundle");

  private static HashMap classLoaderCache = new HashMap();
  
  private static ClassLoader getClassLoader(URL codebase, ArrayList archives)
  {
    // Should load class loader each time. It is possible that there
    // are more than one applet to be loaded with different archives.
    AppletClassLoader loader = new AppletClassLoader(codebase, archives);
    classLoaderCache.put(codebase, loader);

    return loader;
  }
  
  private static String code = null;
  private static String codebase = null;
  private static String archive = null;
  private static List parameters = new ArrayList();
  private static Dimension dimensions = new Dimension(-1, -1);
  private static String pipeInName = null;
  private static String pipeOutName = null;
  private static boolean pluginMode = false;
  private static Parser parser = null;

  static Applet createApplet(AppletTag tag)
  {
    Applet applet = null;

    try
      {
        ClassLoader loader = getClassLoader(tag.prependCodeBase(""),
                                            tag.getArchives());
        String code = tag.getCode();

        if (code.endsWith(".class"))
          code = code.substring(0, code.length() - 6).replace('/', '.');

        Class c = loader.loadClass(code);
        applet = (Applet) c.newInstance();
      }
    catch (Exception e)
      {
        e.printStackTrace();
      }

    if (applet == null)
      applet = new ErrorApplet("Error loading applet");

    return applet;
  }

  protected static boolean verbose;

  /**
   * The main method starting the applet viewer.
   *
   * @param args the arguments given on the command line.
   *
   * @exception IOException if an error occurs.
   */
  public static void main(String[] args) throws IOException
  {
    parser = new ClasspathToolParser("appletviewer", true);
    parser.setHeader("usage: appletviewer [OPTION] -code CODE | URL...");

    OptionGroup attributeGroup = new OptionGroup("Applet tag options");

    attributeGroup.add(new Option("code", Main.messages.getString
                                  ("gcjwebplugin.code_description"),
                                  "CODE")
      {
        public void parsed(String argument) throws OptionException
        {
          code = argument;
        }
      });
    attributeGroup.add(new Option("codebase", Main.messages.getString
                                  ("gcjwebplugin.codebase_description"),
                                  "CODEBASE")
      {
        public void parsed(String argument) throws OptionException
        {
          codebase = argument;
        }
      });
    attributeGroup.add(new Option("archive", Main.messages.getString
                                  ("gcjwebplugin.archive_description"),
                                  "ARCHIVE")
      {
        public void parsed(String argument) throws OptionException
        {
          archive = argument;
        }
      });
    attributeGroup.add(new Option("width", Main.messages.getString
                                  ("gcjwebplugin.width_description"),
                                  "WIDTH")
      {
        public void parsed(String argument) throws OptionException
        {
          dimensions.width = Integer.parseInt(argument);
        }
      });
    attributeGroup.add(new Option("height", Main.messages.getString
                                  ("gcjwebplugin.height_description"),
                                  "HEIGHT")
      {
        public void parsed(String argument) throws OptionException
        {
          dimensions.height = Integer.parseInt(argument);
        }
      });
    attributeGroup.add(new Option("param", Main.messages.getString
                                  ("gcjwebplugin.param_description"),
                                  "NAME,VALUE")
      {
        public void parsed(String argument) throws OptionException
        {
          parameters.add(argument);
        }
      });
    OptionGroup pluginGroup = new OptionGroup("Plugin option");
    pluginGroup.add(new Option("plugin", Main.messages.getString
                               ("gcjwebplugin.plugin_description"),
                               "INPUT,OUTPUT")
      {
        public void parsed(String argument) throws OptionException
        {
          pluginMode = true;
          int comma = argument.indexOf(',');
          pipeInName = argument.substring(0, comma);
          pipeOutName = argument.substring(comma + 1);
        }
      });
    OptionGroup debuggingGroup = new OptionGroup("Debugging option");
    debuggingGroup.add(new Option("verbose", Main.messages.getString
                                  ("gcjwebplugin.verbose_description"),
                                  (String) null)
      {
        public void parsed(String argument) throws OptionException
        {
          verbose = true;
        }
      });
    OptionGroup compatibilityGroup = new OptionGroup("Compatibility options");
    compatibilityGroup.add(new Option("debug", Main.messages.getString
                                      ("gcjwebplugin.debug_description"),
                                      (String) null)
      {
        public void parsed(String argument) throws OptionException
        {
          // Currently ignored.
        }
      });
    compatibilityGroup.add(new Option("encoding", Main.messages.getString
                                      ("gcjwebplugin.encoding_description"),
                                      "CHARSET")
      {
        public void parsed(String argument) throws OptionException
        {
          // FIXME: We should probably be using
          // java.nio.charset.CharsetDecoder to handle the encoding.  What
          // is the status of Classpath's implementation?
        }
      });
    parser.add(attributeGroup);
    parser.add(pluginGroup);
    parser.add(debuggingGroup);
    parser.add(compatibilityGroup);

    String[] urls = parser.parse(args);

    // Print arguments.
    printArguments(args);

    args = urls;

    if (dimensions.height < 0)
      dimensions.height = 200;

    if (dimensions.width < 0)
      dimensions.width = (int) (1.6 * dimensions.height);

    //System.setSecurityManager(new AppletSecurityManager(pluginMode));

    if (pluginMode)
      {
	InputStream in;
	OutputStream out;

        in = new FileInputStream(pipeInName);
        out = new FileOutputStream(pipeOutName);

	PluginAppletViewer.start(in, out);
      }
    else
      {
        if (code == null)
          {
            // The --code option wasn't given and there are no URL
            // arguments so we have nothing to work with.
            if (args.length == 0)
              {
                System.err.println(Main.messages.getString("gcjwebplugin.no_input_files"));
                System.exit(1);
              }
            // Create a standalone appletviewer from a list of URLs.
            new StandaloneAppletViewer(args);
          }
        else
          {
            // Create a standalone appletviewer from the --code
            // option.
            new StandaloneAppletViewer(code, codebase, archive, parameters, dimensions);
          }
      }
  }

  static void printArguments(String[] args)
  {
    if (verbose)
      {
        System.out.println("raw arguments:");

        for (int i = 0; i < args.length; i++)
          System.out.println(" " + args[i]);
      }
  }
}
