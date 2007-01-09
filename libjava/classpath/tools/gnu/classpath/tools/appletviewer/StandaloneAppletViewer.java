/* StandaloneAppletViewer.java -- a standalone viewer for Java applets
   Copyright (C) 2003, 2006  Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;


/**
 * StandaloneAppletViewer displays an applet in its own Frame.  Most
 * of the context that is available to an applet within a webpage is
 * available to it in StandaloneAppletViewer.
 */
class StandaloneAppletViewer extends Main
{
  static ArrayList appletTags = new ArrayList();
  static ArrayList appletWindows = new ArrayList();

  StandaloneAppletViewer(String[] urls)
    throws MalformedURLException, IOException
  {
    // Handle each file specified on the command line.
    for (int i = 0; i < urls.length; i++)
      {
        TagParser parser = new TagParser(urls[i]);
        appletTags.addAll(parser.parseAppletTags());
      }

    printTags();
    createWindows();
  }

  StandaloneAppletViewer(String code, String codebase, String archives,
                         List parameters, Dimension dimensions)
    throws IOException
    {
      if (!(code.equals("") || code.endsWith(".class")))
        {
          System.err.println
            (Messages.getString("StandaloneAppletViewer.CodeOptionError"));
          System.exit(1);
        }
      
      String tagString =
        "<EMBED"
        + " CODE=\"" + code + "\""
        + " WIDTH=" + dimensions.width
        + " HEIGHT=" + dimensions.height
        + " CODEBASE=\"" + codebase + "\""
        + " ARCHIVE=\"" + archives + "\">";

      // Handle parameters.
      Iterator pairs = parameters.iterator();
      while (pairs.hasNext())
        {
          StringTokenizer paramTokenizer =
            new StringTokenizer((String) pairs.next(), ",");
          tagString +=
            "<PARAM NAME=" + paramTokenizer.nextToken().trim() + " VALUE="
            + paramTokenizer.nextToken().trim() + ">";
        }
      
      tagString += "</EMBED>";

      StringReader reader = new StringReader(tagString);
      String path = System.getProperty("user.dir") + File.separator;
      TagParser parser = new TagParser(reader,
                                    new URL("file", "", path));
      appletTags.addAll(parser.parseAppletTags());

      printTags();
      createWindows();
    }

  void printTags()
  {
    if (verbose)
      {
        System.out.println
          (Messages.getString("StandaloneAppletViewer.ParsedAppletTags"));

        for (int i = 0; i < appletTags.size(); i++)
          {
            AppletTag tag = (AppletTag) appletTags.get(i);

            System.out.println
              (" " + Messages.getString("StandaloneAppletViewer.Tag")
               + " " + i + ":");
            System.out.println(tag);
          }
      }
  }

  void createWindows()
  {
    for (int i = 0; i < appletTags.size(); i++)
      {
        AppletTag tag = (AppletTag) appletTags.get(i);
          
        // Create a StandaloneAppletWindow and add it to the
        // appletWindows list.
        new StandaloneAppletWindow(tag, appletWindows);
      }
  }
}
