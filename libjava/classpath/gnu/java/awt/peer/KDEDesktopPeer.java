/* GnomeDesktopPeer.java -- Offers a KDE Desktop peer for DesktopPeer
 Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

package gnu.java.awt.peer;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

/**
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class KDEDesktopPeer
  extends ClasspathDesktopPeer
{
  /**
   * Query string to use if a GNOME desktop is detected to get the name of the
   * default browser. This requires gconftool-2 (part of GNOME).
   */
  private static final String BROWSER_QUERY_GNOME =
    "gconftool-2 -g /desktop/gnome/url-handlers/http/command";

  protected String getCommand(String action)
  {   
    // check if a command already exists
    String command = super.getCommand(action);
    
    if (command == null)
      {
        try
          {
            if (action == _MAIL)
              { 
                command = "kfmclient exec";
              }
            else if (action == _PRINT)
              {
                command = "kprinter";
              }
            else
              {
                command = "kfmclient openURL";
              }
          }
        catch (Exception e)
          {
            command = null;
          }
      }
    
    return command;
  }
     
  protected boolean supportCommand(String check)
  {
    return true;
  }
  
  public void mail() throws IOException
  {
    checkPermissions();
    
    String mail = getCommand(_MAIL);
    
    if (mail == null)
      throw new UnsupportedOperationException();
    
    Runtime.getRuntime().exec(mail + " 'mailto: '");
  }
  
  protected String execQuery(String command) throws IOException
  {
    InputStream in = null;
    StringBuilder output = new StringBuilder();

    try
      {
        Process process = Runtime.getRuntime().exec(command);

        // Get the input stream and read from it
        in = process.getInputStream();
        int c;
        while ((c = in.read()) != - 1)
          {
            output.append((char) c);
          }
      }
    finally
      {
        if (in != null)
          in.close();
      }

    // remove %s from the string, leave only the command line
    int index = output.indexOf("%s");
    output.delete(index, index + 1);
    
    return output.toString().trim();
  }
}
