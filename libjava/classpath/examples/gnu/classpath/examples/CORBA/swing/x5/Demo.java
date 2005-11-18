/* Demo.java --
 Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.classpath.examples.CORBA.swing.x5;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

/**
 * The main executable class of the game client.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class Demo
{

  public static void main(String[] args)
  {
    ClientFrame frame = new ClientFrame();
    frame.setSize(new Dimension(640, 480));
    frame.setTitle("Make vertical, horizontal or diagonal line of 5 dots. "
      + "Click mouse to set the dot.");
    frame.validate();

    // Center the window
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    Dimension frameSize = frame.getSize();
    if (frameSize.height > screenSize.height)
      {
        frameSize.height = screenSize.height;
      }
    if (frameSize.width > screenSize.width)
      {
        frameSize.width = screenSize.width;
      }
    frame.setLocation((screenSize.width - frameSize.width) / 2,
      (screenSize.height - frameSize.height) / 2);
    frame.setVisible(true);

    // Set the ior.
    try
      {
        if (OrbStarter.WRITE_URL_TO_FILE != null)
          {
            File saved_ior = new File(OrbStarter.WRITE_URL_TO_FILE);
            if (saved_ior.exists())
              {
                FileReader f = new FileReader(saved_ior);
                String s = new BufferedReader(f).readLine();
                frame.taUrl.setText(s);
              }
          }
      }
    catch (Exception e)
      {
        // We will print the exception, because this is a demo program -
        // expected to be modified by user.
        e.printStackTrace();
      }
  }
}