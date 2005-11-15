/* ProgressBarDemo.java -- A demonstration of JProgressBars
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


package gnu.classpath.examples.swing;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JSlider;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class ProgressBarDemo
  extends JFrame
  implements ActionListener
{

  /**
   * Creates a new ProgressBarDemo window with the specified title.
   *
   * @param title the title of the program window
   */
  ProgressBarDemo(String title)
  {
    super(title);
    JPanel content = createContent();
    JPanel closePanel = new JPanel();
    JButton closeButton = new JButton("Close");
    closeButton.setActionCommand("CLOSE");
    closeButton.addActionListener(this);
    closePanel.add(closeButton);
    getContentPane().add(content);
    getContentPane().add(closePanel, BorderLayout.SOUTH);
  }

  static JPanel createContent()
  {
    JPanel content = new JPanel();
    content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));
    JPanel horizontalProgressBar = createHorizontalProgressBar();
    content.add(horizontalProgressBar);
    content.add(Box.createVerticalStrut(10));
    JPanel verticalProgressBar = createVerticalProgressBar();
    content.add(verticalProgressBar);
    return content;
  }

  private static JPanel createHorizontalProgressBar()
  {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

    // Plain progress bar.
    final JProgressBar hor1 = new JProgressBar(JProgressBar.HORIZONTAL, 0, 100);
    panel.add(hor1);
    final JSlider slider1 = new JSlider(JSlider.HORIZONTAL, 0, 100, 0);
    slider1.addChangeListener(new ChangeListener()
      {
        public void stateChanged(ChangeEvent event)
        {
          hor1.setValue(slider1.getValue());
        }
      });
    panel.add(slider1);

    panel.add(Box.createVerticalStrut(5));

    // Plain progress bar with some text.
    final JProgressBar hor2 = new JProgressBar(JProgressBar.HORIZONTAL, 0, 100);
    hor2.setString("ProgressBar Demo");
    hor2.setStringPainted(true);
    panel.add(hor2);
    final JSlider slider2 = new JSlider(JSlider.HORIZONTAL, 0, 100, 0);
    slider2.addChangeListener(new ChangeListener()
      {
        public void stateChanged(ChangeEvent event)
        {
          hor2.setValue(slider2.getValue());
        }
      });
    panel.add(slider2);

    panel.add(Box.createVerticalStrut(5));

    // Indeterminate progress bar.
    final JProgressBar hor3 = new JProgressBar(JProgressBar.HORIZONTAL, 0, 100);
    hor3.setIndeterminate(true);
    panel.add(hor3);

    panel.add(Box.createVerticalStrut(5));

    // Indeterminate progress bar with text.
    final JProgressBar hor4 = new JProgressBar(JProgressBar.HORIZONTAL, 0, 100);
    hor4.setIndeterminate(true);
    hor4.setString("Indeterminate ProgressBar");
    hor4.setStringPainted(true);
    panel.add(hor4);

    return panel;
  }

  private static JPanel createVerticalProgressBar()
  {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    final JProgressBar vert = new JProgressBar(JProgressBar.VERTICAL, 0, 100);
    panel.add(vert);
    final JSlider slider = new JSlider(JSlider.VERTICAL, 0, 100, 0);
    slider.addChangeListener(new ChangeListener()
      {
        public void stateChanged(ChangeEvent event)
        {
          vert.setValue(slider.getValue());
        }
      });
    panel.add(slider);

    panel.add(Box.createHorizontalStrut(5));

    final JProgressBar vert2 = new JProgressBar(JProgressBar.VERTICAL, 0, 100);
    vert2.setString("ProgressBar Demo");
    panel.add(vert2);
    vert2.setStringPainted(true);
    final JSlider slider2 = new JSlider(JSlider.VERTICAL, 0, 100, 0);
    slider2.addChangeListener(new ChangeListener()
      {
        public void stateChanged(ChangeEvent event)
        {
          vert2.setValue(slider2.getValue());
        }
      });
    panel.add(slider2);

    panel.add(Box.createHorizontalStrut(5));

    // Indeterminate progress bar.
    final JProgressBar vert3 = new JProgressBar(JProgressBar.VERTICAL, 0, 100);
    vert3.setIndeterminate(true);
    panel.add(vert3);

    panel.add(Box.createHorizontalStrut(5));

    // Indeterminate progress bar with text.
    final JProgressBar vert4 = new JProgressBar(JProgressBar.VERTICAL, 0, 100);
    vert4.setIndeterminate(true);
    vert4.setString("Indeterminate ProgressBar");
    vert4.setStringPainted(true);
    panel.add(vert4);
    return panel;
  }

  public void actionPerformed(ActionEvent event)
  {
    // TODO Auto-generated method stub
    
  }

  /**
   * The entry point when running as a standalone program.
   *
   * @param args command line arguments
   */
  public static void main(String[] args)
  {
    SwingUtilities.invokeLater(
      new Runnable()
        {
          public void run()
          {
            ProgressBarDemo app = new ProgressBarDemo("ProgressBar Demo");
            app.pack();
            app.setVisible(true);
          }
        });
  }
}
