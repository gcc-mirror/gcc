/* TabbedPaneDemo.java -- Demonstrates JTabbedPane
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


package gnu.classpath.examples.swing;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

public class TabbedPaneDemo
  extends JPanel
  implements ActionListener
{
  TabbedPaneDemo()
  {
    super();
    createContent();
  }

  private void createContent()
  {
    JPanel p = new JPanel();
    p.setLayout(new GridLayout(2, 2));
    JTabbedPane tabs1 = new JTabbedPane(SwingConstants.TOP);
    tabs1.add("Top Item 1", new JButton("Button"));
    tabs1.add("Top Item 2", new JButton("Button"));
    JTabbedPane tabs2 = new JTabbedPane(SwingConstants.LEFT);
    tabs2.add("Left Item 1", new JButton("Button"));
    tabs2.add("Left Item 2", new JButton("Button"));
    JTabbedPane tabs3 = new JTabbedPane(SwingConstants.BOTTOM);
    tabs3.add("Bottom Item 1", new JButton("Button"));
    tabs3.add("Bottom Item 2", new JButton("Button"));
    JTabbedPane tabs4 = new JTabbedPane(SwingConstants.RIGHT);
    tabs4.add("Right Item 1", new JButton("Button"));
    tabs4.add("Right Item 2", new JButton("Button"));
    p.add(tabs1);
    p.add(tabs2);
    p.add(tabs3);
    p.add(tabs4);
    setLayout(new BorderLayout());
    add(p, BorderLayout.CENTER);
  }

  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
  }

  /**
   * When the demo is run independently, the frame is displayed, so we should
   * initialise the content panel (including the demo content and a close 
   * button).  But when the demo is run as part of the Swing activity board,
   * only the demo content panel is used, the frame itself is never displayed,
   * so we can avoid this step.
   */
  void initFrameContent()
  {
    JPanel closePanel = new JPanel();
    JButton closeButton = new JButton("Close");
    closeButton.setActionCommand("CLOSE");
    closeButton.addActionListener(this);
    closePanel.add(closeButton);
    add(closePanel, BorderLayout.SOUTH);
  }

  public static void main(String[] args)
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         TabbedPaneDemo app = new TabbedPaneDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("TabbedPane Demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a TabbedPaneDemo.
   *
   * @return a DemoFactory that creates a TabbedPaneDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new TabbedPaneDemo();
      }
    };
  }
}
