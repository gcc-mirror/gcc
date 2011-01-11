/* ScrollBarDemo.java -- An example showing scroll bars in Swing.
   Copyright (C) 2005, 2006, Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
*/


package gnu.classpath.examples.swing;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.SwingUtilities;

/**
 * A simple scroll bar demo showing various scroll bars in different states.
 */
public class ScrollBarDemo
  extends JPanel
  implements ActionListener
{

  /**
   * Creates a new demo instance.
   */
  public ScrollBarDemo()
  {
    super();
    createContent();
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

  /**
   * Returns a panel with the demo content.  The panel
   * uses a BorderLayout(), and the BorderLayout.SOUTH area
   * is empty, to allow callers to add controls to the
   * bottom of the panel if they want to (a close button is
   * added if this demo is being run as a standalone demo).
   */
  private void createContent()
  {
    setLayout(new BorderLayout());
    JPanel panel = createScrollBarPanel();
    add(panel);
  }

  private JPanel createScrollBarPanel()
  {
    JPanel panel = new JPanel(new BorderLayout());

    JPanel horizontalPanel = new JPanel();

    JScrollBar scroll1a = new JScrollBar(JScrollBar.HORIZONTAL);
    JScrollBar scroll1b = new JScrollBar(JScrollBar.HORIZONTAL);
    scroll1b.setEnabled(false);
    JScrollBar scroll1c = new JScrollBar(JScrollBar.HORIZONTAL);
    scroll1c.putClientProperty("JScrollBar.isFreeStanding", Boolean.FALSE);
    JScrollBar scroll1d = new JScrollBar(JScrollBar.HORIZONTAL);
    scroll1d.putClientProperty("JScrollBar.isFreeStanding", Boolean.FALSE);
    scroll1d.setEnabled(false);
    horizontalPanel.add(scroll1a);
    horizontalPanel.add(scroll1b);
    horizontalPanel.add(scroll1c);
    horizontalPanel.add(scroll1d);

    panel.add(horizontalPanel, BorderLayout.NORTH);

    JPanel verticalPanel = new JPanel();
    verticalPanel.setLayout(new GridLayout(1, 7));

    JScrollBar scroll2a = new JScrollBar(JScrollBar.VERTICAL);
    JScrollBar scroll2b = new JScrollBar(JScrollBar.VERTICAL);
    scroll2b.setEnabled(false);
    JScrollBar scroll2c = new JScrollBar(JScrollBar.VERTICAL);
    scroll2c.putClientProperty("JScrollBar.isFreeStanding", Boolean.FALSE);
    JScrollBar scroll2d = new JScrollBar(JScrollBar.VERTICAL);
    scroll2d.setEnabled(false);
    scroll2d.putClientProperty("JScrollBar.isFreeStanding", Boolean.FALSE);

    verticalPanel.add(scroll2a);
    verticalPanel.add(new JPanel());
    verticalPanel.add(scroll2b);
    verticalPanel.add(new JPanel());
    verticalPanel.add(scroll2c);
    verticalPanel.add(new JPanel());
    verticalPanel.add(scroll2d);

    panel.add(verticalPanel, BorderLayout.EAST);

    JPanel centerPanel = new JPanel(new GridLayout(1, 2));
    centerPanel.add(new JScrollBar(JScrollBar.HORIZONTAL));
    centerPanel.add(new JScrollBar(JScrollBar.VERTICAL));
    panel.add(centerPanel);
    return panel;
  }

  public void actionPerformed(ActionEvent e)
  {
    if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
  }

  public static void main(String[] args)
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         ScrollBarDemo app = new ScrollBarDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("ScrollBar Demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }});
    }

  /**
   * Returns a DemoFactory that creates a ScrollBarDemo.
   *
   * @return a DemoFactory that creates a ScrollBarDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new ScrollBarDemo();
      }
    };
  }
}
