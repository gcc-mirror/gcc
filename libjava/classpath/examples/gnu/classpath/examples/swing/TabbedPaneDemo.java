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
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

public class TabbedPaneDemo
  extends JPanel
  implements ActionListener
{
  static Color[] colors = { Color.BLUE, Color.CYAN, Color.GRAY, Color.GREEN,
                            Color.MAGENTA, Color.ORANGE, Color.PINK,
                            Color.ORANGE, Color.RED, Color.BLUE, Color.YELLOW
                          };
  TabbedPaneDemo()
  {
    super();
    createContent();
  }

  private void createContent()
  {
    JPanel p = new JPanel();
    p.setLayout(new GridLayout(1, 1));

    int COUNT = 25;
    JTabbedPane tp = createTabbedPane(SwingConstants.TOP, "tab", COUNT);
    p.add(tp);

    final JPopupMenu popup = new JPopupMenu();

    JMenu menu = new JMenu("tab placement");
    menu.add(createPlacementChangingMenuItem("top",
                                             SwingConstants.TOP,
                                             tp));

    menu.add(createPlacementChangingMenuItem("bottom",
                                             SwingConstants.BOTTOM,
                                             tp));

    menu.add(createPlacementChangingMenuItem("left",
                                             SwingConstants.LEFT,
                                             tp));

    menu.add(createPlacementChangingMenuItem("right",
                                             SwingConstants.RIGHT,
                                             tp));
    popup.add(menu);

    menu = new JMenu("tab layout");
    menu.add(createLayoutPolicyChangingMenuItem("wrapping tabs",
                                                JTabbedPane.WRAP_TAB_LAYOUT,
                                                tp));

    menu.add(createLayoutPolicyChangingMenuItem("scrolling tabs",
                                                JTabbedPane.SCROLL_TAB_LAYOUT,
                                                tp));
    popup.add(menu);

    tp.addMouseListener(new MouseAdapter()
                       {
                         public void mousePressed(MouseEvent e) {
                           showPopup(e);
                         }

                         public void mouseReleased(MouseEvent e) {
                           showPopup(e);
                         }

                         void showPopup(MouseEvent e) {
                           if (e.isPopupTrigger()) {
                             popup.show(e.getComponent(), e.getX(), e.getY());
                           }
                         }
                       });

    setLayout(new BorderLayout());
    add(p, BorderLayout.CENTER);

  }

  private JMenuItem createPlacementChangingMenuItem(String t,
                                                    final int v,
                                                    final JTabbedPane dst)
  {
    JMenuItem item = new JMenuItem(t);

    item.addActionListener(new ActionListener()
                           {
                             public void actionPerformed(ActionEvent ae)
                             {
                              dst.setTabPlacement(v);
                             }
                           });

    return item;
  }

  private JMenuItem createLayoutPolicyChangingMenuItem(String t,
                                                       final int v,
                                                       final JTabbedPane dst)
  {
    JMenuItem item = new JMenuItem(t);

    item.addActionListener(new ActionListener()
                           {
                             public void actionPerformed(ActionEvent ae)
                             {
                              dst.setTabLayoutPolicy(v);
                             }
                           });

    return item;
  }

  private JTabbedPane createTabbedPane(int direction, String name, int count)
  {
    JTabbedPane pane = new JTabbedPane(direction);

    for(int i = 0; i< count; i++)
      {
        pane.addTab(name + " " + i, createTabContent(name + " " + i));
        if (Math.random() >= 0.75)
          pane.setEnabledAt(i, false);
      }

    return pane;
  }

  private JPanel createTabContent(String name)
  {
    JTextArea ta;
    JPanel panel = new JPanel();
    panel.add(new JLabel(name));
    panel.add(new JButton(name));
    panel.add(new JScrollPane(ta = new JTextArea(5, 5)));

    ta.setBackground(colors[(int) (Math.random() * colors.length)]);

    return panel;
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
