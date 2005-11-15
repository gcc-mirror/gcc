/* ScrollBarDemo.java -- An example showing scroll bars in Swing.
   Copyright (C) 2005,  Free Software Foundation, Inc.

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
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollBar;

/**
 * A simple scroll bar demo showing various scroll bars in different states.
 */
public class ScrollBarDemo 
  extends JFrame 
  implements ActionListener 
{

  /**
   * Creates a new demo instance.
   * 
   * @param title  the frame title.
   */
  public ScrollBarDemo(String title) 
  {
    super(title);
    JPanel content = createContent();
    JPanel closePanel = new JPanel();
    JButton closeButton = new JButton("Close");
    closeButton.setActionCommand("CLOSE");
    closeButton.addActionListener(this);
    closePanel.add(closeButton);
    content.add(closePanel, BorderLayout.SOUTH);
    getContentPane().add(content);
  }
       
  /**
   * Returns a panel with the demo content.  The panel
   * uses a BorderLayout(), and the BorderLayout.SOUTH area
   * is empty, to allow callers to add controls to the 
   * bottom of the panel if they want to (a close button is
   * added if this demo is being run as a standalone demo).
   */       
  JPanel createContent() 
  {
    JPanel content = new JPanel(new BorderLayout());
    JPanel panel = createScrollBarPanel();
    content.add(panel);
    return content;        
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
    ScrollBarDemo app = new ScrollBarDemo("ScrollBar Demo");
    app.pack();
    app.setVisible(true);
  }

}
