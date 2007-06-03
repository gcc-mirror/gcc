/* NavigationFilterDemo.java -- An example for the NavigationFilter class.
   Copyright (C) 2006  Free Software Foundation, Inc.

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
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import javax.swing.text.NavigationFilter;
import javax.swing.text.Position;
import javax.swing.text.Utilities;

/**
 * A demonstration of the <code>javax.swing.text.NavigationFilter</code> class.
 * 
 * <p>It shows a NavigationFilter which lets you walk word-wise
 * through a text.</p>
 * 
 * @author Robert Schuster
 */
public class NavigationFilterDemo 
  extends JPanel 
  implements ActionListener 
{
  
  JTextArea textArea;

  /**
   * Creates a new demo instance.
   */
  public NavigationFilterDemo() 
  {
    createContent();
    // initFrameContent() is only called (from main) when running this app 
    // standalone
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

  private void createContent()
  {
    setLayout(new BorderLayout());
    
    add(textArea = new JTextArea(10, 20));
    
    textArea.setWrapStyleWord(true);
    
    textArea.setLineWrap(true);
        
    textArea.setNavigationFilter(new WordFilter());
    
    textArea.setText("GNU Classpath, Essential Libraries for Java, " +
                     "is a GNU project to create free core class " +
                     "libraries for use with virtual machines and " +
                     "compilers for the java programming language.");
  }
  
  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("CLOSE"))
      System.exit(0);
    
  } 

  public static void main(String[] args) 
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         NavigationFilterDemo app = new NavigationFilterDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("NavigationFilterDemo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
         frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a NavigationFilterDemo.
   *
   * @return a DemoFactory that creates a NavigationFilterDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new NavigationFilterDemo();
      }
    };
  }
  
  class WordFilter extends NavigationFilter
  {
    public int getNextVisualPositionFrom(JTextComponent text,
                                         int pos,
                                         Position.Bias bias,
                                         int direction,
                                         Position.Bias[] biasRet)
                                  throws BadLocationException
    {
      Point pt;
      
      int newpos = pos;
      switch (direction)
      {
        case SwingConstants.NORTH:
          // Find out where the caret want to be positioned ideally.
          pt = text.getCaret().getMagicCaretPosition();
          
          // Calculate its position above.
          newpos = Utilities.getPositionAbove(text, pos, (pt != null) ? pt.x : 0);

          // If we have a valid position, then calculate the next word start
          // from there.
          if (newpos != -1)
            return Utilities.getWordStart(text, newpos);
          else
            return pos;
        case SwingConstants.SOUTH:
          // Find out where the caret want to be positioned ideally.
          pt = text.getCaret().getMagicCaretPosition();

          // Calculate its position below.
          newpos = Utilities.getPositionBelow(text, pos, (pt != null) ? pt.x : 0);

          // If we have a valid position, then calculate the next word start
          // from there.
          if (newpos != -1)
            return Utilities.getWordStart(text, newpos);
          else
            return pos;
        case SwingConstants.WEST:
          // Calculate the next word start.
          newpos = Utilities.getWordStart(text, newpos);
          
          // If that means that the caret will not move, return
          // the start of the previous word.
          if (newpos != pos)
            return newpos;
          else
            return Utilities.getPreviousWord(text, newpos);
        case SwingConstants.EAST:
          return Utilities.getNextWord(text, newpos);
        default:
          // Do whatever the super implementation did.
          return super.getNextVisualPositionFrom(text, pos, bias,
                                                 direction, biasRet);
      }
    }
    
  }
  
}
