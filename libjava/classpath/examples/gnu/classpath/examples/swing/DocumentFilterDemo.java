/* DpocumentFilterDemo.java -- An example for the DocumentFilter class.
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
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

/**
 * A demonstration of the <code>javax.swing.text.DocumentFilter</code> class.
 * 
 * <p>Similar to a dialog in a popular programming IDE the user can insert
 * a CVS URL into a textfield and the filter will split the components apart
 * and will put them into the right textfields saving the user a lot of
 * typing time.</p>
 * 
 * @author Robert Schuster
 */
public class DocumentFilterDemo 
  extends JPanel 
  implements ActionListener 
{
  JTextField target;
  
  JTextField host;
  JTextField repositoryPath;
  JTextField user;
  JTextField password;
  JComboBox connectionType;

  /**
   * Creates a new demo instance.
   */
  public DocumentFilterDemo() 
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
    
    JPanel panel = new JPanel(new GridLayout(7, 2));
    panel.add(new JLabel("CVS URL:"));
    panel.add(target = new JTextField(20));
    target.setBackground(Color.RED);
    
    panel.add(new JLabel("Host:"));
    panel.add(host = new JTextField(20));
    
    panel.add(new JLabel("Repository Path:"));
    panel.add(repositoryPath = new JTextField(20));
    
    panel.add(new JLabel("Username:"));
    panel.add(user = new JTextField(20));
    
    panel.add(new JLabel("Password:"));
    panel.add(password = new JTextField(20));
    
    panel.add(new JLabel("Connection Type:"));
    panel.add(connectionType = new JComboBox());
    
    JButton helpButton = new JButton("Help");
    panel.add(helpButton);
    
    helpButton.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent ae)
        {
          JOptionPane.showMessageDialog(DocumentFilterDemo.this,
                                        "Paste a CVS URL into the red " +
                                        "textfield.\nIf you do not want to " +
                                        "look up a CVS URL yourself click " +
                                        "on the 'provide me an example' " +
                                        "button.\nThis will paste a proper " +
                                        "string into your clipboard.");
        }
      });
    
    JButton exampleButton = new JButton("Provide me an example!");
    panel.add(exampleButton);
    exampleButton.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent ae)
        {
         try
           {
             Toolkit tk = Toolkit.getDefaultToolkit();
             Clipboard cb = tk.getSystemSelection();
             StringSelection selection
               = new StringSelection(":extssh:gnu@cvs.savannah.gnu.org:" +
                        "/cvs/example/project");
             
             cb.setContents(selection, selection);
             
             // Confirm success with a beep.
             tk.beep();
           }
         catch (IllegalStateException ise)
           {
             JOptionPane.showMessageDialog(DocumentFilterDemo.this,
                                           "Clipboard is currently" +
                                           " unavailable.",
                                           "Error",
                                           JOptionPane.ERROR_MESSAGE);
           }
        }
      });
    
    connectionType.addItem("pserver");
    connectionType.addItem("ext");
    connectionType.addItem("extssh");
    
    add(panel);
    
    AbstractDocument doc = (AbstractDocument) target.getDocument();
    doc.setDocumentFilter(new CVSFilter());
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
         DocumentFilterDemo app = new DocumentFilterDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("DocumentFilterDemo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
         frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a DocumentFilterDemo.
   *
   * @return a DemoFactory that creates a DocumentFilterDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new DocumentFilterDemo();
      }
    };
  }
  
  class CVSFilter extends DocumentFilter
  {
    // example: pserver:anonymous@cvs.sourceforge.net:/cvsroot/fmj
    String cvsPattern = ":?(pserver|ext|extssh):(\\w)+(:(\\w)+)?@(\\w|\\.)+:/(\\w|/)*";

    public void insertString(DocumentFilter.FilterBypass fb,
                             int offset, String string,
                             AttributeSet attr)
    throws BadLocationException
    {
      filterString(fb, offset, 0, string, attr, true);
    }
    
    public void replace(DocumentFilter.FilterBypass fb,
                              int offset, int length,
                              String string,
                              AttributeSet attr)
    throws BadLocationException
    {
      filterString(fb, offset, length, string, attr, false);
    }
    
    public void filterString(DocumentFilter.FilterBypass fb,
                             int offset, int length, String string,
                             AttributeSet attr, boolean insertion)
    throws BadLocationException
    {
      if(string.matches(cvsPattern))
        {
          // Split off the connection type part.
          String[] result = string.split(":", 2);

          // If the string contained a leading colon, result[0]
          // will be empty at that point. We simply repeat the split
          // operation on the remaining string and continue.
          if(result[0].equals(""))
            result = result[1].split(":", 2);
                
          connectionType.setSelectedItem(result[0]);
              
          // Split off the username and password part
          result = result[1].split("@", 2);
              
          // Break username and password in half
          String[] userCredentials = result[0].split(":");
          user.setText(userCredentials[0]);

          // If the result has two entries the second one will
          // be the password.
          if (userCredentials.length == 2)
            password.setText(userCredentials[1]);
              
          // Now break the host part apart.
          result = result[1].split(":");
              
          host.setText(result[0]);
              
          repositoryPath.setText(result[1]);
        }
          
      // The unmodified string is put into the document.
      if (insertion)
        fb.insertString(offset, string, attr);
      else
        fb.replace(offset, length, string, attr);
    }
    
  }
  
}
