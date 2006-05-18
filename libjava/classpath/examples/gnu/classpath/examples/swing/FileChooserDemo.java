/* FileChooserDemo.java -- An example showing file choosers in Swing.
   Copyright (C) 2005, 2006,  Free Software Foundation, Inc.

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
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;

/**
 * A simple demo showing the {@link JFileChooser} component used in different
 * ways.
 */
public class FileChooserDemo
  extends JPanel
  implements ActionListener 
{
  /**
   * A file filter for Java source files.
   */
  static class JavaFileFilter extends FileFilter
  {
    public String getDescription() 
    {
      return "Java Source Files (.java)";
    }
    public boolean accept(File f)
    {
      if (f != null) 
      {
        return f.getName().endsWith(".java") || f.isDirectory();
      }
      else 
        return false;
    }
  }

  /** A label to display the selected file. */
  JLabel selectedFileLabel;
    
  /** 
   * A list showing the selected files (where multi selections are 
   * allowed). 
   */
  JList selectedFilesList;
    
  /** A label to display the return code for the JFileChooser. */
  JLabel returnCodeLabel;
    
  /**
   * Creates a new demo instance. 
   */
  public FileChooserDemo() 
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
        
    // create a panel of buttons to select the different styles of file 
    // chooser...
    JPanel buttonPanel = new JPanel(new GridLayout(5, 1));
    JButton openButton = new JButton("Open...");
    openButton.setActionCommand("OPEN");
    openButton.addActionListener(this);
    buttonPanel.add(openButton);
    JButton saveButton = new JButton("Save...");
    saveButton.setActionCommand("SAVE");
    saveButton.addActionListener(this);
    buttonPanel.add(saveButton);
    JButton queryButton = new JButton("Select Directory...");
    queryButton.setActionCommand("SELECT_DIRECTORY");
    queryButton.addActionListener(this);
    buttonPanel.add(queryButton);
    JButton openJavaButton = new JButton("Open Java file...");
    openJavaButton.setActionCommand("OPEN_JAVA");
    openJavaButton.addActionListener(this);
    buttonPanel.add(openJavaButton);
    JButton openMultiButton = new JButton("Open multiple files...");
    openMultiButton.setActionCommand("OPEN_MULTI");
    openMultiButton.addActionListener(this);
    buttonPanel.add(openMultiButton);
    add(buttonPanel, BorderLayout.WEST);
        
    // create a panel to display the selected file(s) and the return code
    JPanel displayPanel = new JPanel(new BorderLayout());
        
    selectedFileLabel = new JLabel("-");
    selectedFileLabel.setBorder(BorderFactory.createTitledBorder("Selected File/Directory: "));
    displayPanel.add(selectedFileLabel, BorderLayout.NORTH);
        
    selectedFilesList = new JList();
    JScrollPane sp = new JScrollPane(selectedFilesList);
    sp.setBorder(BorderFactory.createTitledBorder("Selected Files: "));
    displayPanel.add(sp);
        
    returnCodeLabel = new JLabel("0");
    returnCodeLabel.setBorder(BorderFactory.createTitledBorder("Return Code:"));
    displayPanel.add(returnCodeLabel, BorderLayout.SOUTH);
        
    add(displayPanel);
  }
    
  /**
   * When the user clicks on a button, launch the appropriate file chooser
   * and report the results.
   * 
   * @param e  the event.
   */
  public void actionPerformed(ActionEvent e)
  {
    int option = 0;
    File selectedFile = null;
    File[] selectedFiles = new File[0];
    
    if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
    else if (e.getActionCommand().equals("OPEN"))
      {
        JFileChooser chooser = new JFileChooser();
        option = chooser.showOpenDialog(this);
        selectedFile = chooser.getSelectedFile();
        selectedFiles = chooser.getSelectedFiles();
      }
    else if (e.getActionCommand().equals("OPEN_MULTI"))
      {
        JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(true);
        option = chooser.showOpenDialog(this);
        selectedFile = chooser.getSelectedFile();
        selectedFiles = chooser.getSelectedFiles();
      }
    else if (e.getActionCommand().equals("OPEN_JAVA"))
      {
        JFileChooser chooser = new JFileChooser();
        chooser.setAcceptAllFileFilterUsed(false);
        chooser.setFileFilter(new JavaFileFilter());
        option = chooser.showOpenDialog(this);
        selectedFile = chooser.getSelectedFile();
        selectedFiles = chooser.getSelectedFiles();
      }
    else if (e.getActionCommand().equals("SAVE"))
      {
        JFileChooser chooser = new JFileChooser();
        option = chooser.showSaveDialog(this);
        selectedFile = chooser.getSelectedFile();
        selectedFiles = chooser.getSelectedFiles();
      }
    else if (e.getActionCommand().equals("SELECT_DIRECTORY"))
      {
        JFileChooser chooser = new JFileChooser();
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        option = chooser.showDialog(this, "Select");
        selectedFile = chooser.getSelectedFile();
        selectedFiles = chooser.getSelectedFiles();
      }
     
    // display the selection and return code
    if (selectedFile != null)
      selectedFileLabel.setText(selectedFile.toString());
    else
      selectedFileLabel.setText("null");
    DefaultListModel listModel = new DefaultListModel();
    for (int i = 0; i < selectedFiles.length; i++)
      listModel.addElement(selectedFiles[i]);
    selectedFilesList.setModel(listModel);
    returnCodeLabel.setText(Integer.toString(option));
  }
    
  public static void main(String[] args) 
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         FileChooserDemo app = new FileChooserDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("FileChooser Demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a FileChooserDemo.
   *
   * @return a DemoFactory that creates a FileChooserDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new FileChooserDemo();
      }
    };
  }
}
