/* Demo.java -- Simple Java Print Service Demo
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


package gnu.classpath.examples.print;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.Iterator;

import javax.print.DocFlavor;
import javax.print.DocPrintJob;
import javax.print.PrintException;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
import javax.print.ServiceUI;
import javax.print.SimpleDoc;
import javax.print.attribute.Attribute;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;


/**
 * A simple demo showing the use of the Java Print Service API.
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class Demo extends JFrame 
  implements ActionListener
{
  // The discovered print services
  private static PrintService[] services;
  
  // variables for the PrintPanel demo
  private HashPrintRequestAttributeSet atts;
  private PrintService dialogSelectedService;
  private JTextField dialogSelectedService_Tf;
  private JList dialogSelectedServiceAtts;
  private JComboBox dialogSelectedServicedocFormat;
  private JTextField selectedFileTf;
  private File selectedFile;
  
  // variables for the PrintServicePanel demo
  private JComboBox serviceBox;
  private JList docFormat;
  private JList attCategories;
  
  static
  {
    // lookup all services without any constraints
    services = PrintServiceLookup.lookupPrintServices(null, null);   
  }
  
  /**
   * Constructs the Print Demo
   * @param title - the demo title.
   */
  public Demo(String title) 
  {
    super(title);
    JPanel content = new JPanel(new BorderLayout());
    
    JTabbedPane tabbed = new JTabbedPane();
    tabbed.addTab("Discover print services", createPrintServicePanel());
    tabbed.addTab("Print a file", createPrintPanel());
    
    JPanel closePanel = new JPanel();
    JButton closeButton = new JButton("Close");
    closeButton.setActionCommand("CLOSE");
    closeButton.addActionListener(this);
    closePanel.add(closeButton);
    
    content.add(tabbed, BorderLayout.CENTER);
    content.add(closePanel, BorderLayout.SOUTH);
    getContentPane().add(content);
  }
    
  private JPanel createPrintServicePanel() 
  {   
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints c = new GridBagConstraints();
    
    c.insets = new Insets(5,5,5,5);
    c.gridx = 0;
    c.gridy = 0;   
    c.anchor = GridBagConstraints.WEST;
    c.fill = GridBagConstraints.HORIZONTAL;
    JLabel serviceBoxLb = new JLabel("Available print services: ");
    panel.add(serviceBoxLb, c); 
    
    c.gridx = 1;
    c.gridy = 0;
    serviceBox = new JComboBox(services);
    serviceBox.setActionCommand("SERVICE");
    serviceBox.addActionListener(this);
    panel.add(serviceBox, c); 
    
    c.gridx = 0;
    c.gridy = 1;
    JLabel docFormatLb = new JLabel("Supported DocFormat: ");
    panel.add(docFormatLb, c); 
    
    c.gridx = 1;
    c.gridy = 1;
    docFormat = new JList(services[0].getSupportedDocFlavors());
    docFormat.setVisibleRowCount(3);
    JScrollPane scrollPane = new JScrollPane(docFormat);
    panel.add(scrollPane, c); 
    
    c.gridx = 0;
    c.gridy = 2;
    JLabel categoriesLb = new JLabel("Supported Attribute categories: ");
    panel.add(categoriesLb, c); 
    
    c.gridx = 1;
    c.gridy = 2;
    attCategories = new JList(services[0].getSupportedAttributeCategories());
    attCategories.setVisibleRowCount(3);
    JScrollPane scrollPane2 = new JScrollPane(attCategories);    
    panel.add(scrollPane2, c); 
    
    return panel;
  }
  
  private JPanel createPrintPanel() 
  {
    JPanel panel = new JPanel(new GridBagLayout());    
    GridBagConstraints c = new GridBagConstraints();
    
    c.insets = new Insets(5,5,5,5);
    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 2;    
    JButton serviceBtn = new JButton("Show print dialog ...");
    serviceBtn.addActionListener(this);
    panel.add(serviceBtn, c); 
    
    c.gridx = 0;
    c.gridy = 1;
    c.gridwidth = 1;
    c.anchor = GridBagConstraints.WEST;
    c.fill = GridBagConstraints.HORIZONTAL;
    JLabel selectedLb = new JLabel("Selected print service: ");
    panel.add(selectedLb, c);
    
    c.gridx = 1;
    c.gridy = 1;
    dialogSelectedService_Tf = new JTextField(25);
    panel.add(dialogSelectedService_Tf, c);
    
    c.gridx = 0;
    c.gridy = 2;
    JLabel selectedAttsLb = new JLabel("Selected Attributes: ");
    panel.add(selectedAttsLb, c);
    
    c.gridx = 1;
    c.gridy = 2;
    c.weighty = 1.5;
    c.fill = GridBagConstraints.BOTH;
    dialogSelectedServiceAtts = new JList();
    dialogSelectedServiceAtts.setVisibleRowCount(3);    
    JScrollPane scrollPane = new JScrollPane(dialogSelectedServiceAtts);
    panel.add(scrollPane, c);
    
    c.gridx = 0;
    c.gridy = 3;
    c.fill = GridBagConstraints.HORIZONTAL;
    JLabel fileLb = new JLabel("File to print: ");
    panel.add(fileLb, c);
    
    c.gridx = 1;
    c.gridy = 3;
    selectedFileTf = new JTextField(25);
    panel.add(selectedFileTf, c);
    
    c.gridx = 2;
    c.gridy = 3;
    c.fill = GridBagConstraints.NONE;
    JButton fileBt = new JButton("Choose file");
    fileBt.addActionListener(this);
    panel.add(fileBt, c);
    
    c.gridx = 0;
    c.gridy = 4;
    c.fill = GridBagConstraints.HORIZONTAL;
    JLabel docFormatLb = new JLabel("Document format of file: ");
    panel.add(docFormatLb, c);
    
    c.gridx = 1;
    c.gridy = 4;
    dialogSelectedServicedocFormat = new JComboBox();
    panel.add(dialogSelectedServicedocFormat, c);
    
    c.gridx = 0;
    c.gridy = 5;
    c.gridwidth = 2;
    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    JButton printBt = new JButton("Print");
    printBt.setActionCommand("PRINT");
    printBt.addActionListener(this);
    panel.add(printBt, c);
    
    return panel;
  }

  /**
   * Simple action control - only one listener
   * @param event
   */
  public void actionPerformed(ActionEvent event)
  {
    if (event.getActionCommand().equals("CLOSE"))
      {
        System.exit(0);
      } 
    else if (event.getActionCommand().equals("Choose file"))
      {
        JFileChooser chooser = new JFileChooser();
        chooser.showOpenDialog(this);
        
        selectedFile = chooser.getSelectedFile();
        
        if (selectedFile != null)
          selectedFileTf.setText(selectedFile.getName());
        else
          selectedFileTf.setText("None selected");
      }
    else if (event.getActionCommand().equals("Show print dialog ..."))
      {
        atts = new HashPrintRequestAttributeSet();        
        dialogSelectedService = ServiceUI.printDialog(null, 50, 50, services,
                                                      null, null, atts);
        
        if (dialogSelectedService != null)
          {
            dialogSelectedService_Tf.setText(dialogSelectedService.getName());
            
            // we do not want to have the class representation in the dialog
            // as we later always use an InputStream to open the file selected
            
            // use set to remove duplicates
            DocFlavor[] docflavors = dialogSelectedService.getSupportedDocFlavors();
            HashSet set = new HashSet();
            for (int i=0; i < docflavors.length; i++)
              {
                String charset = docflavors[i].getParameter("charset");
                String mimetype = docflavors[i].getMediaType() + "/" + docflavors[i].getMediaSubtype();
                if (charset != null)
                  mimetype += "; charset=" + charset;
                set.add(mimetype);
              }
            
            dialogSelectedServicedocFormat.removeAllItems();       
            for (Iterator it = set.iterator(); it.hasNext(); )
              dialogSelectedServicedocFormat.addItem(it.next());
          }
        else
          dialogSelectedService_Tf.setText("None selected");

        Attribute[] attsArray = atts.toArray();
        String[] attsSTr = new String[attsArray.length];
        for (int i = 0; i < attsSTr.length; i++)
          attsSTr[i] = attsArray[i].getName() + " - " + attsArray[i].toString();

        dialogSelectedServiceAtts.setListData(attsSTr);    
        
        validate();          
      } 
    else if (event.getActionCommand().equals("PRINT"))
      {  
        if (selectedFile != null && dialogSelectedService != null) 
          {
            DocPrintJob job = dialogSelectedService.createPrintJob();
            
            // choose correct docflavor
            String mimetype = (String) dialogSelectedServicedocFormat.getSelectedItem();
                        
            DocFlavor flavor = null;
            if (mimetype.equals(DocFlavor.INPUT_STREAM.GIF.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.GIF;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.AUTOSENSE.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.AUTOSENSE;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.JPEG.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.JPEG;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.PCL.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.PCL;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.PDF.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.PDF;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.PNG.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.PNG;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.POSTSCRIPT.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.POSTSCRIPT;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.TEXT_HTML_HOST.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.TEXT_HTML_HOST;
            else if (mimetype.equals(DocFlavor.INPUT_STREAM.TEXT_PLAIN_HOST.getMimeType()))
              flavor = DocFlavor.INPUT_STREAM.TEXT_PLAIN_HOST;
            else
              flavor = new DocFlavor(mimetype, "java.io.InputStream");
            
            try
              {
                SimpleDoc doc = new SimpleDoc(new FileInputStream(selectedFile), flavor, null);
                job.print(doc, atts);
              }
            catch (FileNotFoundException e)
              {
                JOptionPane.showMessageDialog(this, "The file was not found.");
                e.printStackTrace();
              }
            catch (PrintException e)
              {
                JOptionPane.showMessageDialog(this, e, "PrintException", JOptionPane.ERROR_MESSAGE);
                e.printStackTrace();
              }           
          }
        else
          JOptionPane.showMessageDialog(this, "Please select a file to print using the FileChooser", "No file selected", JOptionPane.INFORMATION_MESSAGE);
      }
    else if (event.getActionCommand().equals("SERVICE"))
      {  // A new service was selected
        PrintService selected = (PrintService) serviceBox.getSelectedItem();
        
        DocFlavor[] flavors = selected.getSupportedDocFlavors();
        docFormat.setListData(flavors);
        attCategories.setListData(selected.getSupportedAttributeCategories());
      }
  }

  /**
   * Main method.
   * @param args - nothing defined.
   */
  public static void main(String[] args)
  {
    Demo app = new Demo("GNU Classpath printing demo");
    app.pack();
    app.setVisible(true);
  }
}
