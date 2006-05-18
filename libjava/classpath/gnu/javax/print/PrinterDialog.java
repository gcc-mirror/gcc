/* PrinterDialog.java --
   Copyright (C)  2006  Free Software Foundation, Inc.

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

package gnu.javax.print;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.HeadlessException;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.print.DocFlavor;
import javax.print.PrintService;
import javax.print.attribute.Attribute;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.Chromaticity;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.Destination;
import javax.print.attribute.standard.JobName;
import javax.print.attribute.standard.JobPriority;
import javax.print.attribute.standard.JobSheets;
import javax.print.attribute.standard.Media;
import javax.print.attribute.standard.MediaPrintableArea;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.PageRanges;
import javax.print.attribute.standard.PrintQuality;
import javax.print.attribute.standard.PrinterInfo;
import javax.print.attribute.standard.PrinterIsAcceptingJobs;
import javax.print.attribute.standard.PrinterMakeAndModel;
import javax.print.attribute.standard.PrinterState;
import javax.print.attribute.standard.RequestingUserName;
import javax.print.attribute.standard.SheetCollate;
import javax.print.attribute.standard.Sides;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * Implementation of the PrinterDialog used by
 * {@link javax.print.ServiceUI} for visual selection
 * of print services and its attributes.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PrinterDialog extends JDialog implements ActionListener
{
  
  /**
   * The General Panel used in the printing dialog.
   * @author Wolfgang Baer (WBaer@gmx.de)
   */
  final class GeneralPanel extends JPanel
  {
    /**
     * Handles the copies attribute.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class CopiesAndSorted extends JPanel 
      implements ChangeListener, ActionListener
    {      
      private JCheckBox sort;    
      private JSpinner copies;
      private JLabel copies_lb;
      private SpinnerNumberModel copiesModel;
    
      CopiesAndSorted()
      {
        copies_lb = new JLabel(getLocalizedString("lb.copies"));        
        sort = new JCheckBox(getLocalizedString("cb.sort"));
        sort.addActionListener(this);
    
        copiesModel = new SpinnerNumberModel(1, 1, 9999, 1);
        copies = new JSpinner(copiesModel);
        copies.addChangeListener(this);
        
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(5, 5, 5, 5);
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.copies")));
    
        c.anchor = GridBagConstraints.WEST;
    
        c.gridx = 0;
        c.gridy = 0;
        add(copies_lb, c);
    
        c.gridx = 1;
        c.gridy = 0;
        add(copies, c);
    
        c.gridx = 0;
        c.gridy = 1;
        add(sort, c);
      }
      
      // copies jspinner state
      public void stateChanged(ChangeEvent event)
      {
        int value = ((Integer) copies.getValue()).intValue();
        atts.add(new Copies(value));
            
        if (value > 1 && categorySupported(SheetCollate.class))
          sort.setEnabled(true);
        else
          sort.setEnabled(false);                
      }

      // sorted checkbox state
      public void actionPerformed(ActionEvent event)
      {
        if (sort.isSelected())
          atts.add(SheetCollate.COLLATED);
      }

      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {        
        if (categorySupported(Copies.class))
          {
            copies.setEnabled(true);
            copies_lb.setEnabled(true);
            
            Copies copies = (Copies) attribute(Copies.class);
            if (copies != null)
              copiesModel.setValue(new Integer(copies.getValue()));
            
            if (((Integer)copiesModel.getValue()).intValue() > 1            
                && categorySupported(SheetCollate.class))
              {
                sort.setEnabled(true);
                Attribute collate = attribute(SheetCollate.class);
                if (collate != null && collate.equals(SheetCollate.COLLATED))
                  sort.setSelected(true); 
              }
            else
              sort.setEnabled(false);
          }
        else
          {
            copies.setEnabled(false);            
            copies_lb.setEnabled(false);
          }
      }
    }

    /**
     * Handles the print ranges attribute.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class PrintRange extends JPanel 
      implements ActionListener, FocusListener
    {    
      private JLabel to;    
      private JRadioButton all_rb, pages_rb;   
      private JTextField from_tf, to_tf;
          
      PrintRange()
      {        
        to = new JLabel(getLocalizedString("lb.to"));
        to.setEnabled(false);
    
        all_rb = new JRadioButton(getLocalizedString("rbt.all"));
        all_rb.setSelected(true);
        all_rb.setActionCommand("ALL");
        all_rb.addActionListener(this);
        pages_rb = new JRadioButton(getLocalizedString("rbt.pages"));
        pages_rb.setActionCommand("PAGES");
        pages_rb.setEnabled(false);
        pages_rb.addActionListener(this);
    
        ButtonGroup group = new ButtonGroup();
        group.add(all_rb);
        group.add(pages_rb);
    
        from_tf = new JTextField("1", 4);
        from_tf.setEnabled(false);
        from_tf.addFocusListener(this);
        to_tf = new JTextField("1", 4);
        to_tf.setEnabled(false);
        to_tf.addFocusListener(this);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.printrange")));
    
        c.insets = new Insets(15, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 0;
        add(all_rb, c);
    
        c.insets = new Insets(5, 5, 15, 5);
        c.gridx = 0;
        c.gridy = 1;
        add(pages_rb, c);
    
        c.gridx = 1;
        c.gridy = 1;
        add(from_tf, c);
    
        c.gridx = 2;
        c.gridy = 1;
        add(to, c);
    
        c.insets = new Insets(5, 5, 15, 15);
        c.gridx = 3;
        c.gridy = 1;
        add(to_tf, c);
      }
            
      // focus pagerange
      public void focusGained(FocusEvent event)
      {
        updatePageRanges();
      }
  
      public void focusLost(FocusEvent event)
      {
        updatePageRanges();
      }
      
      // updates the range after user changed it
      private void updatePageRanges()
      {
        int lower = Integer.parseInt(from_tf.getText());
        int upper = Integer.parseInt(to_tf.getText());
        
        if (lower > upper)
          {
            upper = lower;
            to_tf.setText("" + lower);                
          }
        
        PageRanges range = new PageRanges(lower, upper);
        atts.add(range);
      }

      // page range change
      public void actionPerformed(ActionEvent e)
      { 
        // if ALL is selected we must use a full-range object
        if (e.getActionCommand().equals("ALL"))
          {
            from_tf.setEnabled(false);
            to.setEnabled(false);
            to_tf.setEnabled(false);
            
            atts.add(new PageRanges(1, Integer.MAX_VALUE));
          }
        else
          {
            from_tf.setEnabled(true);
            to.setEnabled(true);
            to_tf.setEnabled(true);
            all_rb.setSelected(false);
          }       
      }
    
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        if (categorySupported(PageRanges.class))
          {
            pages_rb.setEnabled(true);
            PageRanges range = (PageRanges) attribute(PageRanges.class);
            if (range != null)
              {
                from_tf.setEnabled(true);
                to.setEnabled(true);
                to_tf.setEnabled(true);   
                all_rb.setSelected(false);
                pages_rb.setSelected(true);
                
                int[][] members = range.getMembers();
                // Although passed in attributes may contain more than one 
                // range we only take the first one
                from_tf.setText("" + members[0][0]);
                to_tf.setText("" + members[0][1]);
              }
          }
        else
          {
            from_tf.setEnabled(false);
            to.setEnabled(false);
            to_tf.setEnabled(false);
            all_rb.setSelected(true);
          }
       }
    }

    /**
     * Handles the selection of the print services
     * and its location and description attributes.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class PrintServices extends JPanel 
      implements ActionListener
    {    
      private JLabel name, status, typ, info;
      private JLabel statusValue, typValue, infoValue;   
      private JButton attributes;    
      private JComboBox services_cob;    
      private JCheckBox fileRedirection_cb;
    
      PrintServices()
      {
        name = new JLabel(getLocalizedString("lb.name"));
        status = new JLabel(getLocalizedString("lb.status"));
        typ = new JLabel(getLocalizedString("lb.typ"));
        info = new JLabel(getLocalizedString("lb.info"));
        typValue = new JLabel();
        infoValue = new JLabel();
        statusValue = new JLabel();
    
        attributes = new JButton(getLocalizedString("bt.attributes"));
        attributes.setEnabled(false);
        attributes.setActionCommand("ATTRIBUTES");
        attributes.addActionListener(this);
    
        services_cob = new JComboBox(getPrintServices());
        services_cob.setActionCommand("SERVICE");
        services_cob.addActionListener(this);
    
        fileRedirection_cb = new JCheckBox(getLocalizedString("cb.output"));
        fileRedirection_cb.setEnabled(false);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.printservice")));
    
        c.insets = new Insets(5, 5, 5, 5);
        c.anchor = GridBagConstraints.LINE_END;
        c.gridx = 0;
        c.gridy = 0;
        add(name, c);
    
        c.gridx = 0;
        c.gridy = 1;
        add(status, c);
    
        c.gridx = 0;
        c.gridy = 2;
        add(typ, c);
    
        c.gridx = 0;
        c.gridy = 3;
        add(info, c);
    
        c.gridx = 2;
        c.gridy = 3;
        c.weightx = 1;
        add(fileRedirection_cb, c);
    
        c.anchor = GridBagConstraints.LINE_START;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 1;
        c.gridy = 0;
        c.weightx = 1.5;
        add(services_cob, c);
    
        c.gridx = 1;
        c.gridy = 1;
        c.gridwidth = 2;
        c.weightx = 1;
        add(statusValue, c);
        
        c.gridx = 1;
        c.gridy = 2;
        c.gridwidth = 2;
        c.weightx = 1;
        add(typValue, c);
    
        c.gridx = 1;
        c.gridy = 3;
        c.gridwidth = 2;
        c.weightx = 1;
        add(infoValue, c);
        
        c.gridx = 2;
        c.gridy = 0;
        c.weightx = 1.5;
        add(attributes, c);
      }
    
      public void actionPerformed(ActionEvent e)
      {
        if (e.getActionCommand().equals("SERVICE"))
          {
            setSelectedPrintService((PrintService) services_cob.getSelectedItem());
            updateAll();
          }
        else if (e.getActionCommand().equals("ATTRIBUTES"))
          {
            // TODO LowPriority-Enhancement: As tests have shown this button 
            // is even gray and not enabled under Windows - Its a good place
            // to provide a classpath specific browsing dialog for all 
            // attributes not in the default printing dialog. 
          }
      }    
    
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        PrinterMakeAndModel att1 = (PrinterMakeAndModel)
          getSelectedPrintService().getAttribute(PrinterMakeAndModel.class);
        typValue.setText(att1 == null ? "" : att1.getValue());
        
        PrinterInfo att2 = (PrinterInfo) 
          getSelectedPrintService().getAttribute(PrinterInfo.class);
        infoValue.setText(att2 == null ? "" : att2.getValue());
        
        PrinterIsAcceptingJobs att3 = (PrinterIsAcceptingJobs)
          getSelectedPrintService().getAttribute(PrinterIsAcceptingJobs.class);
        PrinterState att4 = (PrinterState)
          getSelectedPrintService().getAttribute(PrinterState.class);
        
        String status = att4.toString();  
        if (att3 == PrinterIsAcceptingJobs.ACCEPTING_JOBS)
          status += " - " + getLocalizedString("lb.acceptingjobs");
        else if (att3 == PrinterIsAcceptingJobs.NOT_ACCEPTING_JOBS)
          status += " - " + getLocalizedString("lb.notacceptingjobs");
        
        statusValue.setText(status);
        
        if (categorySupported(Destination.class))
          {
            fileRedirection_cb.setEnabled(false);
          }
      }
      
    }

    private PrintServices printserv_panel;
    private PrintRange printrange_panel;
    private CopiesAndSorted copies;

    /**
     * Constructs the General Panel.
     */
    public GeneralPanel()
    {     
      setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

      printserv_panel = new PrintServices();
      printrange_panel = new PrintRange();
      copies = new CopiesAndSorted();

      JPanel layout_panel = new JPanel();
      layout_panel.setLayout(new BoxLayout(layout_panel, BoxLayout.LINE_AXIS));
      layout_panel.add(printrange_panel);
      layout_panel.add(Box.createRigidArea(new Dimension(10, 0)));
      layout_panel.add(copies);

      setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
      add(printserv_panel);
      add(Box.createRigidArea(new Dimension(0, 12)));
      add(layout_panel);
    }
    
    /**
     * Calls update on all internal panels to adjust
     * for a new selected print service.
     */
    void update()
    {
      printserv_panel.updateForSelectedService();
      printrange_panel.updateForSelectedService();
      copies.updateForSelectedService();
    }
  }

  /**
   * The Page setup Panel.
   * @author Wolfgang Baer (WBaer@gmx.de)
   */
  final class PageSetupPanel extends JPanel
  {
    /**
     * Handles the orientation attribute.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class Orientation extends JPanel implements ActionListener
    {
      private JRadioButton portrait, landscape, rev_portrait, rev_landscape;
      
      Orientation()
      {
        portrait = new JRadioButton(getLocalizedString("rbt.portrait"));
        portrait.addActionListener(this);
        landscape = new JRadioButton(getLocalizedString("rbt.landscape"));
        landscape.addActionListener(this);
        rev_portrait = new JRadioButton(getLocalizedString("rbt.revportrait"));
        rev_portrait.addActionListener(this);
        rev_landscape = new JRadioButton(getLocalizedString("rbt.revlandscape"));
        rev_landscape.addActionListener(this);
    
        ButtonGroup group = new ButtonGroup();
        group.add(portrait);
        group.add(landscape);
        group.add(rev_portrait);
        group.add(rev_landscape);
          
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.orientation")));
    
        c.insets = new Insets(5, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 0;
        add(portrait, c);
    
        c.gridx = 0;
        c.gridy = 1;
        add(landscape, c);
    
        c.gridx = 0;
        c.gridy = 2;
        add(rev_portrait, c);
    
        c.gridx = 0;
        c.gridy = 3;
        add(rev_landscape, c);
      }
    
      // event handling orientation
      public void actionPerformed(ActionEvent e)
      {
        if (e.getSource() == portrait)
          atts.add(OrientationRequested.PORTRAIT);
        else if (e.getSource() == landscape)
          atts.add(OrientationRequested.LANDSCAPE);
        else if (e.getSource() == rev_portrait)
          atts.add(OrientationRequested.REVERSE_PORTRAIT);
        else
          atts.add(OrientationRequested.REVERSE_LANDSCAPE);      
      }
    
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        if (categorySupported(OrientationRequested.class))
          {
            portrait.setEnabled(true);
            landscape.setEnabled(true);
            rev_landscape.setEnabled(true);
            rev_portrait.setEnabled(true);
            
            Attribute orientation = attribute(OrientationRequested.class);
            if (orientation != null)
              {
                if (orientation.equals(OrientationRequested.LANDSCAPE))
                  landscape.setSelected(true);
                else if (orientation.equals(OrientationRequested.PORTRAIT))
                  portrait.setSelected(true);
                else if (orientation.equals(OrientationRequested.REVERSE_PORTRAIT))
                  rev_portrait.setSelected(true);
                else 
                  rev_landscape.setSelected(true);
              }
            else
              {
                Object defaultValue = defaultValue(OrientationRequested.class);
                if (defaultValue.equals(OrientationRequested.LANDSCAPE))
                  landscape.setSelected(true);
                else if (defaultValue.equals(OrientationRequested.PORTRAIT))
                  portrait.setSelected(true);
                else if (defaultValue.equals(OrientationRequested.REVERSE_PORTRAIT))
                  rev_portrait.setSelected(true);
                else 
                  rev_landscape.setSelected(true);
              }
          }
        else
          {
            portrait.setEnabled(false);
            landscape.setEnabled(false);
            rev_landscape.setEnabled(false);
            rev_portrait.setEnabled(false);
          }       
      }
    }

    /**
     * Handles the media attribute.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class MediaTypes extends JPanel implements ActionListener
    {
      private JLabel size_lb, source_lb;
      private JComboBox size, source;
    
      MediaTypes()
      {
        size_lb = new JLabel(getLocalizedString("lb.size"));
        source_lb = new JLabel(getLocalizedString("lb.source"));
    
        size = new JComboBox();
        size.setEditable(false);
        size.addActionListener(this);
        source = new JComboBox();
        source.setEditable(false);
        size.addActionListener(this);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.medias")));
    
        c.insets = new Insets(5, 5, 5, 5);
        c.anchor = GridBagConstraints.LINE_END;
        c.gridx = 0;
        c.gridy = 0;
        add(size_lb, c);
    
        c.gridx = 0;
        c.gridy = 1;
        add(source_lb, c);
    
        c.anchor = GridBagConstraints.LINE_START;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 1;
        c.gridy = 0;
        c.weightx = 1.5;
        add(size, c);
    
        c.gridx = 1;
        c.gridy = 1;
        c.weightx = 1.5;
        add(source, c);
      }
    
      public void actionPerformed(ActionEvent event)
      {        
        if (event.getSource() == size)
          {
            Object obj = size.getSelectedItem();
            if (obj instanceof Media)
              atts.add((Media) obj);    
          }
        
        // we ignore source events currently
        // as only the automatic selection is used.       
      }
    
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      { 
        if (categorySupported(Media.class))
          {
            Media[] medias = (Media[]) getSelectedPrintService()
              .getSupportedAttributeValues(Media.class, flavor, null);
            
            size.removeAllItems();
            if (medias.length == 0)
              size.addItem(getLocalizedString("lb.automatically"));     
            else
              for (int i=0; i < medias.length; i++)
                size.addItem(medias[i]);
            
            Media media = (Media) attribute(Media.class);
            if (media != null)
              size.setSelectedItem(media);
            
            // this is currently ignored
            source.removeAllItems();
            source.addItem(getLocalizedString("lb.automatically"));
          }
        else
          {
            size.removeAllItems();
            source.removeAllItems();
            
            size.addItem(getLocalizedString("lb.automatically"));
            source.addItem(getLocalizedString("lb.automatically"));
          }
       }
    }

    /**
     * Handles the media printable area attribute.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class Margins extends JPanel implements FocusListener
    {
      private JLabel left, right, top, bottom;
      private JTextField left_tf, right_tf, top_tf, bottom_tf;
    
      Margins()
      {
        left = new JLabel(getLocalizedString("lb.left"));
        right = new JLabel(getLocalizedString("lb.right"));
        top = new JLabel(getLocalizedString("lb.top"));
        bottom = new JLabel(getLocalizedString("lb.bottom"));
    
        left_tf = new JTextField(7);
        left_tf.addFocusListener(this);
        right_tf = new JTextField(7);
        right_tf.addFocusListener(this);
        top_tf = new JTextField(7);
        top_tf.addFocusListener(this);
        bottom_tf = new JTextField(7);
        bottom_tf.addFocusListener(this);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.margins")));
    
        c.insets = new Insets(5, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 0;
        add(left, c);
    
        c.gridx = 1;
        c.gridy = 0;
        add(right, c);
    
        c.insets = new Insets(5, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 1;
        add(left_tf, c);
    
        c.gridx = 1;
        c.gridy = 1;
        add(right_tf, c);
    
        c.insets = new Insets(10, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 2;
        add(top, c);
    
        c.gridx = 1;
        c.gridy = 2;
        add(bottom, c);
    
        c.insets = new Insets(0, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 3;
        add(top_tf, c);
    
        c.gridx = 1;
        c.gridy = 3;
        add(bottom_tf, c);
      }
      
      public void focusGained(FocusEvent event)
      {
        updateMargins();
      }
  
      public void focusLost(FocusEvent event)
      {
        updateMargins();
      }
      
      // updates the margins after user changed it
      private void updateMargins()
      {
        // We currently do not support this attribute
        // as it is not in the IPP spec and therefore not in CUPS
      }
      
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        if (categorySupported(MediaPrintableArea.class))
          {
            left.setEnabled(true);
            right.setEnabled(true);
            top.setEnabled(true);
            bottom.setEnabled(true);
            left_tf.setEnabled(true);
            right_tf.setEnabled(true);
            top_tf.setEnabled(true);
            bottom_tf.setEnabled(true);           
          }
        else
          {
            left.setEnabled(false);
            right.setEnabled(false);
            top.setEnabled(false);
            bottom.setEnabled(false);
            left_tf.setEnabled(false);
            right_tf.setEnabled(false);
            top_tf.setEnabled(false);
            bottom_tf.setEnabled(false); 
          }     
       }
    }

    private MediaTypes media_panel;
    private Orientation orientation_panel;
    private Margins margins_panel;

    /** 
     * Constructs the page setup user interface. 
     */
    public PageSetupPanel()
    {
      setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

      media_panel = new MediaTypes();
      orientation_panel = new Orientation();
      margins_panel = new Margins();

      JPanel layout_panel = new JPanel();
      layout_panel.setLayout(new BoxLayout(layout_panel, BoxLayout.LINE_AXIS));
      layout_panel.add(orientation_panel);
      layout_panel.add(Box.createRigidArea(new Dimension(10, 0)));
      layout_panel.add(margins_panel);

      setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
      add(media_panel);
      add(Box.createRigidArea(new Dimension(0, 12)));
      add(layout_panel);
    }
    
    /**
     * Calls update on all internal panels to adjust
     * for a new selected print service.
     */
    void update()
    {
      media_panel.updateForSelectedService();
      orientation_panel.updateForSelectedService();
      margins_panel.updateForSelectedService();
    }
  }

  /**
   * The Appearance panel for quality, color etc.
   * @author Wolfgang Baer (WBaer@gmx.de)
   */
  final class AppearancePanel extends JPanel
  {
    /**
     * Handles the print quality attribute.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class Quality extends JPanel implements ActionListener
    {
      private JRadioButton low, normal, high;
      private ButtonGroup group;
    
      Quality()
      {
        low = new JRadioButton(getLocalizedString("rbt.low"));
        low.addActionListener(this);
        normal = new JRadioButton(getLocalizedString("rbt.normal"));
        normal.addActionListener(this);
        high = new JRadioButton(getLocalizedString("rbt.high"));
        high.addActionListener(this);
    
        group = new ButtonGroup();
        group.add(low);
        group.add(normal);
        group.add(high);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.quality")));
    
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 0;
        add(low, c);
    
        c.gridx = 0;
        c.gridy = 1;
        add(normal, c);
    
        c.gridx = 0;
        c.gridy = 2;
        add(high, c);
      }
    
      public void actionPerformed(ActionEvent e)
      {
        if (e.getSource() == low)
          atts.add(PrintQuality.DRAFT);
        else if (e.getSource() == normal)
          atts.add(PrintQuality.NORMAL);
        else
          atts.add(PrintQuality.HIGH);   
      }
    
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        if (categorySupported(PrintQuality.class))
          {
            low.setEnabled(true);
            normal.setEnabled(true);
            high.setEnabled(true);
            
            Object defaultValue = defaultValue(PrintQuality.class);          
            Attribute quality = attribute(PrintQuality.class);
            
            if (quality != null)
              {
                if (quality.equals(PrintQuality.DRAFT))
                  low.setSelected(true);
                else if (quality.equals(PrintQuality.NORMAL))
                  normal.setSelected(true);
                else 
                  high.setSelected(true);
              }
            else
              {
                if (defaultValue.equals(PrintQuality.DRAFT))
                  low.setSelected(true);
                else if (defaultValue.equals(PrintQuality.NORMAL))
                  normal.setSelected(true);
                else 
                  high.setSelected(true);
              }              
          }
        else
          {
            low.setEnabled(false);
            normal.setEnabled(false);
            high.setEnabled(false);
          }       
      }
    }
  
    /**
     * Handles the job attributes as requesting username, jobname etc.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class JobAttributes extends JPanel
      implements ActionListener, ChangeListener, FocusListener
    {    
      private JLabel jobname, username, priority_lb;    
      private JTextField jobname_tf, username_tf;    
      private JCheckBox cover;    
      private JSpinner priority;
      private SpinnerNumberModel model;
    
      JobAttributes()
      {
        jobname = new JLabel(getLocalizedString("lb.jobname"));
        username = new JLabel(getLocalizedString("lb.username"));
        priority_lb = new JLabel(getLocalizedString("lb.priority"));
    
        cover = new JCheckBox(getLocalizedString("cb.cover"));
        cover.addActionListener(this);
    
        model = new SpinnerNumberModel(1, 1, 100, 1);
        priority = new JSpinner(model);
        priority.addChangeListener(this);
    
        jobname_tf = new JTextField();
        jobname_tf.addFocusListener(this);
        username_tf = new JTextField();
        username_tf.addFocusListener(this);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.jobattributes")));
    
        c.insets = new Insets(10, 5, 10, 5);
        c.gridx = 0;
        c.gridy = 0;
        add(cover, c);
    
        c.anchor = GridBagConstraints.LINE_END;
        c.gridx = 1;
        c.gridy = 0;
        c.weightx = 2;
        add(priority_lb, c);
    
        c.gridx = 2;
        c.gridy = 0;
        c.weightx = 0.5;
        add(priority, c);
    
        c.anchor = GridBagConstraints.LINE_END;
        c.gridx = 0;
        c.gridy = 1;
        add(jobname, c);
    
        c.gridx = 0;
        c.gridy = 2;
        add(username, c);
    
        c.anchor = GridBagConstraints.CENTER;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 1;
        c.gridy = 1;
        c.gridwidth = 2;
        c.weightx = 1.5;
        add(jobname_tf, c);
    
        c.insets = new Insets(10, 5, 15, 5);
        c.gridx = 1;
        c.gridy = 2;
        add(username_tf, c);
      }
      
      public void actionPerformed(ActionEvent event)
      {
        if (cover.isSelected())
          atts.add(JobSheets.STANDARD);
        else
          atts.add(JobSheets.NONE);
      }
      
      public void stateChanged(ChangeEvent event)
      {
        int value = ((Integer) priority.getValue()).intValue();
        atts.add(new JobPriority(value));  
      }
      
      public void focusGained(FocusEvent event)
      {        
        updateTextfields(event);
      }
  
      public void focusLost(FocusEvent event)
      {
        updateTextfields(event);
      }
      
      private void updateTextfields(FocusEvent event)
      {
        if (event.getSource() == jobname_tf)
            atts.add(new JobName(jobname_tf.getText(), null));
        else
            atts.add(new RequestingUserName(username_tf.getText(), null));
      }

      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        // JobPriority       
        if (categorySupported(JobPriority.class))
          {
            JobPriority prio = (JobPriority) attribute(JobPriority.class);
            JobPriority value = (JobPriority) defaultValue(JobPriority.class);         
            priority.setEnabled(true);
            if (prio != null)
              model.setValue(new Integer(prio.getValue()));
            else
              model.setValue(new Integer(value.getValue()));
          }         
        else
          priority.setEnabled(false);  
        
        // Requesting username
        if (categorySupported(RequestingUserName.class))
          {
            Attribute user = attribute(RequestingUserName.class);
            Object value = defaultValue(RequestingUserName.class);
            username.setEnabled(true);            
            if (user != null)
              username_tf.setText(user.toString());
            else
              username_tf.setText(value.toString());
          }
        else
          username.setEnabled(false);  
        
        // Job Name
        if (categorySupported(JobName.class))
          {
            Attribute job = attribute(JobName.class);
            Object value = defaultValue(JobName.class);
            jobname.setEnabled(true);          
            if (job != null)
              jobname_tf.setText(job.toString());
            else
              jobname_tf.setText(value.toString());
          }
        else
          jobname.setEnabled(false);  
        
        // Job sheets
        if (categorySupported(JobSheets.class))
          {
            Attribute sheet = attribute(JobSheets.class);
            Object value = defaultValue(JobSheets.class);
            cover.setEnabled(true);       
            if (sheet != null)
              {
                if (sheet.equals(JobSheets.NONE))
                  cover.setSelected(false);
                else 
                  cover.setSelected(true);
              }
            else
              {
                if (value.equals(JobSheets.NONE))
                  cover.setSelected(false);
                else
                  cover.setSelected(true);
              }
          }
        else
          cover.setEnabled(false);      
      }
    }
  
    /**
     * Handles the sides attributes.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class SidesPanel extends JPanel implements ActionListener
    {    
      private JRadioButton oneside, calendar, duplex;
    
      SidesPanel()
      { 
        oneside = new JRadioButton(getLocalizedString("rbt.onesided"));
        oneside.addActionListener(this);
        calendar = new JRadioButton(getLocalizedString("rbt.calendar"));
        calendar.addActionListener(this);
        duplex = new JRadioButton(getLocalizedString("rbt.duplex"));
        duplex.addActionListener(this);
    
        ButtonGroup group = new ButtonGroup();
        group.add(oneside);
        group.add(calendar);
        group.add(duplex);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.sides")));
    
        c.insets = new Insets(5, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 0;
        add(oneside, c);
    
        c.gridx = 0;
        c.gridy = 1;
        add(calendar, c);
    
        c.gridx = 0;
        c.gridy = 2;
        add(duplex, c);
      }
    
      public void actionPerformed(ActionEvent e)
      {
        if (e.getSource() == calendar)
          atts.add(Sides.TWO_SIDED_SHORT_EDGE);
        else if (e.getSource() == oneside)
          atts.add(Sides.ONE_SIDED);
        else
          atts.add(Sides.TWO_SIDED_LONG_EDGE);
      }
    
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        if (categorySupported(Sides.class))
          {
            oneside.setEnabled(true);
            calendar.setEnabled(true);
            duplex.setEnabled(true);
            
            Object defaultValue = defaultValue(Sides.class);           
            Attribute sides = attribute(Sides.class);
            if (sides != null)
              {
                if (sides.equals(Sides.TWO_SIDED_SHORT_EDGE))
                  calendar.setSelected(true);
                else if (sides.equals(Sides.ONE_SIDED))
                  oneside.setSelected(true);
                else
                  duplex.setSelected(true);
              }
            else
              {
                if (defaultValue.equals(Sides.TWO_SIDED_SHORT_EDGE))
                  calendar.setSelected(true);
                else if (defaultValue.equals(Sides.ONE_SIDED))
                  oneside.setSelected(true);
                else
                  duplex.setSelected(true);
              }
          }
        else
          {           
            oneside.setEnabled(false);
            calendar.setEnabled(false);
            duplex.setEnabled(false);
          }       
      }
    }
  
    /**
     * Handles the chromaticity attributes.
     * @author Wolfgang Baer (WBaer@gmx.de)
     */
    final class Color extends JPanel implements ActionListener
    {
      private JRadioButton bw, color;
    
      Color()
      {
        bw = new JRadioButton(getLocalizedString("rbt.blackwhite"));
        bw.addActionListener(this);
        color = new JRadioButton(getLocalizedString("rbt.color"));
        color.addActionListener(this);
    
        ButtonGroup group = new ButtonGroup();
        group.add(bw);
        group.add(color);
    
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
    
        setLayout(layout);
        setBorder(new TitledBorder(getLocalizedString("title.color")));
    
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 5, 5, 5);
        c.gridx = 0;
        c.gridy = 0;
        add(bw, c);
    
        c.gridx = 0;
        c.gridy = 1;
        add(color, c);
      }
    
      public void actionPerformed(ActionEvent e)
      {
        if (e.getSource() == bw)
          atts.add(Chromaticity.MONOCHROME);        
        else
          atts.add(Chromaticity.COLOR);
      }
    
      /**
       * Called to update for new selected
       * print service. Tests if currently
       * selected attributes are supported.
       */
      void updateForSelectedService()
      {
        if (categorySupported(Chromaticity.class))
          {
            bw.setEnabled(true);
            color.setEnabled(true);           
            
            Object defaultValue = defaultValue(Chromaticity.class);           
            Attribute chromaticity = attribute(Chromaticity.class);
            if (chromaticity != null)
              {
                if (chromaticity.equals(Chromaticity.MONOCHROME))
                  bw.setSelected(true);
                else 
                  color.setSelected(true);
              }
            else
              {
                if (defaultValue.equals(Chromaticity.MONOCHROME))
                  bw.setSelected(true);
                else               
                  color.setSelected(true);
              }
          }
        else
          {           
            bw.setEnabled(false);
            color.setEnabled(false);
          }
      }
    }
  
    private Quality quality_panel;
    private JobAttributes jobAttr_panel;
    private SidesPanel sides_panel;
    private Color chromaticy_panel;
  
    /**
     * Creates the panel for appearance attributes.
     */
    public AppearancePanel()
    {
      setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
  
      quality_panel = new Quality();
      jobAttr_panel = new JobAttributes();
      sides_panel = new SidesPanel();
      chromaticy_panel = new Color();
  
      JPanel layout_panel = new JPanel();
      layout_panel.setLayout(new BoxLayout(layout_panel, BoxLayout.LINE_AXIS));
      layout_panel.add(chromaticy_panel);
      layout_panel.add(Box.createRigidArea(new Dimension(10, 0)));
      layout_panel.add(quality_panel);
  
      JPanel layout2_panel = new JPanel();
      layout2_panel.setLayout(new BoxLayout(layout2_panel, BoxLayout.LINE_AXIS));
      layout2_panel.add(sides_panel);
      layout2_panel.add(Box.createRigidArea(new Dimension(10, 0)));
      layout2_panel.add(jobAttr_panel);
  
      setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
      add(layout_panel);
      add(Box.createRigidArea(new Dimension(0, 12)));
      add(layout2_panel);
    }
    
    /**
     * Calls update on all internal panels to adjust
     * for a new selected print service.
     */
    void update()
    {
      quality_panel.updateForSelectedService();
      jobAttr_panel.updateForSelectedService();
      sides_panel.updateForSelectedService();
      chromaticy_panel.updateForSelectedService();
    }
  }

  // on main contentpane
  private JButton ok_bt;
  private JButton cancel_bt;

  // the tabs
  private GeneralPanel general_panel;
  private PageSetupPanel pagesetup_panel;
  private AppearancePanel appearance_panel;
  
  private PrintService[] services;
  private PrintService defaultService;
  private PrintService selectedService;
  private DocFlavor flavor;
  private PrintRequestAttributeSet attributes;
  
  private boolean onlyPageDialog;  
  private PrintRequestAttributeSet atts; 
  
  private final static ResourceBundle messages;

  static
  {    
    messages = ResourceBundle.getBundle("gnu/javax/print/PrinterDialog");
  }
  
  // TODO LowPriority: Include checks so that if a specific value formerly
  // selected is no more supported by the new service changes to the default.
  
  /**
   * Class private constructs a printer dialog.
   * 
   * @param gc the screen to use. <code>null</code> is default screen.
   * @param services the print services to browse (not null).
   * @param defaultService the default service. If <code>null</code>
   * the first of the print services in the services array will be used.
   * @param flavor the flavours to be printed.
   * @param attributes the attributes requested. Will be updated 
   * by selections done by the user in the dialog.
   * @param onlyPageDialog if true a page settings only dialog is constructed.
   * 
   * @throws HeadlessException if GraphicsEnvironment is headless
   */
  private PrinterDialog(GraphicsConfiguration gc, PrintService[] services, 
    PrintService defaultService, DocFlavor flavor, 
    PrintRequestAttributeSet attributes, boolean onlyPageDialog, String title)
    throws HeadlessException
  {
    super((Frame)null, title, true, gc);
       
    setResizable(false);
    setDefaultCloseOperation(DISPOSE_ON_CLOSE);

    // check and remove service not supporting the flavor
    if (flavor != null)
      {
        ArrayList list = new ArrayList(services.length);
        for(int i=0; i < services.length; i++)
          if (services[i].isDocFlavorSupported(flavor))
            list.add(services[i]);
        
        if (defaultService != null
            && (! list.contains(defaultService)))
          defaultService = (PrintService) list.get(0);
        
        PrintService[] newServices = new PrintService[list.size()];
        this.services = (PrintService[]) list.toArray(newServices);
      }
    else
      this.services = services;
    
    if (defaultService == null)
      this.defaultService = services[0];
    else
      this.defaultService = defaultService;
    
    this.selectedService = this.defaultService;
    this.flavor = flavor;
    
    // the attributes given by the user
    this.attributes = attributes;
    // the one to work with during browsing
    this.atts = new HashPrintRequestAttributeSet(attributes);
    
    this.onlyPageDialog = onlyPageDialog;
    
    initUI(onlyPageDialog);    
    pack();
    updateAll();
  }
  
  /**
   * Constructs a page settings only dialog.
   * 
   * @param gc the screen to use. <code>null</code> is default screen.
   * @param service the print service for the page dialog.
   * the first of the print services in the services array will be used.
   * @param flavor the flavours to be printed.
   * @param attributes the attributes requested. Will be updated 
   * by selections done by the user in the dialog. 
   * 
   * @throws HeadlessException if GraphicsEnvironment is headless
   */
  public PrinterDialog(GraphicsConfiguration gc, PrintService service, 
    DocFlavor flavor, PrintRequestAttributeSet attributes)
    throws HeadlessException
  {
    this(gc, new PrintService[] {service}, service, flavor, attributes, 
         true, getLocalizedString("title.pagedialog"));  
  }
  
  /**
   * Constructs a printer dialog.
   * 
   * @param gc the screen to use. <code>null</code> is default screen.
   * @param services the print services to browse (not null).
   * @param defaultService the default service. If <code>null</code>
   * the first of the print services in the services array will be used.
   * @param flavor the flavours to be printed.
   * @param attributes the attributes requested. Will be updated 
   * by selections done by the user in the dialog. 
   * 
   * @throws HeadlessException if GraphicsEnvironment is headless
   */
  public PrinterDialog(GraphicsConfiguration gc, PrintService[] services, 
    PrintService defaultService, DocFlavor flavor, 
    PrintRequestAttributeSet attributes)
    throws HeadlessException
  {
    this(gc, services, defaultService, flavor, attributes, 
         false, getLocalizedString("title.printdialog"));
  }

  // initializes the gui parts
  private void initUI(boolean onlyPageDialog)
  { 
    JPanel buttonPane = new JPanel();
    
    if (onlyPageDialog)
      {
        JPanel pane = new JPanel();
        pane.setLayout(new BorderLayout());
        pagesetup_panel = new PageSetupPanel();
        pane.add(pagesetup_panel, BorderLayout.CENTER);
            
        ok_bt = new JButton(getLocalizedString("bt.OK"));
        ok_bt.addActionListener(this);
        cancel_bt = new JButton(getLocalizedString("bt.cancel"));
        cancel_bt.addActionListener(this);     
        
        getContentPane().add(pane, BorderLayout.CENTER);        
      }
    else
      {
        general_panel = new GeneralPanel();
        pagesetup_panel = new PageSetupPanel();
        appearance_panel = new AppearancePanel();

        JTabbedPane pane = new JTabbedPane();
        pane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        ok_bt = new JButton(getLocalizedString("bt.print"));
        ok_bt.addActionListener(this);
        cancel_bt = new JButton(getLocalizedString("bt.cancel"));
        cancel_bt.addActionListener(this);

        // populate jtabbedpane
        pane.addTab(getLocalizedString("tab.general"), general_panel);
        pane.addTab(getLocalizedString("tab.pagesetup"), pagesetup_panel);
        pane.addTab(getLocalizedString("tab.appearance"), appearance_panel);

        // Put everything together
        getContentPane().add(pane, BorderLayout.CENTER);
      }
    
    buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
    buttonPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    buttonPane.add(Box.createHorizontalGlue());
    buttonPane.add(ok_bt);
    buttonPane.add(Box.createRigidArea(new Dimension(5, 0)));
    buttonPane.add(cancel_bt);
    
    getContentPane().add(buttonPane, BorderLayout.PAGE_END);
  }

  /**
   * Returns the modified attributes set.
   * @return The attributes.
   */
  public PrintRequestAttributeSet getAttributes()
  {
    return attributes;
  }

  /**
   * Returns the print service selected by the user.
   * @return The selected print service.
   */
  public PrintService getSelectedPrintService()
  {
    return selectedService;
  }
  
  /**
   * Sets the currently selected print service.
   * 
   * @param service the service selected.
   */
  protected void setSelectedPrintService(PrintService service)
  {
    selectedService = service;
  }
  
  /**
   * Returns the print service array.
   * @return The print services.
   */
  protected PrintService[] getPrintServices()
  {
    return services;
  }
  
  /**
   * Calls update on all panels to adjust
   * for a new selected print service.
   */
  void updateAll()
  {
    pagesetup_panel.update();
    
    if (! onlyPageDialog)
      {
        general_panel.update();
        appearance_panel.update();
      }   
  }
  
  boolean categorySupported(Class category)
  {
    return getSelectedPrintService().
      isAttributeCategorySupported(category);
  }
  
  Object defaultValue(Class category)
  {
    return getSelectedPrintService().
      getDefaultAttributeValue(category);
  }
  
  Attribute attribute(Class category)
  {
    return atts.get(category);
  }
  
  /** 
   *  Action handler for Print/Cancel buttons.
   *  If cancel is pressed we reset the attributes
   *  and the selected service.
   *  
   *  @param e the ActionEvent
   */
  public void actionPerformed(ActionEvent e)
  {
    if (e.getSource() == ok_bt)
      {
        setVisible(false);       
        attributes.addAll(atts);
        dispose();
      }
    else
      {
        setVisible(false);     
        selectedService = null;
        dispose();
      }
  }
  
  /**
   * Retrieves localized messages from the resource bundle.
   * 
   * @param key the key
   * @return The localized value for the key.
   */
  static final String getLocalizedString(String key) {
    return messages.getString(key);
  }
}
