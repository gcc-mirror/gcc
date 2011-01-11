/* MetalThemeEditor.java -- Edit themes using this application
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

import gnu.javax.swing.plaf.metal.CustomizableTheme;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * This application serves two purposes: 1. demonstrate the color chooser
 * component, 2. make creating new Metal themes as easy as possible.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class MetalThemeEditor
  extends JPanel
{
  /**
   * An icon to display a chosen color in a button.
   */
  private class ColorIcon implements Icon
  {
    /**
     * The color to be shown on the icon.
     */
    Color color;

    /**
     * Creates a new ColorIcon.
     *
     * @param c the color to be displayed
     */
    ColorIcon(Color c)
    {
      color = c;
    }

    /**
     * Returns the icon height, which is 10.
     *
     * @return 10
     */
    public int getIconHeight()
    {
      return 10;
    }

    /**
     * Returns the icon width, which is 30.
     *
     * @return 30
     */
    public int getIconWidth()
    {
      return 30;
    }

    /**
     * Paints the icon.
     *
     * @param c the component to paint on
     * @param g the graphics to use
     * @param x the x location
     * @param y the y location
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      g.setColor(color);
      g.fillRect(x, y, 30, 10);
    }

  }

  /**
   * Opens up a color chooser and lets the user select a color for the theme.
   */
  private class ChooseColorAction implements ActionListener
  {

    /**
     * The button that will get updated when a new color is selected.
     */
    private JButton button;

    /**
     * Specifies which color of the theme should be updated. See constants in
     * the MetalThemeEditor class.
     */
    private int colorType;

    /**
     * Creates a new ChooseColorAction. The specified button will have its
     * icon updated to the new color if appropriate.
     *
     * @param b the button to update
     * @param type the color type to update
     */
    ChooseColorAction(JButton b, int type)
    {
      button = b;
      colorType = type;
    }

    /**
     * Opens a color chooser and lets the user select a color.
     */
    public void actionPerformed(ActionEvent event)
    {
      Color c = JColorChooser.showDialog(button, "Choose a color",
                                         getColor(colorType));
      if (c != null)
        {
          setColor(colorType, c);
          button.setIcon(new ColorIcon(c));
        }
    }
  }

  /**
   * Denotes the primary1 color of the theme.
   */
  private static final int PRIMARY1 = 0;

  /**
   * Denotes the primary2 color of the theme.
   */
  private static final int PRIMARY2 = 1;

  /**
   * Denotes the primary3 color of the theme.
   */
  private static final int PRIMARY3 = 2;

  /**
   * Denotes the secondary1 color of the theme.
   */
  private static final int SECONDARY1 = 3;

  /**
   * Denotes the secondary2 color of the theme.
   */
  private static final int SECONDARY2 = 4;

  /**
   * Denotes the secondary3 color of the theme.
   */
  private static final int SECONDARY3 = 5;

  /**
   * The theme that is edited.
   */
  CustomizableTheme theme;

  /**
   * Creates a new instance of the MetalThemeEditor.
   */
  MetalThemeEditor()
  {
    theme = new CustomizableTheme();
    setBorder(BorderFactory.createEmptyBorder(12, 12, 11, 11));
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    add(createConfigurationPanel());
    add(Box.createVerticalStrut(17));
    add(createButtonPanel());
  }

  /**
   * Creates the main panel of the MetalThemeEditor. This is the upper
   * area where the colors can be selected.
   *
   * @return the main panel
   */
  private JPanel createConfigurationPanel()
  {
    JPanel p = new JPanel();
    p.setLayout(new GridBagLayout());
    GridBagConstraints c = new GridBagConstraints();
    c.weightx = 1;
    c.weighty = 0;
    c.fill = GridBagConstraints.HORIZONTAL;
    Insets labelInsets = new Insets(0, 0, 11, 6);
    Insets buttonInsets = new Insets(0, 0, 11, 0);

    // Primary 1
    JLabel primary1Label = new JLabel("Primary 1:");
    c.gridx = 0;
    c.gridy = 0;
    c.insets = labelInsets;
    p.add(primary1Label, c);

    Icon p1Icon = new ColorIcon(theme.getPrimary1());
    JButton primary1Button = new JButton(p1Icon);
    primary1Button.addActionListener(new ChooseColorAction(primary1Button,
                                                           PRIMARY1));
    //c.weightx = 0;
    c.gridx = 1;
    c.insets = buttonInsets;
    p.add(primary1Button, c);
    primary1Label.setLabelFor(primary1Button);

    // Primary 2
    JLabel primary2Label = new JLabel("Primary 2:");
    c.gridx = 0;
    c.gridy = 1;
    c.insets = labelInsets;
    p.add(primary2Label, c);

    Icon p2Icon = new ColorIcon(theme.getPrimary2());
    JButton primary2Button = new JButton(p2Icon);
    primary2Button.addActionListener(new ChooseColorAction(primary2Button,
                                                           PRIMARY2));
    c.gridx = 1;
    c.insets = buttonInsets;
    p.add(primary2Button, c);
    primary2Label.setLabelFor(primary2Button);

    // Primary 3
    JLabel primary3Label = new JLabel("Primary 3:");
    c.gridx = 0;
    c.gridy = 2;
    c.insets = labelInsets;
    p.add(primary3Label, c);

    Icon p3Icon = new ColorIcon(theme.getPrimary3());
    JButton primary3Button = new JButton(p3Icon);
    primary3Button.addActionListener(new ChooseColorAction(primary3Button,
                                                           PRIMARY3));
    c.gridx = 1;
    c.insets = buttonInsets;
    p.add(primary3Button, c);
    primary3Label.setLabelFor(primary3Button);

    // Secondary 1
    JLabel secondary1Label = new JLabel("Secondary 1:");
    c.gridx = 0;
    c.gridy = 3;
    c.insets = labelInsets;
    p.add(secondary1Label, c);

    Icon s1Icon = new ColorIcon(theme.getSecondary1());
    JButton secondary1Button = new JButton(s1Icon);
    secondary1Button.addActionListener(new ChooseColorAction(secondary1Button,
                                                             SECONDARY1));
    c.gridx = 1;
    c.insets = buttonInsets;
    p.add(secondary1Button, c);
    secondary1Label.setLabelFor(secondary1Button);

    // Secondary 2
    JLabel secondary2Label = new JLabel("Secondary 2:");
    c.gridx = 0;
    c.gridy = 4;
    c.insets = labelInsets;
    p.add(secondary2Label, c);

    Icon s2Icon = new ColorIcon(theme.getSecondary2());
    JButton secondary2Button = new JButton(s2Icon);
    secondary2Button.addActionListener(new ChooseColorAction(secondary2Button,
                                                             SECONDARY2));
    c.gridx = 1;
    c.insets = buttonInsets;
    p.add(secondary2Button, c);
    secondary2Label.setLabelFor(secondary2Button);

    // Secondary 3
    JLabel secondary3Label = new JLabel("Secondary 3:");
    c.gridx = 0;
    c.gridy = 5;
    c.insets = labelInsets;
    p.add(secondary3Label, c);

    Icon s3Icon = new ColorIcon(theme.getSecondary3());
    JButton secondary3Button = new JButton(s3Icon);
    secondary3Button.addActionListener(new ChooseColorAction(secondary3Button,
                                                             SECONDARY3));
    c.gridx = 1;
    c.insets = buttonInsets;
    p.add(secondary3Button, c);
    secondary3Label.setLabelFor(secondary3Button);

    return p;
  }

  /**
   * Creates the button panel at the bottom of the MetalThemeEditor.
   *
   * @return the button panel
   */
  private JPanel createButtonPanel()
  {
    JPanel p = new JPanel();
    p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
    p.add(Box.createHorizontalGlue());

    JButton applyButton = new JButton("Apply");
    applyButton.addActionListener
    (new ActionListener()
     {
       public void actionPerformed(ActionEvent ev)
       {
         try
           {
             CustomizableTheme copy = (CustomizableTheme) theme.clone();
             MetalLookAndFeel.setCurrentTheme(copy);
             UIManager.setLookAndFeel(new MetalLookAndFeel());
             Window w = SwingUtilities.getWindowAncestor(MetalThemeEditor.this);
             SwingUtilities.updateComponentTreeUI(w);
           }
         catch (Exception ex)
           {
             ex.printStackTrace();
           }
       }
     });
    p.add(applyButton);

    p.add(Box.createHorizontalStrut(5));

    JButton exportButton = new JButton("Export as Java File");
    exportButton.addActionListener
    (new ActionListener()
     {
       public void actionPerformed(ActionEvent ev)
       {
         export();
       }
     });
    p.add(exportButton);

    return p;
  }

  /**
   * Exports the current theme as Java source file. This will prompt the user
   * to choose a filename.
   */
  void export()
  {
    JFileChooser chooser = new JFileChooser();
    int confirm = chooser.showSaveDialog(this);
    if (confirm == JFileChooser.APPROVE_OPTION)
      exportToFile(chooser.getSelectedFile());
  }

  /**
   * Writes out the current configured Metal theme as Java source file.
   *
   * @param file the file to write into
   */
  void exportToFile(File file)
  {
    String fileName = file.getName();
    if (! fileName.endsWith(".java"))
      {
        JOptionPane.showMessageDialog(this,
                                 "Filename does not denote a Java source file",
                                 "Invalid filename",
                                 JOptionPane.ERROR_MESSAGE);
        return;
      }

    String className = fileName.substring(0, fileName.length() - 5);
    Color p1 = theme.getPrimary1();
    Color p2 = theme.getPrimary2();
    Color p3 = theme.getPrimary3();
    Color s1 = theme.getSecondary1();
    Color s2 = theme.getSecondary2();
    Color s3 = theme.getSecondary3();

    try
      {
        FileOutputStream out = new FileOutputStream(file);
        Writer writer = new OutputStreamWriter(out);
        writer.write("import javax.swing.plaf.ColorUIResource;\n");
        writer.write("import javax.swing.plaf.metal.DefaultMetalTheme;\n");
        writer.write("public class " + className + " extends DefaultMetalTheme\n");
        writer.write("{\n");
        writer.write("  protected ColorUIResource getPrimary1()\n");
        writer.write("  {\n");
        writer.write("    return new ColorUIResource(" + p1.getRGB() + ");\n");
        writer.write("  }\n");
        writer.write("  protected ColorUIResource getPrimary2()\n");
        writer.write("  {\n");
        writer.write("    return new ColorUIResource(" + p2.getRGB() + ");\n");
        writer.write("  }\n");
        writer.write("  protected ColorUIResource getPrimary3()\n");
        writer.write("  {\n");
        writer.write("    return new ColorUIResource(" + p3.getRGB() + ");\n");
        writer.write("  }\n");
        writer.write("  protected ColorUIResource getSecondary1()\n");
        writer.write("  {\n");
        writer.write("    return new ColorUIResource(" + s1.getRGB() + ");\n");
        writer.write("  }\n");
        writer.write("  protected ColorUIResource getSecondary2()\n");
        writer.write("  {\n");
        writer.write("    return new ColorUIResource(" + s2.getRGB() + ");\n");
        writer.write("  }\n");
        writer.write("  protected ColorUIResource getSecondary3()\n");
        writer.write("  {\n");
        writer.write("    return new ColorUIResource(" + s3.getRGB() + ");\n");
        writer.write("  }\n");
        writer.write("}\n");
        writer.close();
        out.close();
      }
    catch (FileNotFoundException ex)
      {
        ex.printStackTrace();
      }
    catch (IOException ex)
      {
        ex.printStackTrace();
      }

  }

  /**
   * Returns the color of the theme with the specified type. For the possible
   * types see the constants of this class.
   *
   * @param colorType the color type to fetch from the theme
   *
   * @return the current color of the specified type
   *
   * @throws IllegalArgumentException for illegal color types
   */
  Color getColor(int colorType)
  {
    Color color = null;
    switch (colorType)
    {
    case PRIMARY1:
      color = theme.getPrimary1();
      break;
    case PRIMARY2:
      color = theme.getPrimary2();
      break;
    case PRIMARY3:
      color = theme.getPrimary3();
      break;
    case SECONDARY1:
      color = theme.getSecondary1();
      break;
    case SECONDARY2:
      color = theme.getSecondary2();
      break;
    case SECONDARY3:
      color = theme.getSecondary3();
      break;
    default:
      throw new IllegalArgumentException("Unknown color type: " + colorType);
    }
    return color;
  }

  /**
   * Sets the color of the specified type in the current theme.
   *
   * @param colorType the color type
   * @param color the color to set
   *
   * @throws IllegalArgumentException for illegal color types
   */
  void setColor(int colorType, Color color)
  {
    switch (colorType)
    {
      case PRIMARY1:
        theme.setPrimary1(color);
        break;
      case PRIMARY2:
        theme.setPrimary2(color);
        break;
      case PRIMARY3:
        theme.setPrimary3(color);
        break;
      case SECONDARY1:
        theme.setSecondary1(color);
        break;
      case SECONDARY2:
        theme.setSecondary2(color);
        break;
      case SECONDARY3:
        theme.setSecondary3(color);
        break;
      default:
        throw new IllegalArgumentException("Illegal color type: " + colorType);
    }
  }

  /**
   * The entry point to the application.
   *
   * @param args ignored
   */
  public static void main(String[] args)
  {
    JFrame f = new JFrame("MetalThemeEditor");
    f.setContentPane(new MetalThemeEditor());
    f.pack();
    f.setVisible(true);
  }

  /**
   * Returns a DemoFactory that creates a MetalThemeEditor.
   *
   * @return a DemoFactory that creates a MetalThemeEditor
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new MetalThemeEditor();
      }
    };
  }
}
