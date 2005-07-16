/* GtkLookAndFeel.java
   Copyright (c) 1999 by Free Software Foundation, Inc.

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

package gnu.javax.swing.plaf.gtk;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.*;

/**
 *
 * @author Brian Jones
 * @see javax.swing.LookAndFeel
 */
public class GtkLookAndFeel extends BasicLookAndFeel
{
    private UIDefaults uiDefaults;

    /**
     */
    public GtkLookAndFeel()
    {
	super();
    }

    /**
     * A short string to identify this look and feel, for example in a 
     * drop down list to choose between several look and feels.
     */
    public String getName() { return "GIMP Toolkit"; }

    /**
     * A much longer description of the look and feel.
     */
    public String getDescription() 
    { 
	return new String("The GIMP Toolkit Look and Feel for Java, " + 
			  "written by Brian Jones (cbj@gnu.org), " + 
			  "(c) 1999 by Free Software Foundation, Inc.  " +
			  "http://www.classpath.org");
    }

    /**
     * Return a unique string identifying this look and feel as different
     * from and not a subclass of any other look and feel.  Usually, a 
     * subclass will return the same <code>String</code> here as the
     * original look and feel if only a few changes are being made rather 
     * than something completely new and different.
     */
    public String getID()
    {
	return "Gtk";
    }

    public boolean isNativeLookAndFeel()
    {
	return false;
    }

    public boolean isSupportedLookAndFeel()
    {
	return true;
    }

    protected void initClassDefaults(UIDefaults table)
    {
	super.initClassDefaults(table);

	String gtkPkgName = "gnu.javax.swing.plaf.gtk.";

	
	Object[] defaults = { 
	    "SliderUI", gtkPkgName + "GtkSliderUI"
	};
	/*
	    "CheckBoxUI", gtkPkgName + "GtkCheckBoxUI",
	    "ButtonUI", gtkPkgName + "GtkButtonUI"
		"ColorChooserUI", "MetalColorChooserUI",
		"MenuBarUI", "MetalMenuBarUI",
		"MenuUI", "MetalMenuUI", 
		"MenuItemUI", "MetalMenuItemUI", 
		"CheckBoxMenuItemUI", "MetalCheckBoxMenuItemUI", 
		"RadioButtonMenuItemUI", "MetalRadioButtonMenuItemUI", 
		"RadioButtonUI", "MetalRadioButtonUI", 
		"ToggleButtonUI", "MetalToggleButtonUI",
		"PopupMenuUI", "MetalPopupMenuUI",
		"ProgressBarUI", "MetalProgressBarUI",
		"ScrollBarUI", "MetalScrollBarUI",
		"ScrollPaneUI", "MetalScrollPaneUI",
		"SplitPaneUI", "MetalSplitPaneUI",
		"SeparatorUI", "MetalSeparatorUI",
		"ToolBarSeparatorUI", "MetalToolBarSeparatorUI",
		"PopupMenuSeparatorUI", "MetalPopupMenuSeparatorUI", 
		"TabbedPaneUI", "MetalTabbedPaneUI",
		"TextAreaUI", "MetalTextAreaUI",
		"TextFieldUI", "MetalTextFieldUI",
		"PasswordFieldUI", "MetalPasswordFieldUI",
		"TextPaneUI", "MetalTextPaneUI",
		"EditorPaneUI", "MetalEditorPaneUI",
		"TreeUI", "MetalTreeUI",
		"LabelUI", "MetalLabelUI",
		"ListUI", "MetalListUI",
		"ToolBarUI", "MetalToolBarUI",
		"ToolTipUI", "MetalToolTipUI",
		"ComboBoxUI", "MetalComboBoxUI",
		"TableUI", "MetalTableUI",
		"TableHeaderUI", "MetalTableHeaderUI",
		"InternalFrameUI", "GtkInternalFrameUI",
		"StandardDialogUI", "GtkStandardDialogUI",
		"DesktopPaneUI", "GtkDesktopPaneUI",
		"DesktopIconUI", "GtkDesktopIconUI",
		"DirectoryPaneUI", "GtkDirectoryPaneUI",
		"FileChooserUI", "GtkFileChooserUI",
		"OptionPaneUI", "GtkOptionPaneUI" }
	*/
	table.putDefaults(defaults);

    }

    protected void initSystemColorDefaults(UIDefaults table)
    {
	String[] colors = {
	    "desktop", "#000000",
	    "activeCaption", "#163555",
	    "activeCaptionText", "#FFFFFF",
	    "activeCaptionBorder", "#000000",
	    "inactiveCaption", "#375676",
	    "inactiveCaptionText", "#999999",
	    "inactiveCaptionBorder", "#000000",
	    "window", "#FFFFFF",
	    "windowBorder", "#969696",
	    "windowText", "#000000",
	    "menu", "#d6d6d6",
	    "menuText", "#000000",
	    "text", "#FFFFFF",
	    "textText", "#000000",
	    "textHighlight", "#00009c",
	    "textHighlightText", "#FFFFFF",
	    "textInactiveText", "#999999",
	    "control", "#d6d6d6",
	    "controlText", "#000000",
	    "controlHighlight", "#eaeaea",
	    "controlLtHighlight", "#eaeaea",
	    "controlShadow", "#c3c3c3",
	    "controlDkShadow", "#888888",
	    "scrollbar", "#c3c3c3",
	    "info", "#d6d6d6",
	    "infoText", "#000000"
	};

	loadSystemColors(table, colors, false);
    }

    protected void initComponentDefaults(UIDefaults table)
    {
	super.initComponentDefaults(table);

	// define common resources
	// fonts
	FontUIResource sansSerifPlain10 = 
	    new FontUIResource("SansSerif", Font.PLAIN, 10);
	FontUIResource serifPlain10 = 
	    new FontUIResource("Serif", Font.PLAIN, 10);
	// insets
	// borders
	// colors
	ColorUIResource controlDkShadow = new ColorUIResource(table.getColor("controlDkShadow"));
	ColorUIResource controlShadow = new ColorUIResource(table.getColor("controlShadow"));
	ColorUIResource control = new ColorUIResource(table.getColor("control"));
	ColorUIResource scrollbar = new ColorUIResource(table.getColor("scrollbar"));
	ColorUIResource controlHighlight = new ColorUIResource(table.getColor("controlHighlight"));
	if (scrollbar == null)
	    System.out.println("scrollbar is null");

  	ColorUIResource white = new ColorUIResource(Color.white);
  	ColorUIResource black = new ColorUIResource(Color.black);
	ColorUIResource blue = new ColorUIResource(Color.blue);

	// icons
	Object errorIcon = LookAndFeel.makeIcon(getClass(), "icons/error.gif");
	// any other resources like dimensions and integer values

	// define defaults
	Object[] defaults = 
	{ 
	    "Button.font", sansSerifPlain10,
	    "CheckBox.font", sansSerifPlain10,
  	    "RadioButton.pressed", black,
	    "Slider.focus", blue,
  	    "Slider.foreground", control,
  	    "Slider.highlight", controlHighlight,
  	    "Slider.shadow", controlShadow,
	    "Slider.background", controlDkShadow
	   
//  	    "Slider.background", "#888888",
//  	    "Slider.focus", "#c3c3c3",
//  	    "Slider.foreground", "#d6d6d6",
//  	    "Slider.highlight", "#ffffff",
//  	    "Slider.shadow", "#000000"


	};

	table.putDefaults(defaults);
    }
}
