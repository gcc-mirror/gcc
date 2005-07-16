/* GtkSliderUI.java
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
import javax.swing.plaf.*;
import javax.swing.plaf.basic.*;

/**
 * Gtk-like slider
 *
 * @author Brian Jones
 * @see javax.swing.LookAndFeel
 */
public class GtkSliderUI extends BasicSliderUI
{
    private static Color thumbFgColor;
    private static Color thumbBgColor;
    private static Color thumbHighlight;
    private static Color thumbFocus;

    private static Color bgColor;
    private static Color fgColor;
    private static Color focusColor;
    private static Color highlight;
    private static Color shadow;

    private static final Dimension PREF_HORIZ = new Dimension(250, 15);
    private static final Dimension PREF_VERT = new Dimension(15, 250);
    private static final Dimension MIN_HORIZ = new Dimension(25, 15);
    private static final Dimension MIN_VERT = new Dimension(15, 25);

    public GtkSliderUI() 
    {
	super(null);
	bgColor = UIManager.getColor("Slider.background");
	fgColor = UIManager.getColor("Slider.foreground");
	focusColor = UIManager.getColor("Slider.focus");
	highlight = UIManager.getColor("Slider.highlight");
	shadow = UIManager.getColor("Slider.shadow");

	System.out.println("bgColor: " + bgColor);
	System.out.println("fgColor: " + fgColor);
	System.out.println("focusColor: " + focusColor);
	System.out.println("highlight: " + highlight);
	System.out.println("shadow: " + shadow);
    }

    public static ComponentUI createUI(JComponent c)
    {
	return new GtkSliderUI();
    }

    // methods not overridden here, using Basic defaults
    // installUI()
    // uninstall()

    public Dimension getPreferredHorizontalSize()
    {
	/*
	Dimension thumbSize = getThumbSize();
	Dimenstion labelSize = getLabelSize();
	// getTickLength()
	int width = thumbSize.width + 
	getWidthOfWidestLabel
	*/
	return PREF_HORIZ;
    }

    public Dimension getPreferredVerticalSize()
    {
	return PREF_VERT;
    }

    public Dimension getMinimumHorizontalSize()
    {
	return MIN_HORIZ;
    }

    public Dimension getMinimumVerticalSize()
    {
	return MIN_VERT;
    }

    /** 
     * Returns thumb size based on slider orientation
     */
    protected Dimension getThumbSize()
    {
	Dimension size = new Dimension();

	if (slider.getOrientation() == JSlider.VERTICAL) {
	    size.width = 15;
	    size.height = 33;
	}
	else {
	    size.width = 33;
	    size.height = 15;
	}
	return size;
    }

    /**
     * Reserved width or height for ticks, as appropriate to the slider
     * orientation.
     */
    protected int getTickLength()
    {
	return 10;
    }

    public void paintFocus(Graphics g)
    {
	super.paintFocus(g);
	System.err.println("focus " + focusRect);
    }

    /**
     * Must account for Unicode when drawing text.
     */
    public void paintLabels(Graphics g)
    {
	super.paintLabels(g);
	System.err.println("label " + labelRect);
    }

    /**
     * A drawRect() generated slider has ghosting when moving left on 
     * a horizontal slider and the bottom is not painted when moving 
     * right.
     */
    public void paintThumb(Graphics g)
    {
	int x = thumbRect.x;
	int y = thumbRect.y;
	int h = thumbRect.height;
	int w = thumbRect.width;

//  	    "Slider.background", "#888888",
//  	    "Slider.focus", "#c3c3c3",
//  	    "Slider.foreground", "#d6d6d6",
//  	    "Slider.highlight", "#ffffff",
//  	    "Slider.shadow", "#000000"

	g.setColor(fgColor);
	g.fillRect(x,y,w,h);
	g.setColor(bgColor);
	
	if (slider.getOrientation() == JSlider.HORIZONTAL) {
	    g.drawRect(x, y, w, h);
	    g.setColor(highlight);
	    g.drawLine(x+1, y+h-1, x+w, y+h-1);
	    g.setColor(focusColor);
	    g.drawLine(x+2, y+h-2, x+w, y+h-2);
	    g.setColor(Color.black);
	    g.drawLine(x+1, y+h-2, x+1, y+h-2);
	    g.drawRect(x+1, y+1, w-1, 12);	    
	}	
	else 
	    g.drawRect(x, y, w, h);

	System.err.println("thumb " + thumbRect);
    }

    // public void paintTicks(Graphics g)
    
    public void paintTrack(Graphics g)
    {
//  	super.paintTrack(g);
	int x = trackRect.x;
	int y = trackRect.y;
	int h = trackRect.height;
	int w = trackRect.width;

	System.err.println("track " + trackRect);

	g.setColor(Color.black);
	g.fillRect(x,y,w,h);

//  	if (slider.getOrientation() == JSlider.HORIZONTAL)
//  	    g.drawLine(x, y+h-1, x+w-1, y+h-1);
//  	else
//  	    g.drawLine(x+w-1, y, x+w-1, y+h-1);

//  	System.err.println("track " + trackRect);
//  	System.err.println("content " + contentRect);
    }

    // the four methods below allow you to control tick painting without 
    // worrying about what paintTicks does, look for in other UI delegates
    // protected void paintMajorTickForHorizSlider(Graphics g, Rectangle tickBounds, int x)
    // protected void paintMajorTickForVertSlider(Graphics g, Rectangle tickBounds, int y)
    // protected void paintMinorTickForHorizSlider(Graphics g, Rectangle tickBounds, int x)
    // protected void paintMinorTickForVertSlider(Graphics g, Rectangle tickBounds, int y)
}
