/* JLayeredPane.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.swing;

import java.awt.Component;
import java.util.*;
import java.awt.Component;
import javax.accessibility.Accessible;


/**
 * The "Layered Pane" is a container which divides its children into 6 (or
 * more) disjoint sets. the pre-defined sets are:
 *
 *  "Frame Content", "Default", "Palette", "Modal", "Popup", and "Drag".
 *
 * A child is in exactly one of these layers at any time, though there may
 * be other layers if someone creates them.
 *
 * The purpose of this class is to translate this view of "layers" into a
 * contiguous array of components: the one held in our ancestor,
 * java.awt.Container.
 *
 * There is a precise set of words we will use to refer to numbers within
 * this class:
 * 
 * Internal Component Index: an offset into the "component" array held in
 * our ancestor, java.awt.Container, from [0 .. component.length). The
 * drawing rule with internal indices is that 0 is drawn first.
 *
 * External Component Index: an offset into the "logical drawing order" of
 * this container. If I is the internal index of a component, the external
 * index E = component.length - I. The rule with external indices is that 0
 * is drawn last.
 *
 * Layer Number: a general int specifying a layer within this component.
 * Negative numbers are drawn first, then layer 0, then positive numbered
 * layers, in ascending order.
 *
 * Position: an offset into a layer's "logical drawing order". Layer
 * position 0 is drawn last. Layer position -1 is a synonym for the first
 * layer position (the logical "bottom").
 */

public class JLayeredPane extends JComponent implements Accessible
{

    public static String LAYER_PROPERTY = "LAYER_PROPERTY";

    public static Integer FRAME_CONTENT_LAYER = new Integer (-30000);

    public static Integer DEFAULT_LAYER = new Integer (0);
    public static Integer PALETTE_LAYER = new Integer (100);
    public static Integer MODAL_LAYER   = new Integer (200);
    public static Integer POPUP_LAYER   = new Integer (300);
    public static Integer DRAG_LAYER    = new Integer (400);

    TreeMap layers;               // Layer Number (Integer) -> Layer Size (Integer)
    Hashtable componentToLayer;   // Component -> Layer Number (Integer)

    protected Integer getLayer (Component c)
    {
	if (! componentToLayer.containsKey (c))
	    throw new IllegalArgumentException ();
	return (Integer) componentToLayer.get (c);
    }

    // this returns a half-open range [bottom, top), which is the range of
    // internal component indices this layer number corresponds to.  note
    // that top is *not* included in the range of component indices in this
    // layer: a layer with 0 elements in it has ret[0] == ret[1].

    protected int[] layerToRange (Integer layer)
    {
	int[] ret = new int[2];	
	Iterator i = layers.entrySet ().iterator ();
	while (i.hasNext())
	    {
		Map.Entry pair = (Map.Entry) i.next();
		Integer layerNum = (Integer) pair.getKey ();
		Integer layerSz = (Integer) pair.getValue ();
		if (layerNum == layer)
		    {
			ret[1] = ret[0] + layerSz.intValue ();
			return ret;
		    }
		else
		    {
			ret[0] += layerSz.intValue ();
		    }
	    }
	// should have found the layer during iteration
	throw new IllegalArgumentException ();
    }

    protected void incrLayer(Integer layer)
    {
	int sz = 1;
	if (layers.containsKey (layer))
	    sz += ((Integer)(layers.get (layer))).intValue ();
	layers.put (layer, new Integer(sz));
    }

    protected void decrLayer(Integer layer)
    {
	int sz = 0;
	if (layers.containsKey (layer))
	    sz = ((Integer)(layers.get (layer))).intValue () - 1;
	layers.put (layer, new Integer(sz));
    }

    JLayeredPane()
    {
	layers = new TreeMap ();
	layers.put (FRAME_CONTENT_LAYER, new Integer (0));
	layers.put (DEFAULT_LAYER, new Integer (0));
	layers.put (PALETTE_LAYER, new Integer (0));
	layers.put (MODAL_LAYER, new Integer (0));
	layers.put (POPUP_LAYER, new Integer (0));
	layers.put (DRAG_LAYER, new Integer (0));	

	componentToLayer = new Hashtable ();
    }

    public int highestLayer()
    {
	if (layers.size() == 0)
	    return 0;
	return ((Integer)(layers.lastKey ())).intValue ();
    }
    
    public int lowestLayer()
    {
	if (layers.size() == 0)
	    return 0;
	return ((Integer)(layers.firstKey ())).intValue ();
    }

    public void moveToFront(Component c)
    {
	setPosition (c, 0);
    }

    public void moveToBack(Component c)
    {
	setPosition (c, -1);
    }
    
    public int getPosition(Component c)
    {
	Integer layer = getLayer (c);
	int[] range = layerToRange (layer);
	int top = (range[1] - 1);
	Component[] comps = getComponents ();
	for (int i = range[0]; i < range[1]; ++i)
	    {
		if (comps[i] == c)
		    return top - i;
	    }
	// should have found it
	throw new IllegalArgumentException ();
    }

    public void setPosition(Component c, int position)
    {
	Integer layer = getLayer (c);
	int[] range = layerToRange (layer);
	if (range[0] == range[1])
	    throw new IllegalArgumentException ();

	int top = (range[1] - 1);
	if (position == -1)
	    position = top - range[0];
	int targ = top - position;
	int curr = -1;

	Component[] comps = getComponents();
	for (int i = range[0]; i < range[1]; ++i)
	    {
		if (comps[i] == c)
		    {
			curr = i;
			break;
		    }
	    }
	if (curr == -1)
	    // should have found it
	    throw new IllegalArgumentException ();

	// System.err.println("set component position to " + position + " in layer " + layer);

	Component tmp = comps[curr];
	super.remove (curr);
	super.add (tmp, targ);
	super.validate ();
    }
    


    public Component[] getComponentsInLayer(int layer)
    {
	int[] range = layerToRange (getObjectForLayer (layer));
	if (range[0] == range[1])
	    return new Component[0];
	else
	    {
		Component[] comps = getComponents ();
		int sz = (range[1] - 1) - range[0];
		Component[] nc = new Component[sz];
		for (int i = 0; i < sz; ++i)
		    nc[i] = comps[range[0] + i];
		return nc;
	    }
    }

    public int getComponentCountInLayer(int layer)
    {
	int[] range = layerToRange (getObjectForLayer (layer));
	if (range[0] == range[1])
	    return 0;
	else
	    return ((range[1] - 1) - range[0]);
    }

    protected Hashtable getComponentToLayer()
    {
	return componentToLayer;
    }

    protected int getInternalIndexOf(Component c) 
    {
	Integer layer = getLayer (c);
	int[] range = layerToRange (layer);
	Component[] comps = getComponents();
	for (int i = range[0]; i < range[1]; ++i)
	    {
		if (comps[i] == c)
		    return i;
	    }
	// should have found the component during iteration
	throw new IllegalArgumentException ();
    }


    public int getIndexOf(Component c) 
    {
	// returns the *external* index of the component.
	int top = getComponentCount() - 1;
	return top - getIndexOf (c);
    }    


    protected Integer getObjectForLayer(int layer)
    {
	switch (layer)
	    {
	    case -30000:
		return FRAME_CONTENT_LAYER;

	    case 0:
		return DEFAULT_LAYER;

	    case 100:
		return PALETTE_LAYER;

	    case 200:
		return MODAL_LAYER;

	    case 300:
		return POPUP_LAYER;

	    case 400:
		return DRAG_LAYER;

	    default:
		break;
	    }

	return new Integer(layer);
    }
    
    protected int insertIndexForLayer(int layer, int position)
    {
	int[] range = layerToRange (getObjectForLayer (layer));
	if (range[0] == range[1])
	    return range[0];
	
	int bottom = range[0];
	int top = range[1] - 1;
	
	if (position == -1 || position > (top - bottom))
	    return bottom;
	else
	    return top - position;
    }
    
    public void remove (int index)
    {
	Component c = getComponent (index);
	Integer layer = getLayer (c);
	decrLayer (layer);
	componentToLayer.remove (c);
	super.remove (index);
    }
	
    public void remove (Component comp)
    {
	Integer layer = getLayer (comp);
	decrLayer (layer);
	componentToLayer.remove (comp);
	super.remove (comp);
    }

    public void removeAll ()
    {
	componentToLayer.clear ();
	layers.clear ();
	super.removeAll ();
    }

    public void setLayer(Component c, int layer)
    {
	componentToLayer.put (c, getObjectForLayer (layer));
    }

    public void setLayer(Component c,
			 int layer,
			 int position)
    {
	componentToLayer.put (c, getObjectForLayer (layer));
	setPosition(c, position);
        repaint();
    }

    protected void addImpl(Component comp, Object layerConstraint, int index) 
    {        	
	Integer layer;
	if (layerConstraint != null && layerConstraint instanceof Integer)
		layer = (Integer) layerConstraint;
	else if (componentToLayer.containsKey (comp))
	    layer = (Integer) componentToLayer.remove (comp);
	else
	    layer = DEFAULT_LAYER;

	int newIdx = insertIndexForLayer(layer.intValue (), -1);
	componentToLayer.put (comp, layer);
	incrLayer (layer);

	// System.err.println("adding component to layer " + layer);
	
        super.addImpl(comp, null, newIdx);	
        validate();
        repaint();
    }     
}
