/* JLayeredPane.java -- 
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import javax.accessibility.Accessible;

/**
 * <p>The "Layered Pane" is a container which divides its children into 6 (or
 * more) disjoint sets. the pre-defined sets are:</p>
 *
 *  <ul>
 *    <li>"Frame Content"</li>
 *    <li>"Default"</li>
 *    <li>"Palette"</li>
 *    <li>"Modal"</li>
 *    <li>"Popup"</li>
 *    <li>"Drag"</li>
 *  </ul>
 *
 * <p>A child is in exactly one of these layers at any time, though there may
 * be other layers if someone creates them.</p>
 *
 * <p>The purpose of this class is to translate this view of "layers" into a
 * contiguous array of components: the one held in our ancestor,
 * {@link java.awt.Container}.</p>
 *
 * <p>There is a precise set of words we will use to refer to numbers within
 * this class:</p>
 * 
 * <dl>
 * <dt>Component Index:</dt> 
 * <dd>An offset into the <code>component</code> array held in our ancestor,
 * {@link java.awt.Container}, from <code>[0 .. component.length)</code>. The drawing
 * rule with indices is that 0 is drawn last.</dd>
 *
 * <dt>Layer Number:</dt>
 * <dd>A general <code>int</code> specifying a layer within this component.  Negative
 * numbers are drawn first, then layer 0, then positive numbered layers, in
 * ascending order.</dd>
 *
 * <dt>Position:</dt> 
 * <dd>An offset into a layer's "logical drawing order". Layer position 0
 * is drawn last. Layer position -1 is a synonym for the first layer
 * position (the logical "bottom").</dd>
 *
 * <p><b>Note:</b> the layer numbering order is the <em>reverse</em> of the
 * component indexing and position order</p>
 *
 * @author Graydon Hoare <graydon@redhat.com>
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

  JLayeredPane()
  {
    layers = new TreeMap ();
    componentToLayer = new Hashtable ();
  }


  /** 
   * Looks up the layer a child component is currently assigned to.
   *
   * @param c the component to look up.
   * @return the layer the component is currently assigned to, in this container.
   * @throws IllegalArgumentException if the component is not a child of this container.
   */

  protected Integer getLayer (Component c)
  {
    if (! componentToLayer.containsKey (c))
	    throw new IllegalArgumentException ();
    return (Integer) componentToLayer.get (c);
  }

  /**
   * <p>Returns a pair of ints representing a half-open interval 
   * <code>[top, bottom)</code>, which is the range of component indices 
   * the provided layer number corresponds to.</p>
   *
   * <p>Note that "bottom" is <em>not</em> included in the interval of
   * component indices in this layer: a layer with 0 elements in it has
   * <code>ret[0] == ret[1]</code>.</p>
   *
   * @param layer the layer to look up.
   * @return the half-open range of indices this layer spans.
   * @throws IllegalArgumentException if layer does not refer to an active layer
   * in this container.
   */

  protected int[] layerToRange (Integer layer)
  {
    int[] ret = new int[2];
    ret[1] = getComponents ().length;
    Iterator i = layers.entrySet ().iterator ();
    while (i.hasNext())
	    {
        Map.Entry pair = (Map.Entry) i.next();
        Integer layerNum = (Integer) pair.getKey ();
        Integer layerSz = (Integer) pair.getValue ();
        if (layerNum == layer)
          {
            ret[0] = ret[1] - layerSz.intValue ();
            return ret;
          }
        else
          {
            ret[1] -= layerSz.intValue ();
          }
	    }
    // should have found the layer during iteration
    throw new IllegalArgumentException ();
  }

  /**
   * Increments the recorded size of a given layer.
   *
   * @param layer the layer number to increment.
   * @see #incrLayer()
   */

  protected void incrLayer(Integer layer)
  {
    int sz = 1;
    if (layers.containsKey (layer))
	    sz += ((Integer)(layers.get (layer))).intValue ();
    layers.put (layer, new Integer(sz));
  }

  /**
   * Decrements the recorded size of a given layer.
   *
   * @param layer the layer number to decrement.
   * @see #decrLayer()
   */

  protected void decrLayer(Integer layer)
  {
    int sz = 0;
    if (layers.containsKey (layer))
	    sz = ((Integer)(layers.get (layer))).intValue () - 1;
    layers.put (layer, new Integer(sz));
  }

  /**
   * Return the greatest layer number currently in use, in this container.
   * This number may legally be positive <em>or</em> negative.
   *
   * @return the least layer number.
   * @see #lowestLayer()
   */

  public int highestLayer()
  {
    if (layers.size() == 0)
	    return 0;
    return ((Integer)(layers.lastKey ())).intValue ();
  }

  /**
   * Return the least layer number currently in use, in this container.
   * This number may legally be positive <em>or</em> negative.
   *
   * @return the least layer number.
   * @see #highestLayer()
   */
    
  public int lowestLayer()
  {
    if (layers.size() == 0)
	    return 0;
    return ((Integer)(layers.firstKey ())).intValue ();
  }

  /**
   * Moves a component to the "front" of its layer. The "front" is a
   * synonym for position 0, which is also the last position drawn in each
   * layer, so is usually the component which occludes the most other
   * components in its layer.
   *
   * @param c the component to move to the front of its layer.
   * @throws IllegalArgumentException if the component is not a child of
   * this container.
   * @see #moveToBack()
   */

  public void moveToFront(Component c)
  {
    setPosition (c, 0);
  }

  /**
   * <p>Moves a component to the "back" of its layer. The "back" is a
   * synonym for position N-1 (also known as position -1), where N is the
   * size of the layer.</p>
   *
   * <p>The "back" of a layer is the first position drawn, so the component at
   * the "back" is usually the component which is occluded by the most
   * other components in its layer.</p>
   *
   * @param c the component to move to the back of its layer.
   * @throws IllegalArgumentException if the component is not a child of
   * this container.
   * @see #moveToFront()
   */

  public void moveToBack(Component c)
  {
    setPosition (c, -1);
  }

  /**
   * Return the position of a component within its layer. Positions are assigned
   * from the "front" (position 0) to the "back" (position N-1), and drawn from 
   * the back towards the front.
   *
   * @param c the component to get the position of.
   * @throws IllegalArgumentException if the component is not a child of
   * this container.
   * @see #setPosition()
   */
    
  public int getPosition(Component c)
  {
    Integer layer = getLayer (c);
    int[] range = layerToRange (layer);
    int top = range[0];
    int bot = range[1];
    Component[] comps = getComponents ();
    for (int i = top; i < bot; ++i)
	    {
        if (comps[i] == c)
          return i - top;
	    }
    // should have found it
    throw new IllegalArgumentException ();
  }

  /**
   * Change the position of a component within its layer. Positions are assigned
   * from the "front" (position 0) to the "back" (position N-1), and drawn from 
   * the back towards the front.
   *
   * @param c the component to change the position of.
   * @param position the position to assign the component to.
   * @throws IllegalArgumentException if the component is not a child of
   * this container.
   * @see #getPosition()
   */

  public void setPosition(Component c, int position)
  {
    Integer layer = getLayer (c);
    int[] range = layerToRange (layer);
    if (range[0] == range[1])
	    throw new IllegalArgumentException ();

    int top = range[0];
    int bot = range[1];
    if (position == -1)
	    position = (bot - top) - 1;
    int targ = top + position;
    int curr = -1;

    Component[] comps = getComponents();
    for (int i = top; i < bot; ++i)
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

    super.swapComponents (curr, targ);
    validate();
    repaint();
  }
    
  /**
   * Return an array of all components within a layer of this
   * container. Components are ordered front-to-back, with the "front"
   * element (which draws last) at position 0 of the returned array.
   *
   * @param layer the layer to return components from.
   * @return the components in the layer.
   */

  public Component[] getComponentsInLayer(int layer)
  {
    int[] range = layerToRange (getObjectForLayer (layer));
    if (range[0] == range[1])
	    return new Component[0];
    else
	    {
        Component[] comps = getComponents ();
        int sz = range[1] - range[0];
        Component[] nc = new Component[sz];
        for (int i = 0; i < sz; ++i)
          nc[i] = comps[range[0] + i];
        return nc;
	    }
  }

  /**
   * Return the number of components within a layer of this
   * container.
   *
   * @param layer the layer count components in.
   * @return the number of components in the layer.
   */

  public int getComponentCountInLayer(int layer)
  {
    int[] range = layerToRange (getObjectForLayer (layer));
    if (range[0] == range[1])
	    return 0;
    else
	    return (range[1] - range[0]);
  }

  /**
   * Return a hashtable mapping child components of this container to
   * Integer objects representing the component's layer assignments.
   */

  protected Hashtable getComponentToLayer()
  {
    return componentToLayer;
  }

  /**
   * Return the index of a component within the underlying (contiguous)
   * array of children. This is a "raw" number which does not represent the
   * child's position in a layer, but rather its position in the logical
   * drawing order of all children of the container.
   *
   * @param c the component to look up.
   * @return the external index of the component.
   * @throws IllegalArgumentException if the component is not a child of
   * this container.
   */

  public int getIndexOf(Component c) 
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

  /**
   * Return an Integer object which holds the same int value as the
   * parameter. This is strictly an optimization to minimize the number of
   * identical Integer objects which we allocate.
   *
   * @param layer the layer number as an int.
   * @return the layer number as an Integer, possibly shared.
   */

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

  /**
   * Computes an index at which to request the superclass {@link
   * java.awt.Container} inserts a component, given an abstract layer and
   * position number.
   *
   * @param layer the layer in which to insert a component.
   * @param position the position in the layer at which to insert a component.
   * @return the index at which to insert the component.
   */
    
  protected int insertIndexForLayer(int layer, int position)
  {

    Integer lobj = getObjectForLayer (layer);
    if (! layers.containsKey(lobj))
      layers.put (lobj, new Integer (0));
    int[] range = layerToRange (lobj);
    if (range[0] == range[1])
        return range[0];
	
    int top = range[0];
    int bot = range[1];

    if (position == -1 || position > (bot - top))
        return bot;
    else
        return top + position;
  }

  /**
   * Removes a child from this container. The child is specified by
   * index. After removal, the child no longer occupies a layer.
   *
   * @param index the index of the child component to remove.
   */
    
  public void remove (int index)
  {
    Component c = getComponent (index);
    Integer layer = getLayer (c);
    decrLayer (layer);
    componentToLayer.remove (c);
    super.remove (index);
  }

  /**
   * Removes a child from this container. The child is specified directly.
   * After removal, the child no longer occupies a layer.
   *
   * @param comp the child to remove.
   */
	
  public void remove (Component comp)
  {
    remove (getIndexOf (comp));
  }

  /**
   * <p>Set the layer property for a component, within this container. The
   * component will be implicitly mapped to the bottom-most position in the
   * layer, but only if added <em>after</em> calling this method.</p>
   *
   * <p>Read that carefully: this method should be called <em>before</em> the
   * component is added to the container.</p>
   *
   * @param c the component to set the layer property for.
   * @param layer the layer number to assign to the component.
   */

  public void setLayer(Component c, int layer)
  {
    componentToLayer.put (c, getObjectForLayer (layer));
  }

  /**
   * Set the layer and position of a component, within this container.
   *
   * @param c the child component to set the layer property for.
   * @param layer the layer number to assign to the component.
   * @param position the position number to assign to the component.
   */

  public void setLayer(Component c,
                       int layer,
                       int position)
  {
    componentToLayer.put (c, getObjectForLayer (layer));
    setPosition(c, position);
    validate();
    repaint();
  }

  /**
   * Overrides the default implementation from {@link java.awt.Container}
   * such that <code>layerConstraint</code> is interpreted as an {@link
   * Integer}, specifying the layer to which the component will be added
   * (at the bottom position).
   *
   * @param comp the component to add.
   * @param layerConstraint an integer specifying the layer to add the component to.
   * @param index an ignored parameter, for compatibility.
   */

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
	
    super.addImpl(comp, null, newIdx);	
    validate();
    repaint();
  }     
}
