/* KeyboardManager.java -- 
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing;

import java.applet.Applet;
import java.awt.Component;
import java.awt.Container;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * This class maintains a mapping from top-level containers to a 
 * Hashtable.  The Hashtable maps KeyStrokes to Components to be used when 
 * Components register keyboard actions with the condition
 * JComponent.WHEN_IN_FOCUSED_WINDOW.
 * 
 * @author Anthony Balkissoon abalkiss at redhat dot com
 *
 */
class KeyboardManager
{
  /** Shared instance of KeyboardManager **/
  static KeyboardManager manager = new KeyboardManager();
  
  /** 
   * A mapping between top level containers and Hashtables that 
   * map KeyStrokes to Components.
   */
  Hashtable topLevelLookup = new Hashtable();  
  
  /**
   * A mapping between top level containers and Vectors of JMenuBars
   * used to allow all the JMenuBars within a top level container
   * a chance to consume key events.
   */
  Hashtable menuBarLookup = new Hashtable();
  /**
   * Returns the shared instance of KeyboardManager.
   * @return the shared instance of KeybaordManager.
   */
  public static KeyboardManager getManager()
  {
    return manager;
  }
  /**
   * Returns the top-level ancestor for the given JComponent.
   * @param c the JComponent whose top-level ancestor we want
   * @return the top-level ancestor for the given JComponent.
   */
  static Container findTopLevel (Component c)
  {
    Container topLevel = (c instanceof Container) ? (Container) c
                                                 : c.getParent();
    while (topLevel != null && 
           !(topLevel instanceof Window) && 
           !(topLevel instanceof Applet) && 
           !(topLevel instanceof JInternalFrame))
      topLevel = topLevel.getParent();
    return topLevel;
  }
  
  /**
   * Returns the Hashtable that maps KeyStrokes to Components, for 
   * the specified top-level container c.  If no Hashtable exists
   * we create and register it here and return the newly created
   * Hashtable.
   * 
   * @param c the top-level container whose Hashtable we want
   * @return the Hashtable mapping KeyStrokes to Components for the 
   * specified top-level container
   */
  Hashtable getHashtableForTopLevel (Container c)
  {
    Hashtable keyToComponent = (Hashtable)topLevelLookup.get(c);
    if (keyToComponent == null)
      {
        keyToComponent = new Hashtable();
        topLevelLookup.put(c, keyToComponent);
      }
    return keyToComponent;
  }
  
  /**
   * Registers a KeyStroke with a Component.  This does not register
   * the KeyStroke to a specific Action.  When searching for a 
   * WHEN_IN_FOCUSED_WINDOW binding we will first go up to the focused
   * top-level Container, then get the Hashtable that maps KeyStrokes 
   * to components for that particular top-level Container, then 
   * call processKeyBindings on that component with the condition
   * JComponent.WHEN_IN_FOCUSED_WINDOW.
   * @param comp the JComponent associated with the KeyStroke
   * @param key the KeyStroke
   */
  public void registerBinding(JComponent comp, KeyStroke key)
  {
    // This method associates a KeyStroke with a particular JComponent
    // When the KeyStroke occurs, if this component's top-level ancestor
    // has focus (one of its children is the focused Component) then 
    // comp.processKeyBindings will be called with condition
    // JComponent.WHEN_IN_FOCUSED_WINDOW.

    // Look for the JComponent's top-level parent and return if it is null
    Container topLevel = findTopLevel(comp);
    if (topLevel == null)
      return;
    
    // Now get the Hashtable for this top-level container
    Hashtable keyToComponent = getHashtableForTopLevel(topLevel);    
    
    // And add the new binding to this Hashtable
    // FIXME: should allow more than one JComponent to be associated
    // with a KeyStroke, in case one of them is disabled
    keyToComponent.put(key, comp);
  }
  
  public void clearBindingsForComp(JComponent comp)
  {
    // This method clears all the WHEN_IN_FOCUSED_WINDOW bindings associated
    // with <code>comp</code>.  This is used for a terribly ineffcient
    // strategy in which JComponent.updateComponentInputMap simply clears
    // all bindings associated with its component and then reloads all the
    // bindings from the updated ComponentInputMap.  This is only a preliminary
    // strategy and should be improved upon once the WHEN_IN_FOCUSED_WINDOW
    // bindings work.
    
    // Find the top-level ancestor
    
    Container topLevel = findTopLevel(comp);
    if (topLevel == null)
      return;
    // And now get its Hashtable
    Hashtable keyToComponent = getHashtableForTopLevel(topLevel);

    Enumeration keys = keyToComponent.keys();
    Object temp;

    // Iterate through the keys and remove any key whose value is comp
    while (keys.hasMoreElements())
      {
        temp = keys.nextElement();
        if (comp == (JComponent)keyToComponent.get(temp))
          keyToComponent.remove(temp);          
      }
  }
    
  /**
   * This method registers all the bindings in the given ComponentInputMap.
   * Rather than call registerBinding on all the keys, we do the work here
   * so that we don't duplicate finding the top-level container and 
   * getting its Hashtable.
   * 
   * @param map the ComponentInputMap whose bindings we want to register
   */
  public void registerEntireMap (ComponentInputMap map)
  {
    if (map == null)
      return;    
    JComponent comp = map.getComponent();
    KeyStroke[] keys = map.keys();
    if (keys == null)
      return;
    // Find the top-level container associated with this ComponentInputMap
    Container topLevel = findTopLevel(comp);
    if (topLevel == null)
      return;
    
    // Register the KeyStrokes in the top-level container's Hashtable
    Hashtable keyToComponent = getHashtableForTopLevel(topLevel);
    for (int i = 0; i < keys.length; i++)
      keyToComponent.put(keys[i], comp);
  }
  
  public boolean processKeyStroke (Component comp, KeyStroke key, KeyEvent e)
  {
    boolean pressed = e.getID() == KeyEvent.KEY_PRESSED;

    // Look for the top-level ancestor
    Container topLevel = findTopLevel(comp);
    if (topLevel == null)
      return false;    
    // Now get the Hashtable for that top-level container
    Hashtable keyToComponent = getHashtableForTopLevel(topLevel);
    Enumeration keys = keyToComponent.keys();
    JComponent target = (JComponent)keyToComponent.get(key);    
    if (target != null && target.processKeyBinding
        (key, e, JComponent.WHEN_IN_FOCUSED_WINDOW, pressed))
      return true;
    
    // Have to give all the JMenuBars a chance to consume the event
    Vector menuBars = getVectorForTopLevel(topLevel);
    for (int i = 0; i < menuBars.size(); i++)
      if (((JMenuBar)menuBars.elementAt(i)).processKeyBinding(key, e, JComponent.WHEN_IN_FOCUSED_WINDOW, pressed))
        return true;
    return false;
  }
  
  /**
   * Returns the Vector of JMenuBars associated with the top-level
   * @param c the top-level container whose JMenuBar Vector we want
   * @return the Vector of JMenuBars for this top level container
   */
  Vector getVectorForTopLevel(Container c)
  {
    Vector result = (Vector) menuBarLookup.get(c);
    if (result == null)
      {
        result = new Vector();
        menuBarLookup.put (c, result);
      }
    return result;
  }
  
  /**
   * In processKeyStroke, KeyManager must give all JMenuBars in the 
   * focused top-level container a chance to process the event.  So, 
   * JMenuBars must be registered in KeyManager and associated with a 
   * top-level container.  That's what this method is for.
   * @param menuBar the JMenuBar to register
   */
  public void registerJMenuBar (JMenuBar menuBar)
  {
    Container topLevel = findTopLevel(menuBar);
    Vector menuBars = getVectorForTopLevel(topLevel);
    if (!menuBars.contains(menuBar))
      menuBars.add(menuBar);
  }
  
  /**
   * Unregisters a JMenuBar from its top-level container.  This is 
   * called before the JMenuBar is actually removed from the container
   * so findTopLevel will still find us the correct top-level container.
   * @param menuBar the JMenuBar to unregister.
   */
  public void unregisterJMenuBar (JMenuBar menuBar)
  {
    Container topLevel = findTopLevel(menuBar);
    Vector menuBars = getVectorForTopLevel(topLevel);
    if (menuBars.contains(menuBar))
      menuBars.remove(menuBar);
  }
}
