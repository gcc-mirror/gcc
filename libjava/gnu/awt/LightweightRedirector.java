/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt;

import java.awt.AWTEvent;
import java.awt.AWTError;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.MouseEvent;
import java.awt.event.InputEvent;

/**
 * Encapsulates the logic required to dispatch events to the correct
 * component in a component tree that may contain lightweight
 * components. Toolkits typically only identify heavyweight components
 * as the source of events. This class redirects the events to the
 * appropriate lightweight children of the heavyweight component.
 */
public class LightweightRedirector
{
  final static int LAST_BUTTON_NUMBER = 3;

  /* We sacrifice one array element to allow the button number to 
     match the index of this array. */
  Component[] releaseTargets = new Component[LAST_BUTTON_NUMBER+1];

  /** 
   *
   * Modifies or replaces the given event with an event that has been
   * properly redirected.  State of button presses are kept so that
   * button releases can be redirected to the same component as the
   * button press.  It is required that all events are sent through
   * this method in chronological order.
   */
  public AWTEvent redirect(AWTEvent event)
  {
    if (event instanceof MouseEvent)
      return redirectMouse((MouseEvent) event);

    /* In case we don't know how to redirect the event, simply return
       the event unchanged. */
    return event;
  }

  MouseEvent redirectMouse(MouseEvent event)
  {
    int button = getButtonNumber(event);
    int id = event.getID();

    Component heavySource = (Component) event.getSource();
    Component source = heavySource;
    int x = event.getX();
    int y = event.getY();

    if (id == MouseEvent.MOUSE_RELEASED)
      {
	Component target = releaseTargets[button];

	if (target != null)
	  {
	    releaseTargets[button] = null;
	    source = target;

	    Component child = source;
	    while (child != heavySource)
	      {
		x -= child.getX();
		y -= child.getY();
		child = child.getParent();
		if (child == null)
		  System.err.println("warning, orphaned release target");
	      }
	  }
      }
    else
      {
	/* Find real component, and adjust source, x and y
	   accordingly. */
	
	while (true)
	  {
	    Component parent = source;
	    
	    Component child = parent.getComponentAt(x, y);
	    
	    if (parent == child)
	      break;
	    
	    // maybe ignoring would be better?
	    if (child == null)
	      {
		String msg = "delivered event not within component. " +
		  "Heavyweight source was " + heavySource + ". " +
		  "Component was " + parent;
		throw new AWTError(msg);
	      }
	    if (child.isLightweight())
	      {
		// descend down to child
		source = child;
		x -= child.getX();
		y -= child.getY();
	      }
	    else
	      {
		System.err.println("warning: event delivered to wrong " +
				   "heavyweight component. Was " +
				   "delivered to " + source + ". " +
				   "Should have been delivered to " +
				   child + ". Maybe the native window " +
				   "system is bubbling events up the " +
				   "containment hierarchy.");
		break;
	      }
	  }
	
	/* ensure that the release event is delivered to the same
	   component as the press event. For most toolkits this is
	   only necessary for lightweight components, since the
	   underlying windowing system takes care of its heavyweight
	   components. */
	if (id == MouseEvent.MOUSE_PRESSED)
	  releaseTargets[button] = source;
      }
    
    
    if (source == heavySource)
      return event; // no change in event
    
    // print warning for heavyweights
    /* this warning can safely be removed if a toolkit that
       needs heavyweight redirection support is ever created. */
    if (!source.isLightweight())
      System.err.println("warning: redirecting to heavyweight");
    
    MouseEvent redirected = new MouseEvent(source, event.getID(),
					   event.getWhen(),
					   event.getModifiers(),
					   x, y,
					   event.getClickCount(),
					   event.isPopupTrigger());
    
    return redirected;
  }
  
  /**
   * Identifies the button number for an input event.
   * 
   * @returns the button number, or 0 if no button modifier was set
   * for the event.
   */
  int getButtonNumber(InputEvent event)
  {
    int modifiers = event.getModifiers();
    
    modifiers &=
      InputEvent.BUTTON1_MASK |
      InputEvent.BUTTON2_MASK |
      InputEvent.BUTTON3_MASK;
    
    switch (modifiers)
      {
      case InputEvent.BUTTON1_MASK:
	return 1;
      case InputEvent.BUTTON2_MASK:
	return 2;
      case InputEvent.BUTTON3_MASK:
	return 3;
      case 0:
	return 0;

      default:
	System.err.println("FIXME: multibutton event");
	return 0;
      }
  }
}
