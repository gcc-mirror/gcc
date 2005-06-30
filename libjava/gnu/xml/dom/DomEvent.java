/* DomEvent.java -- 
   Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.

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

package gnu.xml.dom;

import org.w3c.dom.*;
import org.w3c.dom.events.*;
import org.w3c.dom.views.AbstractView;		// used by UIEvent

/**
 * "Event" implementation.  Events are
 * created (through DocumentEvent interface methods on the document object),
 * and are sent to any target node in the document.
 *
 * <p> Applications may define application specific event subclasses, but
 * should otherwise use the <em>DocumentTraversal</em> interface to acquire
 * event objects.
 *
 * @author David Brownell
 */
public class DomEvent
  implements Event
{
  
  String  type;  // init
  EventTarget  target;
  EventTarget  currentNode;
  short  eventPhase;
  boolean  bubbles; // init
  boolean  cancelable; // init
  long  timeStamp; // ?

  /** Returns the event's type (name) as initialized */
  public final String getType()
  {
    return type;
  }

  /**
   * Returns event's target; delivery of an event is initiated
   * by a <em>target.dispatchEvent(event)</em> invocation.
   */
  public final EventTarget getTarget()
  {
    return target;
  }

  /**
   * Returns the target to which events are currently being
   * delivered.  When capturing or bubbling, this will not
   * be what <em>getTarget</em> returns.
   */
  public final EventTarget getCurrentTarget()
  {
    return currentNode;
  }

  /**
   * Returns CAPTURING_PHASE, AT_TARGET, or BUBBLING;
   * only meaningful within EventListener.handleEvent
   */
  public final short getEventPhase()
  {
    return eventPhase;
  }

  /**
   * Returns true if the news of the event bubbles to tree tops
   * (as specified during initialization).
   */
  public final boolean getBubbles()
  {
    return bubbles;
  }

  /**
   * Returns true if the default handling may be canceled
   * (as specified during initialization).
   */
  public final boolean getCancelable()
  {
    return cancelable;
  }

  /**
   * Returns the event's timestamp.
   */
  public final long getTimeStamp()
  {
    return timeStamp;
  }
  
  boolean stop;
  boolean doDefault;
  
  /**
   * Requests the event no longer be captured or bubbled; only
   * listeners on the event target will see the event, if they
   * haven't yet been notified.
   *
   * <p> <em> Avoid using this </em> except for application-specific
   * events, for which you the protocol explicitly "blesses" the use
   * of this with some event types.  Otherwise, you are likely to break
   * algorithms which depend on event notification either directly or
   * through bubbling or capturing.  </p>
   *
   * <p> Note that this method is not final, specifically to enable
   * enforcing of policies about events always propagating. </p>
   */
  public void stopPropagation()
  {
    stop = true;
  }

  /**
   * Requests that whoever dispatched the event not perform their
   * default processing when event delivery completes.  Initializes
   * event timestamp.
   */
  public final void preventDefault()
  {
    doDefault = false;
  }

  /** Initializes basic event state.  */
  public void initEvent(String typeArg,
                        boolean canBubbleArg,
                        boolean cancelableArg)
  {
    eventPhase = 0;
    type = typeArg;
    bubbles = canBubbleArg;
    cancelable = cancelableArg;
    timeStamp = System.currentTimeMillis();
  }
  
  /** Constructs, but does not initialize, an event. */
  public DomEvent(String type)
  {
    this.type = type;
  }

  /**
   * Returns a basic printable description of the event's type,
   * state, and delivery conditions
   */
  public String toString()
  {
    StringBuffer buf = new StringBuffer("[Event ");
    buf.append(type);
    switch (eventPhase)
      {
      case CAPTURING_PHASE:
        buf.append(", CAPTURING");
        break;
      case AT_TARGET:
        buf.append(", AT TARGET");
        break;
      case BUBBLING_PHASE:
        buf.append(", BUBBLING");
        break;
      default:
        buf.append(", (inactive)");
        break;
      }
    if (bubbles && eventPhase != BUBBLING_PHASE)
      {
        buf.append(", bubbles");
      }
    if (cancelable)
      {
        buf.append(", can cancel");
      }
    // were we to provide subclass info, this's where it'd live
    buf.append("]");
    return buf.toString();
  }
  
  /**
   * "MutationEvent" implementation.
   */
  public static final class DomMutationEvent
    extends DomEvent
    implements MutationEvent
  {
    
    // package private
    Node   relatedNode; // init
    
    private String  prevValue; // init
    private String  newValue; // init
    
    private String  attrName; // init
    private short  attrChange; // init
    
    /** Returns any "related" node provided by this type of event */
    public final Node getRelatedNode()
    {
      return relatedNode;
    }
    
    /** Returns any "previous value" provided by this type of event */
    public final String getPrevValue()
    {
      return prevValue;
    }
    
    /** Returns any "new value" provided by this type of event */
    public final String getNewValue()
    {
      return newValue;
    }
    
    /** For attribute change events, returns the attribute's name */
    public final String getAttrName()
    {
      return attrName;
    }
    
    /** For attribute change events, returns how the attribuet changed */
    public final short getAttrChange()
    {
      return attrChange;
    }
    
    /** Initializes a mutation event */
    public final void initMutationEvent(String typeArg,
                                        boolean canBubbleArg,
                                        boolean cancelableArg,
                                        Node relatedNodeArg,
                                        String prevValueArg,
                                        String newValueArg,
                                        String attrNameArg,
                                        short attrChangeArg)
    {
      // super.initEvent is inlined here for speed
      // (mutation events are issued on all DOM changes)
      eventPhase = 0;
      type = typeArg;
      bubbles = canBubbleArg;
      cancelable = cancelableArg;
      timeStamp = System.currentTimeMillis();
      
      relatedNode = relatedNodeArg;
      prevValue = prevValueArg;
      newValue = newValueArg;
      attrName = attrNameArg;
      attrChange = attrChangeArg;
    }

    // clear everything that should be GC-able
    void clear()
    {
      type = null;
      target = null;
      relatedNode = null;
      currentNode = null;
      prevValue = newValue = attrName = null;
    }

    /** Constructs an uninitialized mutation event. */
    public DomMutationEvent(String type)
    {
      super(type);
    }
  
  }

  /**
   * "UIEvent" implementation.
   */
  public static class DomUIEvent
    extends DomEvent
    implements UIEvent
  {
    
    private AbstractView view;  // init
    private int  detail;  // init
    
    /** Constructs an uninitialized User Interface (UI) event */
    public DomUIEvent (String type) { super (type); }
    
    public final AbstractView getView () { return view; }
    public final int getDetail () { return detail; }
    
    /** Initializes a UI event */
    public final void initUIEvent(String typeArg,
                                  boolean canBubbleArg,
                                  boolean cancelableArg,
                                  AbstractView viewArg,
                                  int detailArg)
    {
      super.initEvent(typeArg, canBubbleArg, cancelableArg);
      view = viewArg;
      detail = detailArg;
    }
  
  }

    /*

    static final class DomMouseEvent extends DomUIEvent
 implements MouseEvent
    {
 // another half dozen state variables/accessors
    }

    */

}

