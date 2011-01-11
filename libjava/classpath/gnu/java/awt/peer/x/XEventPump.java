/* XEventPump.java -- Pumps events from X to AWT
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


package gnu.java.awt.peer.x;

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.PaintEvent;
import java.awt.event.WindowEvent;
import java.util.HashMap;

import gnu.java.awt.ComponentReshapeEvent;
import gnu.x11.Atom;
import gnu.x11.Display;
import gnu.x11.event.ButtonPress;
import gnu.x11.event.ButtonRelease;
import gnu.x11.event.ClientMessage;
import gnu.x11.event.ConfigureNotify;
import gnu.x11.event.DestroyNotify;
import gnu.x11.event.Event;
import gnu.x11.event.Expose;
import gnu.x11.event.Input;
import gnu.x11.event.KeyPress;
import gnu.x11.event.KeyRelease;
import gnu.x11.event.MotionNotify;
import gnu.x11.event.PropertyNotify;
import gnu.x11.event.ResizeRequest;
import gnu.x11.event.UnmapNotify;

/**
 * Fetches events from X, translates them to AWT events and pumps them up
 * into the AWT event queue.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class XEventPump
  implements Runnable
{

  /**
   * The X Display from which we fetch and pump up events.
   */
  private Display display;

  /**
   * Maps X Windows to AWT Windows to be able to correctly determine the
   * event targets.
   */
  private HashMap windows;

  /**
   * Indicates if we are currently inside a drag operation. This is
   * set to the button ID when a button is pressed and to -1 (indicating
   * that no drag is active) when the mouse is released.
   */
  private int drag;

  /**
   * Creates a new XEventPump for the specified X Display.
   *
   * @param d the X Display
   */
  XEventPump(Display d)
  {
    display = d;
    windows = new HashMap();
    drag = -1;
    Thread thread = new Thread(this, "X Event Pump");
    thread.setDaemon(true);
    thread.start();
  }

  /**
   * The main event pump loop. This basically fetches events from the
   * X Display and pumps them into the system event queue.
   */
  public void run()
  {
    while (display.connected)
      {
        try
          {
            Event xEvent = display.next_event();
            handleEvent(xEvent);
          }
        catch (ThreadDeath death)
          {
            // If someone wants to kill us, let them.
            return;
          }
        catch (Throwable x)
          {
            System.err.println("Exception during event dispatch:");
            x.printStackTrace(System.err);
          }
      }
  }

  /**
   * Adds an X Window to AWT Window mapping. This is required so that the
   * event pump can correctly determine the event targets.
   *
   * @param xWindow the X Window
   * @param awtWindow the AWT Window
   */
  void registerWindow(gnu.x11.Window xWindow, Window awtWindow)
  {
    if (XToolkit.DEBUG)
      System.err.println("registering window id: " + xWindow.id);
    windows.put(new Integer(xWindow.id), awtWindow);
  }

  void unregisterWindow(gnu.x11.Window xWindow)
  {
    windows.remove(new Integer(xWindow.id));
  }

  private void handleButtonPress(ButtonPress event)
  {
    Integer key = new Integer(event.getEventWindowID());
    Window awtWindow = (Window) windows.get(key);

    // Create and post the mouse event.
    int button = event.detail();

    // AWT cannot handle more than 3 buttons and expects 0 instead.
    if (button >= gnu.x11.Input.BUTTON3)
      button = 0;
    drag = button;

    Component target =
      findMouseEventTarget(awtWindow, event.getEventX(), event.getEventY());
    if(target == null)
      {
        target = awtWindow;
      }

    MouseEvent mp = new MouseEvent(target, MouseEvent.MOUSE_PRESSED,
                                   System.currentTimeMillis(),
                                   KeyboardMapping.mapModifiers(event.getState())
                                     | buttonToModifier(button),
                                   event.getEventX(), event.getEventY(),
                                   1, false, button);
    Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(mp);
  }

  private void handleButtonRelease(ButtonRelease event)
  {
    Integer key = new Integer(event.getEventWindowID());
    Window awtWindow = (Window) windows.get(key);

    int button = event.detail();

    // AWT cannot handle more than 3 buttons and expects 0 instead.
    if (button >= gnu.x11.Input.BUTTON3)
      button = 0;
    drag = -1;

    Component target =
      findMouseEventTarget(awtWindow, event.getEventX(), event.getEventY());
    if(target == null)
      {
        target = awtWindow;
      }

    MouseEvent mr = new MouseEvent(target, MouseEvent.MOUSE_RELEASED,
                                   System.currentTimeMillis(),
                                   KeyboardMapping.mapModifiers(event.getState())
                                     | buttonToModifier(button),
                                   event.getEventX(), event.getEventY(),
                                   1, false, button);
    Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(mr);
  }


  private void handleMotionNotify(MotionNotify event)
  {
    Integer key = new Integer(event.getEventWindowID());
    Window awtWindow = (Window) windows.get(key);

    int button = event.detail();

    // AWT cannot handle more than 3 buttons and expects 0 instead.
    if (button >= gnu.x11.Input.BUTTON3)
      button = 0;

    MouseEvent mm = null;
    if (drag == -1)
      {
        mm = new MouseEvent(awtWindow, MouseEvent.MOUSE_MOVED,
                            System.currentTimeMillis(),
                            KeyboardMapping.mapModifiers(event.getState())
                              | buttonToModifier(button),
                            event.getEventX(), event.getEventY(),
                            1, false);

      }
    else
      {
        mm = new MouseEvent(awtWindow, MouseEvent.MOUSE_DRAGGED,
                            System.currentTimeMillis(),
                            KeyboardMapping.mapModifiers(event.getState())
                              | buttonToModifier(drag),
                            event.getEventX(), event.getEventY(),
                            1, false);
      }
    Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(mm);
  }

  // FIME: refactor and make faster, maybe caching the event and handle
  // and/or check timing (timing is generated for PropertyChange)?
  private void handleExpose(Expose event)
  {
    Integer key = new Integer(event.window_id);
    Window awtWindow = (Window) windows.get(key);

    if (XToolkit.DEBUG)
      System.err.println("expose request for window id: " + key);

    Rectangle r = new Rectangle(event.x(), event.y(), event.width(),
                                event.height());
    // We need to clear the background of the exposed rectangle.
    assert awtWindow != null : "awtWindow == null for window ID: " + key;

    Graphics g = awtWindow.getGraphics();
    g.clearRect(r.x, r.y, r.width, r.height);
    g.dispose();

    XWindowPeer xwindow = (XWindowPeer) awtWindow.getPeer();
    Insets i = xwindow.insets();
    if (event.width() != awtWindow.getWidth() - i.left - i.right
        || event.height() != awtWindow.getHeight() - i.top - i.bottom)
      {
        int w = event.width();
        int h = event.height();
        int x = xwindow.xwindow.x;
        int y = xwindow.xwindow.y;

        if (XToolkit.DEBUG)
          System.err.println("Setting size on AWT window: " + w
                           + ", " + h + ", " + awtWindow.getWidth()
                           + ", " + awtWindow.getHeight());

        // new width and height
        xwindow.xwindow.width = w;
        xwindow.xwindow.height = h;

        // reshape the window
        ComponentReshapeEvent cre =
          new ComponentReshapeEvent(awtWindow, x, y, w, h);
        awtWindow.dispatchEvent(cre);
      }

    ComponentEvent ce =
      new ComponentEvent(awtWindow, ComponentEvent.COMPONENT_RESIZED);
    awtWindow.dispatchEvent(ce);

    PaintEvent pev = new PaintEvent(awtWindow, PaintEvent.UPDATE, r);
    Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(pev);
  }

  private void handleDestroyNotify(DestroyNotify destroyNotify)
  {
    if (XToolkit.DEBUG)
      System.err.println("DestroyNotify event: " + destroyNotify);

    Integer key = new Integer(destroyNotify.event_window_id);
    Window awtWindow = (Window) windows.get(key);

    AWTEvent event = new WindowEvent(awtWindow, WindowEvent.WINDOW_CLOSED);
    Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(event);
  }

  private void handleClientMessage(ClientMessage clientMessage)
  {
    if (XToolkit.DEBUG)
      System.err.println("ClientMessage event: " + clientMessage);

    if (clientMessage.delete_window())
      {
        if (XToolkit.DEBUG)
          System.err.println("ClientMessage is a delete_window event");

        Integer key = new Integer(clientMessage.window_id);
        Window awtWindow = (Window) windows.get(key);

        AWTEvent event = new WindowEvent(awtWindow, WindowEvent.WINDOW_CLOSING);
        Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(event);
      }
  }

  private void handleEvent(Event xEvent)
  {
    if (XToolkit.DEBUG)
      System.err.println("fetched event: " + xEvent);

    switch (xEvent.code() & 0x7f)
    {
    case ButtonPress.CODE:
      this.handleButtonPress((ButtonPress) xEvent);
      break;
    case ButtonRelease.CODE:
      this.handleButtonRelease((ButtonRelease) xEvent);
      break;
    case MotionNotify.CODE:
      this.handleMotionNotify((MotionNotify) xEvent);
      break;
    case Expose.CODE:
      this.handleExpose((Expose) xEvent);
      break;
    case KeyPress.CODE:
    case KeyRelease.CODE:
      Integer key = new Integer(((Input) xEvent).getEventWindowID());
      Window awtWindow = (Window) windows.get(key);
      handleKeyEvent(xEvent, awtWindow);
      break;
    case DestroyNotify.CODE:
      this.handleDestroyNotify((DestroyNotify) xEvent);
      break;
    case ClientMessage.CODE:
      this.handleClientMessage((ClientMessage) xEvent);
      break;
    case PropertyNotify.CODE:
      key = new Integer (((PropertyNotify) xEvent).getWindowID());
      awtWindow = (Window) windows.get(key);
      AWTEvent event = new WindowEvent(awtWindow, WindowEvent.WINDOW_STATE_CHANGED);
      Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(event);
      break;
    default:
      if (XToolkit.DEBUG)
        System.err.println("Unhandled X event: " + xEvent);
    }
  }

  /**
   * Handles key events from X.
   *
   * @param xEvent the X event
   * @param awtWindow the AWT window to which the event gets posted
   */
  private void handleKeyEvent(Event xEvent, Window awtWindow)
  {
    Input keyEvent = (Input) xEvent;
    int xKeyCode = keyEvent.detail();
    int xMods = keyEvent.getState();
    int keyCode = KeyboardMapping.mapToKeyCode(xEvent.display.input, xKeyCode,
                                               xMods);
    char keyChar = KeyboardMapping.mapToKeyChar(xEvent.display.input, xKeyCode,
                                                xMods);
    if (XToolkit.DEBUG)
      System.err.println("XEventPump.handleKeyEvent: " + xKeyCode + ", "
                         + xMods + ": " + ((int) keyChar) + ", " + keyCode);
    int awtMods = KeyboardMapping.mapModifiers(xMods);
    long when = System.currentTimeMillis();
    KeyEvent ke;
    if (keyEvent.code() == KeyPress.CODE)
      {
        ke = new KeyEvent(awtWindow, KeyEvent.KEY_PRESSED, when,
                          awtMods, keyCode,
                          KeyEvent.CHAR_UNDEFINED);
        Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(ke);
        if (keyChar != KeyEvent.CHAR_UNDEFINED)
          {
            ke = new KeyEvent(awtWindow, KeyEvent.KEY_TYPED, when,
                              awtMods, KeyEvent.VK_UNDEFINED,
                              keyChar);
            Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(ke);
          }

      }
    else
      {
        ke = new KeyEvent(awtWindow, KeyEvent.KEY_RELEASED, when,
                          awtMods, keyCode,
                          KeyEvent.CHAR_UNDEFINED);
        Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(ke);
      }

  }

  /** Translates an X button identifier to the AWT's MouseEvent modifier
   *  mask. As the AWT cannot handle more than 3 buttons those return
   *  <code>0</code>.
   */
  static int buttonToModifier(int button)
  {
    switch (button)
    {
      case gnu.x11.Input.BUTTON1:
        return MouseEvent.BUTTON1_DOWN_MASK | MouseEvent.BUTTON1_MASK;
      case gnu.x11.Input.BUTTON2:
        return MouseEvent.BUTTON2_DOWN_MASK | MouseEvent.BUTTON2_MASK;
      case gnu.x11.Input.BUTTON3:
        return MouseEvent.BUTTON3_DOWN_MASK | MouseEvent.BUTTON3_MASK;
    }

    return 0;
  }

  /**
   * Finds the heavyweight mouse event target.
   *
   * @param src the original source of the event
   *
   * @param pt the event coordinates
   *
   * @return the real mouse event target
   */
  private Component findMouseEventTarget(Component src, int x, int y)
  {
    Component found = null;
    if (src instanceof Container)
      {
        Container cont = (Container) src;
        int numChildren = cont.getComponentCount();
        for (int i = 0; i < numChildren && found == null; i++)
          {
            Component child = cont.getComponent(i);
            if (child != null && child.isVisible()
                && child.contains(x - child.getX(), y - child.getY()))
              {
                if (child instanceof Container)
                  {
                    Component deeper = findMouseEventTarget(child,
                                                            x - child.getX(),
                                                            y - child.getY());
                    if (deeper != null)
                      found = deeper;
                  }
                else if (! child.isLightweight())
                  found = child;
              }
          }
      }

    // Consider the source itself.
    if (found == null && src.contains(x, y) && ! src.isLightweight())
      found = src;

    return found;
  }
}
