/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;
import java.awt.image.*;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.lang.reflect.*;
import java.util.EventListener;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Vector;
import java.awt.peer.ComponentPeer;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;
// import javax.accessibility.AccessibleContext;

/* Status: Incomplete. The event dispatch mechanism is implemented. All 
   other methods defined in the J2SE 1.3 API javadoc exist, but are mostly 
   incomplete or only stubs; except for methods relating to the Drag and Drop, 
   Input Method, and Accessibility frameworks: These methods are present but 
   commented out. */

public abstract class Component implements ImageObserver, MenuContainer, 
					   java.io.Serializable
{

  /* Constants for use with getAlignmentX()/getAlignmentY(). */
  public static final float BOTTOM_ALIGNMENT = 1.0f,
			    CENTER_ALIGNMENT = 0.5f,
			    LEFT_ALIGNMENT   = 0.0f,
			    RIGHT_ALIGNMENT  = 1.0f,
			    TOP_ALIGNMENT    = 0.0f;

  /* Serialized fields from the serialization spec. */
  // FIXME: Default values?
  int x;
  int y;
  int width;
  int height;
  Color foreground;
  Color background;
  Font font;
  Font peerFont;
  Cursor cursor;
  Locale locale;
  boolean visible;
  boolean enabled;
  boolean valid;
  boolean hasFocus;
  //DropTarget dropTarget;
  Vector popups;
  String name;
  boolean nameExplicitlySet;
  Dimension minSize;
  Dimension prefSize;
  boolean newEventsOnly;  
  long eventMask;
  PropertyChangeSupport changeSupport;
  boolean isPacked;
  int componentSerializedDataVersion;
  /* AccessibleContext accessibleContext; */

  /* Anything else is non-serializable, and should be declared "transient". */
  transient Container parent;
  transient java.awt.peer.ComponentPeer peer;

  transient Object treeLock;

  transient ComponentListener componentListener;
  transient FocusListener focusListener;
  transient KeyListener keyListener;
  transient MouseListener mouseListener;
  transient MouseMotionListener mouseMotionListener;
  transient InputMethodListener inputMethodListener;
  transient HierarchyListener hierarchyListener;
  transient HierarchyBoundsListener hierarchyBoundsListener;

  protected Component()
  {
  }

  public String getName()
  {
    if (name == null && !nameExplicitlySet)
      name = generateName();
    return name;
  }
  
  public void setName(String name)
  {
    nameExplicitlySet = true;
    this.name = name;
  }
  
  /** Subclasses should override this to return unique component names like 
    * "menuitem0".
    */
  String generateName()
  {
    // Component is abstract.
    return null;
  }
  
  public Container getParent()
  {
    return parent;  
  }
  
  /** @deprecated */
  // However, Classpath's Gtk peers rely on it.
  public java.awt.peer.ComponentPeer getPeer()
  {
    return peer;
  }
  
  // FIXME: java.awt.dnd classes not yet implemented
  /*
  public void setDropTarget(DropTarget dt)
  {
    this.dropTarget = dt;
  }
  
  public DropTarget getDropTarget()
  {
    return dropTarget;
  }
  */
  
  /** @since 1.3 */
  public GraphicsConfiguration getGraphicsConfiguration()
  {
    // FIXME
    return null;
  }

  public final Object getTreeLock()
  {
    return treeLock;
  }

  public Toolkit getToolkit()
  {
    if (peer != null)
      return peer.getToolkit ();
    if (parent != null)
      return parent.getToolkit ();
    return Toolkit.getDefaultToolkit ();
  }

  public boolean isValid()
  {
    return valid;
  }
  
  /** @since 1.2 */
  public boolean isDisplayable()
  {
    // FIXME
    return false;
  }
  
  public boolean isVisible()
  {
    return visible;
  }
  
  public boolean isShowing()
  {
    if (! visible)
      return false;

    if (parent != null)
      return (parent.isShowing());

    return true;
  }
  
  public boolean isEnabled()
  {
    return enabled;
  }
  
  public void setEnabled(boolean b)
  {
    this.enabled = b;
  }
  
  /** @deprecated */
  public void enable()
  {
    setEnabled(true);
  }
  
  /** @deprecated */
  public void enable(boolean b)
  {
    setEnabled(b);
  }
  
  /** @deprecated */
  public void disable()
  {
    setEnabled(false);
  }
  
  public boolean isDoubleBuffered()
  {
    return false;
  }
  
  /** @since 1.2 */
  public void enableInputMethods(boolean enable)
  {
    // FIXME
  }
  
  public void setVisible(boolean b)
  {
    visible = true;
    // FIXME
  }
  
  /** @deprecated */
  public void show()
  {
    setVisible(true);
  }
  
  /** @deprecated */
  public void show(boolean b)
  {
    setVisible(b);
  }
  
  /** @deprecated */
  public void hide()
  {
    setVisible(false);
  }
  
  public Color getForeground()
  {
    return this.foreground;
  }
  
  public void setForeground(Color c)
  {
    this.foreground = c;
  }
  
  public Color getBackground()
  {
    return this.background;
  }
  
  public void setBackground(Color c)
  {
    this.background = c;
  }
  
  public Font getFont()
  {
    return this.font;
  }
  
  public void setFont(Font f)
  {
    this.font = f;
  }

  public Locale getLocale() throws IllegalComponentStateException
  {
    if (locale != null)
      return locale;
    if (parent == null)
      throw new IllegalComponentStateException
        ("Component has no parent: Can not determine Locale");
    return parent.getLocale();
  }
  
  public void setLocale(Locale l)  
  {
    this.locale = l;
  }
  
  public ColorModel getColorModel()
  {
    // FIXME
    return null;
  }

  public Point getLocation()
  {
    return new Point(x, y);
  }

  public Point getLocationOnScreen()
  {
    // FIXME
    return null;
  }

  /** @deprecated Use getLocation() instead. */
  public Point location()
  {
    return getLocation();
  }

  public void setLocation (int x, int y)
  {
    this.x = x;
    this.y = y;
    if (peer != null)
      peer.setBounds(x, y, width, height);
  }

  /** @deprecated */
  public void move(int x, int y)
  {
    setLocation(x,y);
  }
  
  public void setLocation(Point p)
  {
    setLocation(p.x, p.y);
  }
  
  public Dimension getSize()
  {
    return new Dimension(width, height);
  }
  
  /** @deprecated */
  public Dimension size()
  {
    return getSize();
  }
  
  public void setSize(int width, int height)
  {
    this.width = width;
    this.height = height;
    if (peer != null)
      peer.setBounds(x, y, width, height);
  }
  
  /** @deprecated */
  public void resize(int width, int height)
  {
    setSize(width, height);
  }
  
  public void setSize(Dimension d)
  {
    setSize(d.width, d.height);
  }

  /** @deprecated */
  public void resize(Dimension d)
  {
    setSize(d.width, d.height);
  }

  public Rectangle getBounds()
  {
    return new Rectangle (x, y, width, height);
  }

  /** @deprecated */
  public Rectangle bounds()
  {
    return getBounds();
  }
  
  public void setBounds(int x, int y, int w, int h)
  {
    this.x = x;
    this.y = y;
    this.width = w;
    this.height = h;
    if (peer != null)
      peer.setBounds(x, y, w, h);
  }
  
  /** @deprecated */
  public void reshape(int x, int y, int width, int height)
  {
    setBounds(x, y, width, height);
  }
  
  public void setBounds(Rectangle r)
  { 
    setBounds(r.x, r.y, r.width, r.height);
  }
  
  /** @since 1.2 */
  public int getX()
  {
    return x;
  }
  
  /** @since 1.2 */
  public int getY()
  {
    return y;
  }
  
  /** @since 1.2 */
  public int getWidth()
  {
    return width;
  }
  
  /** @since 1.2 */
  public int getHeight()
  {
    return height;
  }
  
  public Rectangle getBounds(Rectangle r)
  {
    r.x = this.x;
    r.y = this.y;
    r.width = this.width;
    r.height = this.height;
    return r;
  }
  
  public Dimension getSize(Dimension d)
  {
    d.width = this.width;
    d.height = this.height;
    return d;
  }
  
  public Point getLocation(Point p)
  {
    p.x = x;
    p.y = y;
    return p;
  }
  
  /** @since 1.2 */
  public boolean isOpaque()
  {
    return false;
  }
  
  /** @since 1.2 */  
  public boolean isLightweight()
  {
    // FIXME
    return false;
  }
  
  public Dimension getPreferredSize()
  {
    // FIXME?
    if (peer == null)
      return new Dimension(width, height);
    else
      return peer.getPreferredSize();
  }

  /** @deprecated */
  public Dimension preferredSize()
  {
    return getPreferredSize();
  }
  
  public Dimension getMinimumSize()
  {
    // FIXME?
    if (peer == null)
      return new Dimension(width, height);
    else
      return peer.getMinimumSize();
  }

  /** @deprecated */
  public Dimension minimumSize()
  {
    return getMinimumSize();
  }
  
  public Dimension getMaximumSize()
  {
    // FIXME
    return null;
  }
  
  public float getAlignmentX()
  {
    // FIXME
    return 0;
  }
  
  public float getAlignmentY()
  {
    // FIXME
    return 0;
  }
  
  public void doLayout()
  {
    // FIXME
  }
  
  /** @deprecated */
  public void layout()
  {
    doLayout();
  }
  
  public void validate()
  {
    // FIXME
  }
  
  public void invalidate()
  {
    valid = false;
    if (parent != null)
      parent.invalidate ();
  }
  
  public Graphics getGraphics()
  {
    // FIXME
    return null;
  }
  
  public FontMetrics getFontMetrics(Font font)
  {
    // FIXME
    return null;
  }
  
  public void setCursor(Cursor cursor)
  {
    this.cursor = cursor;
  }
  
  public Cursor getCursor()
  {
    return this.cursor;
  }
  
  public void paint(Graphics g)
  {  
  }
  
  public void update(Graphics g)
  {
    // FIXME
  }
  
  public void paintAll(Graphics g)
  {    
  }
  
  public void repaint()
  {
    // FIXME
  }
  
  public void repaint(long tm)
  {
    // FIXME
  }
  
  public void repaint(int x, int y, int width, int height)
  {
    // FIXME  
  }
  
  public void repaint(long tm, int x, int y, int width, int height)
  {    
    // FIXME  
  }
  
  public void print(Graphics g)
  {
    // FIXME    
  }
  
  public void printAll(Graphics g)
  {
    // FIXME      
  }
  
  public boolean imageUpdate(Image img, int infoflags, int x, int y, int w, int h)
  {
    // FIXME
    return false;
  }
  
  public Image createImage(ImageProducer producer)
  {
    // FIXME
    return null;
  }
  
  public Image createImage(int width, int height)
  {
    // FIXME
    return null;
  }
  
  public boolean prepareImage(Image image, ImageObserver observer)
  {
    // FIXME
    return false;
  }
  
  public boolean prepareImage(Image image, int width, int height, ImageObserver observer)
  {
    // FIXME
    return false;
  }

  public int checkImage(Image image, ImageObserver observer)
  {
    // FIXME
    return false;
  }
  
  public int checkImage(Image image, int width, int height, ImageObserver observer)
  {
    // FIXME
    return 0; 
  }
  
  public boolean contains(int x, int y)
  {
    return (x >= 0) && (y >= 0) && (x < width) && (y < height);
  }
  
  /** @deprecated */
  public boolean inside(int x, int y)
  {
    return contains(x,y);
  }
  
  public boolean contains(Point p)
  {
    return contains(p.x, p.y);
  }
  
  public Component getComponentAt(int x, int y)
  {
    if (contains(x,y))
      return this;
    return null;
  }
  
  /** @deprecated */
  public Component locate(int x, int y)
  {
    return getComponentAt(x, y);
  }
  
  public Component getComponentAt(Point p)
  {
    return getComponentAt(p.x, p.y);
  }
    
  /** @deprecated */
  public void deliverEvent(Event e)
  {
    
  }
  
  /** Forward AWT events to processEvent() if:
    *     - Events have been enabled for this type of event via enableEvents(),
    *   OR:
    *	 - There is at least one registered listener for this type of event
    * 
    * @specnote This method is final, but we need to be able to 
    *           override it in order to handle other event types in our 
    *	        subclasses. The solution is to define a second, non-final
    *           method - dispatchEventImpl() - to actually do the work. 
    *           Investigations with Thread.dumpStack() on the dispatch thread 
    *           in JDK 1.3 show Sun's implementation is doing the same 
    *           thing.
    */
  public final void dispatchEvent(AWTEvent e)
  {
    dispatchEventImpl(e);
  }
  /* This code needs to be split up and put in to dispatchEventImpl() in the
     appropriate Component subclasses:
   
    else if ((e.id <= WindowEvent.WINDOW_LAST
             && e.id >= WindowEvent.WINDOW_FIRST)
	&& (windowListener != null
	    || eventMask & AWTEvent.WINDOW_EVENT_MASK != 0))
      processEvent(e);
    else if ((e.id <= AdjustmentEvent.ADJUSTMENT_LAST
             && e.id >= AdjustmentEvent.ADJUSTMENT_FIRST)
	&& (adjustmentListener != null
	    || eventMask & AWTEvent.ADJUSTMENT_EVENT_MASK != 0))
      processEvent(e);
    else if ((e.id <= ItemEvent.ITEM_LAST
             && e.id >= ItemEvent.ITEM_FIRST)
	&& (itemListener != null
	    || eventMask & AWTEvent.ITEM_EVENT_MASK != 0))
      processEvent(e);
    else if ((e.id <= PaintEvent.PAINT_LAST
             && e.id >= PaintEvent.PAINT_FIRST)
	&& (paintListener != null
	    || eventMask & AWTEvent.PAINT_EVENT_MASK != 0))
      processEvent(e);
    else if ((e.id <= TextEvent.TEXT_LAST
             && e.id >= TextEvent.TEXT_FIRST)
	&& (textListener != null
	    || eventMask & AWTEvent.TEXT_EVENT_MASK != 0))
      processEvent(e);
    else if ((e.id <= InvocationEvent.INVOCATION_LAST
             && e.id >= InvocationEvent.INVOCATION_FIRST)
	&& (invocationListener != null
	    || eventMask & AWTEvent.INVOCATION_EVENT_MASK != 0))
      processEvent(e);
  }
  */      
  
  void dispatchEventImpl(AWTEvent e)
  {
    // Make use of event id's in order to avoid multiple instanceof tests.
    if (e.id <= ComponentEvent.COMPONENT_LAST 
        && e.id >= ComponentEvent.COMPONENT_FIRST
        && (componentListener != null 
	    || (eventMask & AWTEvent.COMPONENT_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= KeyEvent.KEY_LAST
             && e.id >= KeyEvent.KEY_FIRST
	     && (keyListener != null
		 || (eventMask & AWTEvent.KEY_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= MouseEvent.MOUSE_LAST
             && e.id >= MouseEvent.MOUSE_FIRST
	     && (mouseListener != null
		 || mouseMotionListener != null
		 || (eventMask & AWTEvent.MOUSE_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= FocusEvent.FOCUS_LAST
             && e.id >= FocusEvent.FOCUS_FIRST
	     && (focusListener != null
		 || (eventMask & AWTEvent.FOCUS_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= InputMethodEvent.INPUT_METHOD_LAST
             && e.id >= InputMethodEvent.INPUT_METHOD_FIRST
	     && (inputMethodListener != null
		 || (eventMask & AWTEvent.INPUT_METHOD_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= HierarchyEvent.HIERARCHY_LAST
             && e.id >= HierarchyEvent.HIERARCHY_FIRST
	     && (hierarchyListener != null
		 || hierarchyBoundsListener != null
		 || (eventMask & AWTEvent.HIERARCHY_EVENT_MASK) != 0))
      processEvent(e);
  }
  
  /** @deprecated */
  public boolean postEvent(Event e)
  {
    return false;
  }
  
  public synchronized void addComponentListener(ComponentListener l)
  {
    componentListener = AWTEventMulticaster.add(componentListener, l);
    if (componentListener != null)
      enableEvents(AWTEvent.COMPONENT_EVENT_MASK);
  }
  
  public synchronized void removeComponentListener(ComponentListener l)
  {
    componentListener = AWTEventMulticaster.remove(componentListener, l);
  }
  
  public synchronized void addFocusListener(FocusListener l)
  {
    focusListener = AWTEventMulticaster.add(focusListener, l);
    if (focusListener != null)
      enableEvents(AWTEvent.FOCUS_EVENT_MASK);    
  }
  
  public synchronized void removeFocusListener(FocusListener l)
  {
    focusListener = AWTEventMulticaster.remove(focusListener, l);
  }
  
  /** @since 1.3 */
  public synchronized void addHierarchyListener(HierarchyListener l)
  {
    hierarchyListener = AWTEventMulticaster.add(hierarchyListener, l);
    if (hierarchyListener != null)
      enableEvents(AWTEvent.HIERARCHY_EVENT_MASK);    
  }
  
  /** @since 1.3 */
  public synchronized void removeHierarchyListener(HierarchyListener l)
  {
    hierarchyListener = AWTEventMulticaster.remove(hierarchyListener, l);
  }

  /** @since 1.3 */
  public synchronized void addHierarchyBoundsListener(HierarchyBoundsListener l)
  {
    hierarchyBoundsListener = 
      AWTEventMulticaster.add(hierarchyBoundsListener, l);
    if (hierarchyBoundsListener != null)
      enableEvents(AWTEvent.HIERARCHY_EVENT_MASK);    
  }

  /** @since 1.3 */
  public synchronized void 
    removeHierarchyBoundsListener(HierarchyBoundsListener l)
  {
    hierarchyBoundsListener = 
      AWTEventMulticaster.remove(hierarchyBoundsListener, l);
  }

  public synchronized void addKeyListener(KeyListener l)
  {
    keyListener = AWTEventMulticaster.add(keyListener, l);
    if (keyListener != null)
      enableEvents(AWTEvent.KEY_EVENT_MASK);    
  }

  public synchronized void removeKeyListener(KeyListener l)
  {
    keyListener = AWTEventMulticaster.remove(keyListener, l);
  }

  public synchronized void addMouseListener(MouseListener l)
  {
    mouseListener = AWTEventMulticaster.add(mouseListener, l);
    if (mouseListener != null)
      enableEvents(AWTEvent.MOUSE_EVENT_MASK);    
  }

  public synchronized void removeMouseListener(MouseListener l)
  {
    mouseListener = AWTEventMulticaster.remove(mouseListener, l);    
  }

  public synchronized void addMouseMotionListener(MouseMotionListener l)
  {
    mouseMotionListener = AWTEventMulticaster.add(mouseMotionListener, l);
    if (mouseMotionListener != null)
      enableEvents(AWTEvent.MOUSE_EVENT_MASK);    
  }

  public synchronized void removeMouseMotionListener(MouseMotionListener l)
  {
    mouseMotionListener = AWTEventMulticaster.remove(mouseMotionListener, l);
  }

  /** @since 1.2 */
  public synchronized void addInputMethodListener(InputMethodListener l)
  {
    inputMethodListener = AWTEventMulticaster.add(inputMethodListener, l);
    if (inputMethodListener != null)
      enableEvents(AWTEvent.INPUT_METHOD_EVENT_MASK);    
  }

  /** @since 1.2 */
  public synchronized void removeInputMethodListener(InputMethodListener l)
  {
    inputMethodListener = AWTEventMulticaster.remove(inputMethodListener, l);
  }

  /** Returns all registered EventListers of the given listenerType. 
    * listenerType must be a subclass of EventListener, or a 
    * ClassClassException is thrown.
    * @since 1.3 
    */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ComponentListener.class)
      return getListenersImpl(listenerType, componentListener);
    else if (listenerType == FocusListener.class)
      return getListenersImpl(listenerType, focusListener);
    else if (listenerType == KeyListener.class)
      return getListenersImpl(listenerType, keyListener);
    else if (listenerType == MouseListener.class)
      return getListenersImpl(listenerType, mouseListener);
    else if (listenerType == MouseMotionListener.class)
      return getListenersImpl(listenerType, mouseMotionListener);
    else if (listenerType == InputMethodListener.class)
      return getListenersImpl(listenerType, inputMethodListener);
    else if (listenerType == HierarchyListener.class)
      return getListenersImpl(listenerType, hierarchyListener);
    else if (listenerType == HierarchyBoundsListener.class)
      return getListenersImpl(listenerType, hierarchyBoundsListener);
    else
      return getListenersImpl(listenerType, null);
  }
  
  static EventListener[] getListenersImpl(Class listenerType, EventListener el)
  {
    if (! EventListener.class.isAssignableFrom(listenerType))
      throw new ClassCastException();
    
    Vector v = new Vector();
    if (el != null)
      getListenerList (el, v);    
    EventListener[] el_a = (EventListener[]) Array.newInstance(listenerType, 
							       v.size());
    v.copyInto(el_a);
    return el_a;
  }
  
  static void getListenerList(EventListener el, Vector v)
  {
    if (el instanceof AWTEventMulticaster)
      {
        AWTEventMulticaster mc = (AWTEventMulticaster) el;
        getListenerList(mc.a, v);
	getListenerList(mc.b, v);
      }
    else
      v.addElement(el);      
  }

  // The input method framework is currently unimplemented.  
  // /** @since 1.2 */
  //
  //public InputMethodRequests getInputMethodRequests()
  //
  // /** @since 1.2 */
  //
  // public InputContext getInputContext()
  
  protected final void enableEvents(long eventsToEnable)
  {
    eventMask |= eventsToEnable;
    // TODO: Unlike Sun's implementation, I think we should try and 
    // enable/disable events at the peer (gtk/X) level. This will avoid 
    // clogging the event pipeline with useless mousemove events that 
    // we arn't interested in, etc. This will involve extending the peer 
    // interface, but thats okay because the peer interfaces have been
    // deprecated for a long time, and no longer feature in the 
    // API specification at all.
  }
  
  protected final void disableEvents(long eventsToDisable)
  {
    eventMask &= ~eventsToDisable;
    // forward new event mask to peer?
  }
  
  /** coalesceEvents is called by the EventQueue if two events with the same 
    * event id are queued. Returns a new combined event, or null if no 
    * combining is done. 
    */
  protected AWTEvent coalesceEvents(AWTEvent existingEvent, AWTEvent newEvent)
  {
    if (existingEvent instanceof MouseEvent
        && (existingEvent.id == MouseEvent.MOUSE_DRAGGED
	    || existingEvent.id == MouseEvent.MOUSE_MOVED))
      {
        // Just drop the old (intermediate) event and return the new one.
	return newEvent;
      }
    /*
    else if (existingEvent instanceof PaintEvent)
      {
        // The JDK 1.3 documentation says that in this case a complex 
	// RepaintArea is generated. We don't do that yet, and creating a 
	// union area as suggested by older documentation sounds ugly.
      }
    */
      
    // FIXME
    return null;
  }
  
  /** Forward event to the appropriate processXXXEvent method based on the
    * event type.
    */
  protected void processEvent(AWTEvent e)
  {
    if (e instanceof ComponentEvent)
      processComponentEvent((ComponentEvent) e);
    else if (e instanceof FocusEvent)
      processFocusEvent((FocusEvent) e);
    else if (e instanceof KeyEvent)
      processKeyEvent((KeyEvent) e);
    else if (e instanceof MouseEvent)
      {
        if (e.id == MouseEvent.MOUSE_MOVED 
	    || e.id == MouseEvent.MOUSE_DRAGGED)
	  processMouseMotionEvent((MouseEvent) e);
	else
	  processMouseEvent((MouseEvent) e);
      }
    else if (e instanceof InputMethodEvent)
      processInputMethodEvent((InputMethodEvent) e);
    else if (e instanceof HierarchyEvent)
      {
        if (e.id == HierarchyEvent.HIERARCHY_CHANGED)
	  processHierarchyEvent((HierarchyEvent) e);
	else
	  processHierarchyBoundsEvent((HierarchyEvent) e);
      }
  }
  
  protected void processComponentEvent(ComponentEvent e)
  {
    if (componentListener == null)
      return;
    switch (e.id)
      {
        case ComponentEvent.COMPONENT_HIDDEN:
	  componentListener.componentHidden(e);
	break;
		
        case ComponentEvent.COMPONENT_MOVED:
	  componentListener.componentMoved(e);
	break;
	
	case ComponentEvent.COMPONENT_RESIZED:
	  componentListener.componentResized(e);
	break;
	
	case ComponentEvent.COMPONENT_SHOWN:
	  componentListener.componentShown(e);
	break;
      }
  }
  
  protected void processFocusEvent(FocusEvent e)
  {
    if (focusListener == null)
      return;
    switch (e.id)
      {
        case FocusEvent.FOCUS_GAINED:
	  focusListener.focusGained(e);
	break;
        case FocusEvent.FOCUS_LOST:
	  focusListener.focusLost(e);
	break;
      }    
  }
  
  protected void processKeyEvent(KeyEvent e)
  {
    if (keyListener == null)
      return;
    switch (e.id)
      {
	case KeyEvent.KEY_PRESSED:
	  keyListener.keyPressed(e);
	break;
	case KeyEvent.KEY_RELEASED:
	  keyListener.keyReleased(e);
	break;
	case KeyEvent.KEY_TYPED:
	  keyListener.keyTyped(e);
	break;
      }
  }
  
  protected void processMouseEvent(MouseEvent e)
  {
    if (mouseListener == null)
      return;
    switch (e.id)
      {
	case MouseEvent.MOUSE_CLICKED:
	  mouseListener.mousePressed(e);
	break;
        case MouseEvent.MOUSE_ENTERED:
	  mouseListener.mouseEntered(e);
	break;
	case MouseEvent.MOUSE_EXITED:
	  mouseListener.mouseExited(e);
	break;
	case MouseEvent.MOUSE_PRESSED:
	  mouseListener.mousePressed(e);
	break;
	case MouseEvent.MOUSE_RELEASED:
	  mouseListener.mouseReleased(e);
	break;
      }
  }

  protected void processMouseMotionEvent(MouseEvent e)
  {
    if (mouseMotionListener == null)
      return;
    switch (e.id)
      {
	case MouseEvent.MOUSE_DRAGGED:
	  mouseMotionListener.mouseDragged(e);
	break;
        case MouseEvent.MOUSE_MOVED:
	  mouseMotionListener.mouseMoved(e);
	break;
      }	
  }
  
  /** @since 1.2 */
  protected void processInputMethodEvent(InputMethodEvent e)
  {
    if (inputMethodListener == null)
      return;
    switch (e.id)
      {
	case InputMethodEvent.CARET_POSITION_CHANGED:
          inputMethodListener.caretPositionChanged(e);
	break;
	case InputMethodEvent.INPUT_METHOD_TEXT_CHANGED:
          inputMethodListener.inputMethodTextChanged(e);
	break;
      }	
  }
  
  /** @since 1.3 */
  protected void processHierarchyEvent(HierarchyEvent e)
  {
    if (hierarchyListener == null)
      return;
    if (e.id == HierarchyEvent.HIERARCHY_CHANGED)
      hierarchyListener.hierarchyChanged(e);
  }
  
  /** @since 1.3 */
  protected void processHierarchyBoundsEvent(HierarchyEvent e)
  {
    if (hierarchyBoundsListener == null)
      return;
    switch (e.id)
      {
        case HierarchyEvent.ANCESTOR_MOVED:
	  hierarchyBoundsListener.ancestorMoved(e);
	break;
	case HierarchyEvent.ANCESTOR_RESIZED:
	  hierarchyBoundsListener.ancestorResized(e);
	break;
      }
  }
  
  /** @deprecated */
  public boolean handleEvent(Event evt)
  {
    return false;
  }
  
  /** @deprecated */
  public boolean mouseDown(Event evt, int x, int y)
  {
    return false;
  }
  
  /** @deprecated */
  public boolean mouseDrag(Event evt, int x, int y)
  {
    return false;
  }

  /** @deprecated */
  public boolean mouseUp(Event evt, int x, int y)
  {
    return false;
  }

  /** @deprecated */
  public boolean mouseMove(Event evt, int x, int y)
  {
    return false;
  }

  /** @deprecated */
  public boolean mouseEnter(Event evt, int x, int y)
  {
    return false;
  }

  /** @deprecated */
  public boolean mouseExit(Event evt, int x, int y)
  {
    return false;
  }

  /** @deprecated */
  public boolean keyDown(Event evt, int key)
  {
    return false;
  }

  /** @deprecated */
  public boolean keyUp(Event evt, int key)
  {
    return false;
  }

  /** @deprecated */
  public boolean action(Event evt, Object what)
  {
    return false;
  }

  public void addNotify()
  {
    // FIXME
  }
  
  public void removeNotify()
  {
    // FIXME
  }
  
  /** @deprecated */
  public boolean gotFocus(Event evt, Object what)
  {
    return false;
  }
  
  /** @deprecated */
  public boolean lostFocus(Event evt, Object what)
  {
    return false;
  }

  public boolean isFocusTraversable()
  {
    // FIXME
    return false;
  }
  
  public void requestFocus()
  {
    // FIXME
  }
  
  public void transferFocus()
  {
    // FIXME
  }
  
  /** @deprecated */
  public void nextFocus()
  {
    transferFocus();
  }
  
  /** @since 1.2 */
  public boolean hasFocus()
  {
    // FIXME
    return false;
  }
  
  public synchronized void add(PopupMenu popup)
  {
    if (popups == null)
      popups = new Vector();
    popups.addElement(popup);    
  }
  
  public synchronized void remove(MenuComponent popup)
  {
    popups.removeElement(popup);
  }
  
  protected String paramString()
  {
    // FIXME
    return "FIXME";
  }
  
  public String toString()
  {
    return paramString();
  }
  
  public void list()
  {
  }
  
  public void list(PrintStream out)
  {
  }
  
  public void list(PrintStream out, int indent)
  {
  }
  
  public void list(PrintWriter out)
  {
  }
  
  public void list(PrintWriter out, int indent)
  {
  }
  
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {

  }
  
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
  }
  
  public void addPropertyChangeListener(String propertyName,
                                	PropertyChangeListener listener)
  {
  }
  
  public void removePropertyChangeListener(String propertyName,
                                           PropertyChangeListener listener)
  {
  }
  
  protected void firePropertyChange(String propertyName, Object oldValue, 
                                    Object newValue)
  {
  }
  
  public void setComponentOrientation(ComponentOrientation o)
  {
    // FIXME
  }
  
  public ComponentOrientation getComponentOrientation()
  {
    // FIXME
    return null;
  }
  
  /*
  public AccessibleContext getAccessibleContext()
  {
    return accessibleContext;
  }
  */
}
