/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.peer.*;
import java.awt.event.*;
import java.net.URL;
import java.beans.*;
import java.awt.image.*;
import java.awt.datatransfer.Clipboard;
import java.util.Hashtable;
import gnu.gcj.awt.GLightweightPeer;

/* A very incomplete placeholder. */

public abstract class Toolkit
{
  static Toolkit defaultToolkit;
  PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);
  Hashtable desktopProperties = new Hashtable();

  public static Toolkit getDefaultToolkit()
  {
    if (defaultToolkit != null)
      return defaultToolkit;
    
    Class toolkit_class;
    String tk_class_name = System.getProperty("awt.toolkit");
    if (tk_class_name == null)
      tk_class_name = "gnu.awt.gtk.GtkToolkit";

    try
    {
      toolkit_class = Class.forName(tk_class_name);
      defaultToolkit = (Toolkit) toolkit_class.newInstance();
    }
    catch (Exception x)
    {
      throw new AWTError("Toolkit class " + tk_class_name + 
        		 " could not be initialized:\n  " + x);
    }

    return defaultToolkit;
  }

  protected abstract ButtonPeer createButton(Button target);
  protected abstract TextFieldPeer createTextField(TextField target);
  protected abstract LabelPeer createLabel(Label target);
  protected abstract ListPeer createList(List target);
  protected abstract CheckboxPeer createCheckbox(Checkbox target);
  protected abstract ScrollbarPeer createScrollbar(Scrollbar target);
  protected abstract ScrollPanePeer createScrollPane(ScrollPane target);
  protected abstract TextAreaPeer createTextArea(TextArea target);
  protected abstract ChoicePeer createChoice(Choice target);
  protected abstract FramePeer createFrame(Frame target);
  protected abstract CanvasPeer createCanvas(Canvas target);
  protected abstract PanelPeer createPanel(Panel target);
  protected abstract WindowPeer createWindow(Window target);
  protected abstract DialogPeer createDialog(Dialog target);
  protected abstract MenuBarPeer createMenuBar(MenuBar target);
  protected abstract MenuPeer createMenu(Menu target);
  protected abstract PopupMenuPeer createPopupMenu(PopupMenu target);
  protected abstract MenuItemPeer createMenuItem(MenuItem target);
  protected abstract FileDialogPeer createFileDialog(FileDialog target);
  protected abstract CheckboxMenuItemPeer 
    createCheckboxMenuItem(CheckboxMenuItem target);

  protected LightweightPeer createComponent(Component target)
  {
    return GLightweightPeer.INSTANCE;
  }
  
  /* @deprecated Use GraphicsEnvironment.getAllFonts() */
  protected abstract java.awt.peer.FontPeer getFontPeer(String name, int style);
  
  /*
  public abstract DragSourceContextPeer 
    createDragSourceContextPeer(DragGestureEvent dge)
    throws InvalidDnDOperationException;
  */
  
  protected void loadSystemColors(int[] systemColors)
  {
    // FIXME
  }

  public abstract Dimension getScreenSize();
  public abstract int getScreenResolution();
  public abstract ColorModel getColorModel();
  /* @deprecated Use GraphicsEnvironment.getAvailableFontFamilyNames() */
  public abstract String[] getFontList();
  public abstract FontMetrics getFontMetrics(Font font);
  public abstract void sync();
  public abstract Image getImage(String filename);
  public abstract Image getImage(URL url);
  public abstract Image createImage(String filename);
  public abstract Image createImage(URL url);
  public abstract boolean prepareImage(Image image, int width, int height,
                                       ImageObserver observer);
  public abstract int checkImage(Image image, int width, int height,
                        	 ImageObserver observer);
  public abstract Image createImage(ImageProducer producer);

  public Image createImage(byte[] imagedata)
  {
    return createImage (imagedata, 0, imagedata.length);
  }
  
  public abstract Image createImage(byte[] imagedata, int imageoffset,
                                    int imagelength);
  /*
  public abstract PrintJob getPrintJob(Frame frame, String jobtitle,
                                       Properties props);
  public PrintJob getPrintJob(Frame frame, String jobtitle,
                              JobAttributes jobAttributes,
			      PageAttributes pageAttributes)
  {
    
  }
  */
  
  public abstract void beep();
  public abstract Clipboard getSystemClipboard();

  public int getMenuShortcutKeyMask()
  {
    return InputEvent.CTRL_MASK;
  }

  public boolean getLockingKeyState(int keyCode)
  {
    if (keyCode != KeyEvent.VK_CAPS_LOCK
	&& keyCode != KeyEvent.VK_NUM_LOCK
	&& keyCode != KeyEvent.VK_SCROLL_LOCK)
      throw new IllegalArgumentException();

    throw new UnsupportedOperationException();
  }

  public void setLockingKeyState(int keyCode, boolean on)
  {
    if (keyCode != KeyEvent.VK_CAPS_LOCK
	&& keyCode != KeyEvent.VK_NUM_LOCK
	&& keyCode != KeyEvent.VK_SCROLL_LOCK)
      throw new IllegalArgumentException();

    throw new UnsupportedOperationException();
  }

  protected static Container getNativeContainer(Component c)
  {
    while (c != null) 
    {
      if (!c.isLightweight ())
	return (Container) c;

      c = c.getParent();
    }
    return null;
  }

  public Cursor createCustomCursor(Image cursor, Point hotSpot, String name)
    throws IndexOutOfBoundsException
  {
    // Presumably the only reason this isn't abstract is for backwards
    // compatibility? FIXME?
    return null;
  }

  public Dimension getBestCursorSize(int preferredWidth, int preferredHeight)
  {
    return new Dimension (0,0);
  }

  public int getMaximumCursorColors()
  {
    return 0;
  }

  public static String getProperty(String key, String defaultValue)
  {
    // FIXME
    return defaultValue;
  }

  public final EventQueue getSystemEventQueue()
  {
      return getSystemEventQueueImpl();
  }

  protected abstract EventQueue getSystemEventQueueImpl();

  /*
  public DragGestureRecognizer 
    createDragGestureRecognizer(Class abstractRecognizerClass, DragSource ds,
                        	Component c, int srcActions,
				DragGestureListener dgl)
  {
    // err... FIXME
    return null;
  }
  */

  public final Object getDesktopProperty(String propertyName)
  {
    return desktopProperties.get(propertyName);
  }

  protected final void setDesktopProperty(String name, Object newValue)
  {
    Object oldValue = getDesktopProperty(name);
    desktopProperties.put(name, newValue);
    changeSupport.firePropertyChange(name, oldValue, newValue);
  }

  protected Object lazilyLoadDesktopProperty(String name)
  {
    // FIXME - what is this??
    return null;
  }

  protected void initializeDesktopProperties()
  {
    // Overridden by toolkit implementation?
  }

  public void addPropertyChangeListener(String name,
                                	PropertyChangeListener pcl)
  {
    changeSupport.addPropertyChangeListener(name, pcl);
  }
  
  public void removePropertyChangeListener(String name,
                                           PropertyChangeListener pcl)
  {
    changeSupport.removePropertyChangeListener(name, pcl);
  }

  public void addAWTEventListener(AWTEventListener listener, long eventMask)
  {
    // SecurityManager s = System.getSecurityManager();
    // if (s != null)
    //  s.checkPermission(AWTPermission("listenToAllAWTEvents"));

    // FIXME
  }

  public void removeAWTEventListener(AWTEventListener listener)
  {
    // FIXME
  }
  
  /*
  public abstract Map mapInputMethodHighlight(InputMethodHighlight highlight)
  {
  }  
  */
}
