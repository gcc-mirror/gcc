/* TransferHandler.java --
   Copyright (C) 2004, 2005, 2006, Free Software Foundation, Inc.

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

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceContext;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Method;

public class TransferHandler implements Serializable
{

  /**
   * An implementation of {@link Transferable} that can be used to export
   * data from a component's property.
   */
  private static class PropertyTransferable
    implements Transferable
  {
    /**
     * The component from which we export.
     */
    private JComponent component;

    /**
     * The property descriptor of the property that we handle.
     */
    private PropertyDescriptor property;

    /**
     * Creates a new PropertyTransferable.
     *
     * @param c the component from which we export
     * @param prop the property from which we export
     */
    PropertyTransferable(JComponent c, PropertyDescriptor prop)
    {
      component = c;
      property = prop;
    }

    /**
     * Returns the data flavors supported by the Transferable.
     *
     * @return the data flavors supported by the Transferable
     */
    public DataFlavor[] getTransferDataFlavors()
    {
      DataFlavor[] flavors;
      Class propClass = property.getPropertyType();
      String mime = DataFlavor.javaJVMLocalObjectMimeType + "; class="
                    + propClass.getName();
      try
        {
          DataFlavor flavor = new DataFlavor(mime);
          flavors = new DataFlavor[]{ flavor };
        }
      catch (ClassNotFoundException ex)
        {
          flavors = new DataFlavor[0];
        }
      return flavors;
    }

    /**
     * Returns <code>true</code> when the specified data flavor is supported,
     * <code>false</code> otherwise.
     *
     * @return <code>true</code> when the specified data flavor is supported,
     *         <code>false</code> otherwise
     */
    public boolean isDataFlavorSupported(DataFlavor flavor)
    {
      Class propClass = property.getPropertyType();
      return flavor.getPrimaryType().equals("application")
        && flavor.getSubType().equals("x-java-jvm-local-objectref")
        && propClass.isAssignableFrom(flavor.getRepresentationClass());
    }

    /**
     * Returns the actual transfer data.
     *
     * @param flavor the data flavor
     *
     * @return the actual transfer data
     */
    public Object getTransferData(DataFlavor flavor)
      throws UnsupportedFlavorException, IOException
    {
      if (isDataFlavorSupported(flavor))
        {
          Method getter = property.getReadMethod();
          Object o;
          try
            {
              o = getter.invoke(component);
              return o;
            }
          catch (Exception ex)
            {
              throw new IOException("Property read failed: "
                                    + property.getName());
            }
        }
      else
        throw new UnsupportedFlavorException(flavor);
    }
  }

  static class TransferAction extends AbstractAction
  {
    private String command;

    public TransferAction(String command)
    {
      super(command);
      this.command = command;
    }

    public void actionPerformed(ActionEvent event)
    {
      JComponent component = (JComponent) event.getSource();
      TransferHandler transferHandler = component.getTransferHandler();
      Clipboard clipboard = getClipboard(component);

      if (clipboard == null)
        {
          // Access denied!
          Toolkit.getDefaultToolkit().beep();
          return;
        }

      if (command.equals(COMMAND_COPY))
        transferHandler.exportToClipboard(component, clipboard, COPY);
      else if (command.equals(COMMAND_CUT))
        transferHandler.exportToClipboard(component, clipboard, MOVE);
      else if (command.equals(COMMAND_PASTE))
        {
          Transferable transferable = clipboard.getContents(null);

          if (transferable != null)
            transferHandler.importData(component, transferable);
        }
    }

    /**
     * Get the system cliboard or null if the caller isn't allowed to
     * access the system clipboard.
     *
     * @param component a component, used to get the toolkit.
     * @return the clipboard
     */
    private static Clipboard getClipboard(JComponent component)
    {
      try
        {
          return component.getToolkit().getSystemClipboard();
        }
      catch (SecurityException se)
        {
          return null;
        }
    }
  }

  private static class SwingDragGestureRecognizer extends DragGestureRecognizer
  {

    protected SwingDragGestureRecognizer(DragGestureListener dgl)
    {
      super(DragSource.getDefaultDragSource(), null, NONE, dgl);
    }

    void gesture(JComponent c, MouseEvent e, int src, int drag)
    {
      setComponent(c);
      setSourceActions(src);
      appendEvent(e);
      fireDragGestureRecognized(drag, e.getPoint());
    }

    protected void registerListeners()
    {
      // Nothing to do here.
    }

    protected void unregisterListeners()
    {
      // Nothing to do here.
    }

  }

  private static class SwingDragHandler
    implements DragGestureListener, DragSourceListener
  {

    private boolean autoscrolls;

    public void dragGestureRecognized(DragGestureEvent e)
    {
      JComponent c = (JComponent) e.getComponent();
      TransferHandler th = c.getTransferHandler();
      Transferable t = th.createTransferable(c);
      if (t != null)
        {
          autoscrolls = c.getAutoscrolls();
          c.setAutoscrolls(false);
          try
            {
              e.startDrag(null, t, this);
              return;
            }
          finally
            {
              c.setAutoscrolls(autoscrolls);
            }
        }
      th.exportDone(c, t, NONE);
    }

    public void dragDropEnd(DragSourceDropEvent e)
    {
      DragSourceContext ctx = e.getDragSourceContext();
      JComponent c = (JComponent) ctx.getComponent();
      TransferHandler th = c.getTransferHandler();
      if (e.getDropSuccess())
        {
          th.exportDone(c, ctx.getTransferable(), e.getDropAction());
        }
      else
        {
          th.exportDone(c, ctx.getTransferable(), e.getDropAction());
        }
      c.setAutoscrolls(autoscrolls);
    }

    public void dragEnter(DragSourceDragEvent e)
    {
      // Nothing to do here.
    }

    public void dragExit(DragSourceEvent e)
    {
      // Nothing to do here.
    }

    public void dragOver(DragSourceDragEvent e)
    {
      // Nothing to do here.
    }

    public void dropActionChanged(DragSourceDragEvent e)
    {
      // Nothing to do here.
    }

  }

  private static final long serialVersionUID = -967749805571669910L;

  private static final String COMMAND_COPY = "copy";
  private static final String COMMAND_CUT = "cut";
  private static final String COMMAND_PASTE = "paste";

  public static final int NONE = 0;
  public static final int COPY = 1;
  public static final int MOVE = 2;
  public static final int COPY_OR_MOVE = 3;

  private static Action copyAction = new TransferAction(COMMAND_COPY);
  private static Action cutAction = new TransferAction(COMMAND_CUT);
  private static Action pasteAction = new TransferAction(COMMAND_PASTE);

  private int sourceActions;
  private Icon visualRepresentation;

  /**
   * The name of the property into/from which this TransferHandler
   * imports/exports.
   */
  private String propertyName;

  /**
   * The DragGestureRecognizer for Swing.
   */
  private SwingDragGestureRecognizer recognizer;

  public static Action getCopyAction()
  {
    return copyAction;
  }

  public static Action getCutAction()
  {
    return cutAction;
  }

  public static Action getPasteAction()
  {
    return pasteAction;
  }

  protected TransferHandler()
  {
    this.sourceActions = NONE;
  }

  public TransferHandler(String property)
  {
    propertyName = property;
    this.sourceActions = property != null ? COPY : NONE;
  }

  /**
   * Returns <code>true</code> if the data in this TransferHandler can be
   * imported into the specified component. This will be the case when:
   * <ul>
   *   <li>The component has a readable and writable property with the property
   *   name specified in the TransferHandler constructor.</li>
   *   <li>There is a dataflavor with a mime type of
   *     <code>application/x-java-jvm-local-object-ref</code>.</li>
   *   <li>The dataflavor's representation class matches the class of the
   *    property in the component.</li>
   * </li>
   *
   * @param c the component to check
   * @param flavors the possible data flavors
   *
   * @return <code>true</code> if the data in this TransferHandler can be
   *         imported into the specified component, <code>false</code>
   *         otherwise
   */
  public boolean canImport(JComponent c, DataFlavor[] flavors)
  {
    PropertyDescriptor propDesc = getPropertyDescriptor(c);
    boolean canImport = false;
    if (propDesc != null)
      {
        // Check if the property is writable. The readable check is already
        // done in getPropertyDescriptor().
        Method writer = propDesc.getWriteMethod();
        if (writer != null)
          {
            Class[] params = writer.getParameterTypes();
            if (params.length == 1)
              {
                // Number of parameters ok, now check mime type and
                // representation class.
                DataFlavor flavor = getPropertyDataFlavor(params[0], flavors);
                if (flavor != null)
                  canImport = true;
              }
          }
      }
    return canImport;
  }

  /**
   * Creates a {@link Transferable} that can be used to export data
   * from the specified component.
   *
   * This method returns <code>null</code> when the specified component
   * doesn't have a readable property that matches the property name
   * specified in the <code>TransferHandler</code> constructor.
   *
   * @param c the component to create a transferable for
   *
   * @return a {@link Transferable} that can be used to export data
   *         from the specified component, or null if the component doesn't
   *         have a readable property like the transfer handler
   */
  protected Transferable createTransferable(JComponent c)
  {
    Transferable transferable = null;
    if (propertyName != null)
      {
        PropertyDescriptor prop = getPropertyDescriptor(c);
        if (prop != null)
          transferable = new PropertyTransferable(c, prop);
      }
    return transferable;
  }

  public void exportAsDrag(JComponent c, InputEvent e, int action)
  {
    int src = getSourceActions(c);
    int drag = src & action;
    if (! (e instanceof MouseEvent))
      {
        drag = NONE;
      }
    if (drag != NONE)
      {
        if (recognizer == null)
          {
            SwingDragHandler ds = new SwingDragHandler();
            recognizer = new SwingDragGestureRecognizer(ds);
          }
        recognizer.gesture(c, (MouseEvent) e, src, drag);
      }
    else
      {
        exportDone(c, null, NONE);
      }
  }

  /**
   * This method is invoked after data has been exported.
   * Subclasses should implement this method to remove the data that has been
   * transferred when the action was <code>MOVE</code>.
   *
   * The default implementation does nothing because MOVE is not supported.
   *
   * @param c the source component
   * @param data the data that has been transferred or <code>null</code>
   *        when the action is NONE
   * @param action the action that has been performed
   */
  protected void exportDone(JComponent c, Transferable data, int action)
  {
    // Nothing to do in the default implementation.
  }

  /**
   * Exports the property of the component <code>c</code> that was
   * specified for this TransferHandler to the clipboard, performing
   * the specified action.
   *
   * This will check if the action is allowed by calling
   * {@link #getSourceActions(JComponent)}. If the action is not allowed,
   * then no export is performed.
   *
   * In either case the method {@link #exportDone} will be called with
   * the action that has been performed, or {@link #NONE} if the action
   * was not allowed or could otherwise not be completed.
   * Any IllegalStateException that is thrown by the Clipboard due to
   * beeing unavailable will be propagated through this method.
   *
   * @param c the component from which to export
   * @param clip the clipboard to which the data will be exported
   * @param action the action to perform
   *
   * @throws IllegalStateException when the clipboard is not available
   */
  public void exportToClipboard(JComponent c, Clipboard clip, int action)
    throws IllegalStateException
  {
    action &= getSourceActions(c);
    Transferable transferable = createTransferable(c);
    if (transferable != null && action != NONE)
      {
        try
          {
            clip.setContents(transferable, null);
            exportDone(c, transferable, action);
          }
        catch (IllegalStateException ex)
          {
            exportDone(c, transferable, NONE);
            throw ex;
          }
      }
    else
      exportDone(c, null, NONE);
  }

  public int getSourceActions(JComponent c)
  {
    return sourceActions;
  }

  public Icon getVisualRepresentation(Transferable t)
  {
    return visualRepresentation;
  }

  /**
   * Imports the transfer data represented by <code>t</code> into the specified
   * component <code>c</code> by setting the property of this TransferHandler
   * on that component. If this succeeds, this method returns
   * <code>true</code>, otherwise <code>false</code>.
   *
   *
   * @param c the component to import into
   * @param t the transfer data to import
   *
   * @return <code>true</code> if the transfer succeeds, <code>false</code>
   *         otherwise
   */
  public boolean importData(JComponent c, Transferable t)
  {
    boolean ok = false;
    PropertyDescriptor prop = getPropertyDescriptor(c);
    if (prop != null)
      {
        Method writer = prop.getWriteMethod();
        if (writer != null)
          {
            Class[] params = writer.getParameterTypes();
            if (params.length == 1)
              {
                DataFlavor flavor = getPropertyDataFlavor(params[0],
                                                   t.getTransferDataFlavors());
                if (flavor != null)
                  {
                    try
                      {
                        Object value = t.getTransferData(flavor);
                        writer.invoke(c, new Object[]{ value });
                        ok = true;
                      }
                    catch (Exception ex)
                      {
                        // If anything goes wrong here, do nothing and return
                        // false;
                      }
                  }
              }
          }
      }
    return ok;
  }

  /**
   * Returns the property descriptor for the property of this TransferHandler
   * in the specified component, or <code>null</code> if no such property
   * exists in the component. This method only returns properties that are
   * at least readable (that is, it has a public no-arg getter method).
   *
   * @param c the component to check
   *
   * @return the property descriptor for the property of this TransferHandler
   *         in the specified component, or <code>null</code> if no such
   *         property exists in the component
   */
  private PropertyDescriptor getPropertyDescriptor(JComponent c)
  {
    PropertyDescriptor prop = null;
    if (propertyName != null)
      {
        Class clazz = c.getClass();
        BeanInfo beanInfo;
        try
          {
            beanInfo = Introspector.getBeanInfo(clazz);
          }
        catch (IntrospectionException ex)
          {
            beanInfo = null;
          }
        if (beanInfo != null)
          {
            PropertyDescriptor[] props = beanInfo.getPropertyDescriptors();
            for (int i = 0; i < props.length && prop == null; i++)
              {
                PropertyDescriptor desc = props[i];
                if (desc.getName().equals(propertyName))
                  {
                    Method reader = desc.getReadMethod();
                    if (reader != null)
                      {
                        Class[] params = reader.getParameterTypes();
                        if (params == null || params.length == 0)
                          prop = desc;
                      }
                  }
              }
          }
      }
    return prop;
  }

  /**
   * Searches <code>flavors</code> to find a suitable data flavor that
   * has the mime type application/x-java-jvm-local-objectref and a
   * representation class that is the same as the specified <code>clazz</code>.
   * When no such data flavor is found, this returns <code>null</code>.
   *
   * @param clazz the representation class required for the data flavor
   * @param flavors the possible data flavors
   *
   * @return the suitable data flavor or null if none is found
   */
  private DataFlavor getPropertyDataFlavor(Class clazz, DataFlavor[] flavors)
  {
    DataFlavor found = null;
    for (int i = 0; i < flavors.length && found == null; i++)
      {
        DataFlavor flavor = flavors[i];
        if (flavor.getPrimaryType().equals("application")
            && flavor.getSubType().equals("x-java-jvm-local-objectref")
            && clazz.isAssignableFrom(flavor.getRepresentationClass()))
          found = flavor;
      }
    return found;
  }
}
