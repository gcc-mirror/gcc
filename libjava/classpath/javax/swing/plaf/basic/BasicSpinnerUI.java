/* BasicSpinnerUI.java --
   Copyright (C) 2003, 2004, 2005, 2006,  Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JSpinner;
import javax.swing.LookAndFeel;
import javax.swing.Timer;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.SpinnerUI;

/**
 * A UI delegate for the {@link JSpinner} component.
 *
 * @author Ka-Hing Cheung
 *
 * @since 1.4
 */
public class BasicSpinnerUI extends SpinnerUI
{
  /**
   * Creates a new <code>BasicSpinnerUI</code> for the specified
   * <code>JComponent</code>
   *
   * @param c  the component (ignored).
   *
   * @return A new instance of {@link BasicSpinnerUI}.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicSpinnerUI();
  }

  /**
   * Creates an editor component. Really, it just returns
   * <code>JSpinner.getEditor()</code>
   *
   * @return a JComponent as an editor
   *
   * @see javax.swing.JSpinner#getEditor
   */
  protected JComponent createEditor()
  {
    return spinner.getEditor();
  }

  /**
   * Creates a <code>LayoutManager</code> that layouts the sub components. The
   * subcomponents are identifies by the constraint "Next", "Previous" and
   * "Editor"
   *
   * @return a LayoutManager
   *
   * @see java.awt.LayoutManager
   */
  protected LayoutManager createLayout()
  {
    return new DefaultLayoutManager();
  }

  /**
   * Creates the "Next" button
   *
   * @return the next button component
   */
  protected Component createNextButton()
  {
    JButton button = new BasicArrowButton(BasicArrowButton.NORTH);
    return button;
  }

  /**
   * Creates the "Previous" button
   *
   * @return the previous button component
   */
  protected Component createPreviousButton()
  {
    JButton button = new BasicArrowButton(BasicArrowButton.SOUTH);
    return button;
  }

  /**
   * Creates the <code>PropertyChangeListener</code> that will be attached by
   * <code>installListeners</code>. It should watch for the "editor"
   * property, when it's changed, replace the old editor with the new one,
   * probably by calling <code>replaceEditor</code>
   *
   * @return a PropertyChangeListener
   *
   * @see #replaceEditor
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeListener()
      {
        public void propertyChange(PropertyChangeEvent event)
        {
          // FIXME: Add check for enabled property change. Need to
          // disable the buttons.
          if ("editor".equals(event.getPropertyName()))
            BasicSpinnerUI.this.replaceEditor((JComponent) event.getOldValue(),
                (JComponent) event.getNewValue());
          // FIXME: Handle 'font' property change
        }
      };
  }

  /**
   * Called by <code>installUI</code>. This should set various defaults
   * obtained from <code>UIManager.getLookAndFeelDefaults</code>, as well as
   * set the layout obtained from <code>createLayout</code>
   *
   * @see javax.swing.UIManager#getLookAndFeelDefaults
   * @see #createLayout
   * @see #installUI
   */
  protected void installDefaults()
  {
    LookAndFeel.installColorsAndFont(spinner, "Spinner.background",
                                     "Spinner.foreground", "Spinner.font");
    LookAndFeel.installBorder(spinner, "Spinner.border");
    JComponent e = spinner.getEditor();
    if (e instanceof JSpinner.DefaultEditor)
      {
        JSpinner.DefaultEditor de = (JSpinner.DefaultEditor) e;
        de.getTextField().setBorder(null);
      }
    spinner.setLayout(createLayout());
    spinner.setOpaque(true);
  }

  /*
   * Called by <code>installUI</code>, which basically adds the
   * <code>PropertyChangeListener</code> created by
   * <code>createPropertyChangeListener</code>
   *
   * @see #createPropertyChangeListener
   * @see #installUI
   */
  protected void installListeners()
  {
    spinner.addPropertyChangeListener(listener);
  }

  /*
   * Install listeners to the next button so that it increments the model
   */
  protected void installNextButtonListeners(Component c)
  {
    c.addMouseListener(new MouseAdapter()
        {
          public void mousePressed(MouseEvent evt)
          {
            if (! spinner.isEnabled())
              return;
            increment();
            timer.setInitialDelay(500);
            timer.start();
          }

          public void mouseReleased(MouseEvent evt)
          {
            timer.stop();
          }

          void increment()
          {
            Object next = BasicSpinnerUI.this.spinner.getNextValue();
            if (next != null)
              BasicSpinnerUI.this.spinner.getModel().setValue(next);
          }

          volatile boolean mouseDown;
          Timer timer = new Timer(50,
                                  new ActionListener()
              {
                public void actionPerformed(ActionEvent event)
                {
                  increment();
                }
              });
        });
  }

  /*
   * Install listeners to the previous button so that it decrements the model
   */
  protected void installPreviousButtonListeners(Component c)
  {
    c.addMouseListener(new MouseAdapter()
        {
          public void mousePressed(MouseEvent evt)
          {
            if (! spinner.isEnabled())
              return;
            decrement();
            timer.setInitialDelay(500);
            timer.start();
          }

          public void mouseReleased(MouseEvent evt)
          {
            timer.stop();
          }

          void decrement()
          {
            Object prev = BasicSpinnerUI.this.spinner.getPreviousValue();
            if (prev != null)
              BasicSpinnerUI.this.spinner.getModel().setValue(prev);
          }

          volatile boolean mouseDown;
          Timer timer = new Timer(50,
                                  new ActionListener()
              {
                public void actionPerformed(ActionEvent event)
                {
                  decrement();
                }
              });
        });
  }

  /**
   * Install this UI to the <code>JComponent</code>, which in reality, is a
   * <code>JSpinner</code>. Calls <code>installDefaults</code>,
   * <code>installListeners</code>, and also adds the buttons and editor.
   *
   * @param c DOCUMENT ME!
   *
   * @see #installDefaults
   * @see #installListeners
   * @see #createNextButton
   * @see #createPreviousButton
   * @see #createEditor
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);

    spinner = (JSpinner) c;

    installDefaults();
    installListeners();

    Component next = createNextButton();
    Component previous = createPreviousButton();

    installNextButtonListeners(next);
    installPreviousButtonListeners(previous);

    c.add(createEditor(), "Editor");
    c.add(next, "Next");
    c.add(previous, "Previous");
  }

  /**
   * Replace the old editor with the new one
   *
   * @param oldEditor the old editor
   * @param newEditor the new one to replace with
   */
  protected void replaceEditor(JComponent oldEditor, JComponent newEditor)
  {
    spinner.remove(oldEditor);
    spinner.add(newEditor);
  }

  /**
   * The reverse of <code>installDefaults</code>. Called by
   * <code>uninstallUI</code>
   */
  protected void uninstallDefaults()
  {
    spinner.setLayout(null);
  }

  /**
   * The reverse of <code>installListeners</code>, called by
   * <code>uninstallUI</code>
   */
  protected void uninstallListeners()
  {
    spinner.removePropertyChangeListener(listener);
  }

  /**
   * Called when the current L&F is replaced with another one, should call
   * <code>uninstallDefaults</code> and <code>uninstallListeners</code> as
   * well as remove the next/previous buttons and the editor
   *
   * @param c DOCUMENT ME!
   */
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);

    uninstallDefaults();
    uninstallListeners();
    c.removeAll();
  }

  /** The spinner for this UI */
  protected JSpinner spinner;

  /** DOCUMENT ME! */
  private PropertyChangeListener listener = createPropertyChangeListener();

  /**
   * A layout manager for the {@link JSpinner} component.  The spinner has
   * three subcomponents: an editor, a 'next' button and a 'previous' button.
   */
  private class DefaultLayoutManager implements LayoutManager
  {
    /**
     * Layout the spinners inner parts.
     *
     * @param parent The parent container
     */
    public void layoutContainer(Container parent)
    {
      synchronized (parent.getTreeLock())
        {
          Insets i = parent.getInsets();
          boolean l2r = parent.getComponentOrientation().isLeftToRight();
          /*
            --------------    --------------
            |        | n |    | n |        |
            |   e    | - | or | - |   e    |
            |        | p |    | p |        |
            --------------    --------------
          */
          Dimension e = prefSize(editor);
          Dimension n = prefSize(next);
          Dimension p = prefSize(previous);
          Dimension s = parent.getSize();

          int x = l2r ? i.left : i.right;
          int y = i.top;
          int w = Math.max(p.width, n.width);
          int h = (s.height - i.bottom) / 2;
          int e_width = s.width - w - i.left - i.right;

          if (l2r)
            {
              setBounds(editor, x, y, e_width, 2 * h);
              x += e_width;
              setBounds(next, x, y, w, h);
              y += h;
              setBounds(previous, x, y, w, h);
            }
          else
            {
              setBounds(next, x, y + (s.height - e.height) / 2, w, h);
              y += h;
              setBounds(previous, x, y + (s.height - e.height) / 2, w, h);
              x += w;
              y -= h;
              setBounds(editor, x, y, e_width, e.height);
            }
        }
    }

    /**
     * Calculates the minimum layout size.
     *
     * @param parent  the parent.
     *
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      Dimension d = new Dimension();

      if (editor != null)
        {
          Dimension tmp = editor.getMinimumSize();
          d.width += tmp.width;
          d.height = tmp.height;
        }

      int nextWidth = 0;
      int previousWidth = 0;

      if (next != null)
        {
          Dimension tmp = next.getMinimumSize();
          nextWidth = tmp.width;
        }
      if (previous != null)
        {
          Dimension tmp = previous.getMinimumSize();
          previousWidth = tmp.width;
        }

      d.width += Math.max(nextWidth, previousWidth);

      return d;
    }

    /**
     * Returns the preferred layout size of the container.
     *
     * @param parent DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      Dimension d = new Dimension();

      if (editor != null)
        {
          Dimension tmp = editor.getPreferredSize();
          d.width += Math.max(tmp.width, 40);
          d.height = tmp.height;
        }

      int nextWidth = 0;
      int previousWidth = 0;

      if (next != null)
        {
          Dimension tmp = next.getPreferredSize();
          nextWidth = tmp.width;
        }
      if (previous != null)
        {
          Dimension tmp = previous.getPreferredSize();
          previousWidth = tmp.width;
        }

      d.width += Math.max(nextWidth, previousWidth);
      Insets insets = parent.getInsets();
      d.width = d.width + insets.left + insets.right;
      d.height = d.height + insets.top + insets.bottom;
      return d;
    }

    /**
     * DOCUMENT ME!
     *
     * @param child DOCUMENT ME!
     */
    public void removeLayoutComponent(Component child)
    {
      if (child == editor)
        editor = null;
      else if (child == next)
        next = null;
      else if (previous == child)
        previous = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param child DOCUMENT ME!
     */
    public void addLayoutComponent(String name, Component child)
    {
      if ("Editor".equals(name))
        editor = child;
      else if ("Next".equals(name))
        next = child;
      else if ("Previous".equals(name))
        previous = child;
    }

    /**
     * DOCUMENT ME!
     *
     * @param c DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    private Dimension prefSize(Component c)
    {
      if (c == null)
        return new Dimension();
      else
        return c.getPreferredSize();
    }

    /**
     * Sets the bounds for the specified component.
     *
     * @param c  the component.
     * @param x  the x-coordinate for the top-left of the component bounds.
     * @param y  the y-coordinate for the top-left of the component bounds.
     * @param w  the width of the bounds.
     * @param h  the height of the bounds.
     */
    private void setBounds(Component c, int x, int y, int w, int h)
    {
      if (c != null)
        c.setBounds(x, y, w, h);
    }

    /** The editor component. */
    private Component editor;

    /** The next button. */
    private Component next;

    /** The previous button. */
    private Component previous;
  }
}
