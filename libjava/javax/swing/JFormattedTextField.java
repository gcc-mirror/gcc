/* JFormattedTextField.java --
   Copyright (C) 2003 Free Software Foundation, Inc.

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

import java.awt.event.FocusEvent;
import java.io.Serializable;
import java.text.Format;
import java.text.ParseException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
import javax.swing.text.NavigationFilter;

/**
 * @author Michael Koch
 * @since 1.4
 */
public class JFormattedTextField extends JTextField
{
  public abstract static class AbstractFormatter implements Serializable
  {
    public AbstractFormatter ()
    {
      //Do nothing here.
    }

    protected Object clone ()
    {
      throw new InternalError ("not implemented");
    }

    protected Action[] getActions ()
    {
      throw new InternalError ("not implemented");
    }

    protected DocumentFilter getDocumentFilter ()
    {
      throw new InternalError ("not implemented");
    }

    protected JFormattedTextField getFormattedTextField ()
    {
      throw new InternalError ("not implemented");
    }

    protected NavigationFilter getNavigationFilter ()
    {
      throw new InternalError ("not implemented");
    }

    public void install (JFormattedTextField ftf)
    {
      throw new InternalError ("not implemented");
    }

    public void uninstall ()
    {
      throw new InternalError ("not implemented");
    }

    protected void invalidEdit ()
    {
      throw new InternalError ("not implemented");
    }

    protected void setEditValid ()
    {
      throw new InternalError ("not implemented");
    }

    public abstract Object stringToValue (String text);

    public abstract String valueToString (Object value);
  }
  
  public abstract static class AbstractFormatterFactory
  {
    public AbstractFormatterFactory ()
    {
      // Do nothing here.
    }

    public abstract AbstractFormatter getFormatter (JFormattedTextField tf);
  }

  public static final int COMMIT = 0;
  public static final int COMMIT_OR_REVERT = 1;
  public static final int REVERT = 2;
  public static final int PERSIST = 3;

  public JFormattedTextField ()
  {
    throw new InternalError ("not implemented");
  }

  public JFormattedTextField (Format format)
  {
    throw new InternalError ("not implemented");
  }

  public JFormattedTextField (AbstractFormatter formatter)
  {
    throw new InternalError ("not implemented");
  }

  public JFormattedTextField (AbstractFormatterFactory factory)
  {
    throw new InternalError ("not implemented");
  }

  public JFormattedTextField (AbstractFormatterFactory factory, Object value)
  {
    throw new InternalError ("not implemented");
  }

  public JFormattedTextField (Object value)
  {
    throw new InternalError ("not implemented");
  }

  public void commitEdit ()
  {
    throw new InternalError ("not implemented");
  }

  public Action[] getActions ()
  {
    throw new InternalError ("not implemented");
  }

  public int getFocusLostBehaviour ()
  {
    throw new InternalError ("not implemented");
  }

  public AbstractFormatter getFormatter ()
  {
    throw new InternalError ("not implemented");
  }

  public AbstractFormatterFactory getFormatterFactory ()
  {
    throw new InternalError ("not implemented");
  }

  public String getUIClassID ()
  {
    throw new InternalError ("not implemented");
  }

  public Object getValue ()
  {
    throw new InternalError ("not implemented");
  }

  protected void invalidEdit ()
  {
    throw new InternalError ("not implemented");
  }

  public boolean isEditValid ()
  {
    throw new InternalError ("not implemented");
  }

  protected void processFocusEvent (FocusEvent evt)
  {
    throw new InternalError ("not implemented");
  }

  public void setDocument (Document document)
  {
    throw new InternalError ("not implemented");
  }

  public void setLostFocusBehavior (int behavior)
  {
    throw new InternalError ("not implemented");
  }

  protected void setFormatter (AbstractFormatter formatter)
  {
    throw new InternalError ("not implemented");
  }

  public void setFormatterFactory (AbstractFormatterFactory factory)
  {
    throw new InternalError ("not implemented");
  }

  public void setValue (Object value)
  {
    throw new InternalError ("not implemented");
  }
}
