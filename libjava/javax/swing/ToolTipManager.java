/* ToolTipManager.java --
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;


/**
 * ToolTipManager
 * 
 * @author	Andrew Selkirk
 */
public class ToolTipManager extends MouseAdapter
  implements MouseMotionListener
{
	/**
	 * stillInsideTimerAction
	 */
  protected class stillInsideTimerAction
    implements ActionListener
  {
		/**
		 * Constructor stillInsideTimerAction
		 */
    protected stillInsideTimerAction()
    {
			// TODO
    }

		/**
		 * actionPerformed
		 * @param event TODO
		 */
    public void actionPerformed(ActionEvent event)
    {
			// TODO
    }
  }

	/**
	 * outsideTimerAction
	 */
  protected class outsideTimerAction
    implements ActionListener
  {
		/**
		 * Constructor outsideTimerAction
		 */
    protected outsideTimerAction()
    {
			// TODO
    }

		/**
		 * actionPerformed
		 * @param value0 TODO
		 */
    public void actionPerformed(ActionEvent event)
    {
			// TODO
    }
  }

	/**
	 * insideTimerAction
	 */
  protected class insideTimerAction
    implements ActionListener
  {
		/**
		 * Constructor insideTimerAction
		 */
    protected insideTimerAction()
    {
			// TODO
    }

		/**
		 * actionPerformed
		 * @param event TODO
		 */
    public void actionPerformed(ActionEvent event)
    {
			// TODO
    }
  }

	/**
	 * enterTimer
	 */
	Timer enterTimer;

	/**
	 * exitTimer
	 */
	Timer exitTimer;

	/**
	 * insideTimer
	 */
	Timer insideTimer;

	/**
	 * toolTipText
	 */
	String toolTipText;

	/**
	 * mouseEvent
	 */
	MouseEvent mouseEvent;

	/**
	 * showImmediately
	 */
	boolean showImmediately;

	/**
	 * tip
	 */
	JToolTip tip;

	/**
	 * enabled
	 */
	boolean enabled;

	/**
	 * timerEnter
	 */
	private long timerEnter;

	/**
	 * lightWeightPopupEnabled
	 */
	protected boolean lightWeightPopupEnabled;

	/**
	 * heavyWeightPopupEnabled
	 */
	protected boolean heavyWeightPopupEnabled;

	/**
	 * Constructor ToolTipManager
	 */
  ToolTipManager()
  {
		// TODO
  }

	/**
	 * sharedInstance
   * @return ToolTipManager
	 */
  public static ToolTipManager sharedInstance()
  {
		return null; // TODO
  }

	/**
	 * setEnabled
	 * @param enabled TODO
	 */
  public void setEnabled(boolean enabled)
  {
		// TODO
  }

	/**
	 * isEnabled
   * @return boolean
	 */
  public boolean isEnabled()
  {
		return false; // TODO
  }

	/**
	 * isLightWeightPopupEnabled
   * @return boolean
	 */
  public boolean isLightWeightPopupEnabled()
  {
		return false; // TODO
  }

	/**
	 * setLightWeightPopupEnabled
	 * @param enabled TODO
	 */
  public void setLightWeightPopupEnabled(boolean enabled)
  {
		// TODO
  }

	/**
	 * getInitialDelay
   * @return int
	 */
  public int getInitialDelay()
  {
		return 0; // TODO
  }

	/**
	 * setInitialDelay
	 * @param delay TODO
	 */
  public void setInitialDelay(int delay)
  {
		// TODO
  }

	/**
	 * getDismissDelay
   * @return int
	 */
  public int getDismissDelay()
  {
		return 0; // TODO
  }

	/**
	 * setDismissDelay
	 * @param delay TODO
	 */
  public void setDismissDelay(int delay)
  {
		// TODO
  }

	/**
	 * getReshowDelay
   * @return int
	 */
  public int getReshowDelay()
  {
		return 0; // TODO
  }

	/**
	 * setReshowDelay
	 * @param delay TODO
	 */
  public void setReshowDelay(int delay)
  {
		// TODO
  }

	/**
	 * registerComponent
	 * @param component TODO
	 */
  public void registerComponent(JComponent component)
  {
		// TODO
  }

	/**
	 * unregisterComponent
	 * @param component TODO
	 */
  public void unregisterComponent(JComponent component)
  {
		// TODO
  }

	/**
	 * mouseEntered
	 * @param event TODO
	 */
  public void mouseEntered(MouseEvent event)
  {
		// TODO
  }

	/**
	 * mouseExited
	 * @param event TODO
	 */
  public void mouseExited(MouseEvent event)
  {
		// TODO
  }

	/**
	 * mousePressed
	 * @param event TODO
	 */
  public void mousePressed(MouseEvent event)
  {
		// TODO
  }

	/**
	 * mouseDragged
	 * @param event TODO
	 */
  public void mouseDragged(MouseEvent event)
  {
		// TODO
  }

	/**
	 * mouseMoved
	 * @param event TODO
	 */
  public void mouseMoved(MouseEvent event)
  {
		// TODO
  }
}
