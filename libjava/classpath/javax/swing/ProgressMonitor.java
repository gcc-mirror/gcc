/* ProgressMonitor.java --
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.accessibility.AccessibleContext;

/**
 * <p>Using this class you can easily monitor tasks where you cannot
 * estimate the duration exactly.</p>
 *
 * <p>A ProgressMonitor instance waits until the first time setProgress
 * is called. When <code>millisToDecideToPopup</code> time elapsed the
 * instance estimates the duration until the whole operation is completed.
 * If this duration exceeds <code>millisToPopup</code> a non-modal dialog
 * with a message and a progress bar is shown.</p>
 *
 * <p>The value of <code>millisToDecideToPopup</code> defaults to
 * <code>500</code> and <code>millisToPopup</code> to
 * <code>2000</code>.</p>
 *
 * @author Andrew Selkirk
 * @author Robert Schuster (robertschuster@fsfe.org)
 * @since 1.2
 * @status updated to 1.2
 */
public class ProgressMonitor
{

  /**
   * The accessible content for this component
   */
  protected AccessibleContext accessibleContext;

  /**
   * parentComponent
   */
  Component component;

  /**
   * note
   */
  String note;

  /**
   * message
   */
  Object message;

  /**
   * millisToDecideToPopup
   */
  int millisToDecideToPopup = 500;

  /**
   * millisToPopup
   */
  int millisToPopup = 2000;

  int min, max, progress;

  JProgressBar progressBar;

  JLabel noteLabel;

  JDialog progressDialog;

  Timer timer;

  boolean canceled;

  /**
   * Creates a new <code>ProgressMonitor</code> instance.  This is used to
   * monitor a task and pops up a dialog if the task is taking a long time to
   * run.
   *
   * @param component The parent component of the progress dialog or
   *                  <code>null</code>.
   * @param message A constant message object which works in the way it does
   *                in {@link JOptionPane}.
   * @param note A string message which can be changed while the operation goes
   *             on.
   * @param minimum The minimum value for the operation (start value).
   * @param maximum The maximum value for the operation (end value).
   */
  public ProgressMonitor(Component component, Object message,
                         String note, int minimum, int maximum)
  {

    // Set data.
    this.component = component;
    this.message = message;
    this.note = note;

    min = minimum;
    max = maximum;
  }

  /**
   * <p>Hides the dialog and stops any measurements.</p>
   *
   * <p>Has no effect when <code>setProgress</code> is not at least
   * called once.</p>
   */
  public void close()
  {
    if (progressDialog != null)
      {
        progressDialog.setVisible(false);
      }

    if (timer != null)
      {
        timer.stop();
        timer = null;
      }
  }

  /**
   * <p>Updates the progress value.</p>
   *
   * <p>When called for the first time this initializes a timer
   * which decides after <code>millisToDecideToPopup</code> time
   * whether to show a progress dialog or not.</p>
   *
   * <p>If the progress value equals or exceeds the maximum
   * value the progress dialog is closed automatically.</p>
   *
   * @param progress New progress value.
   */
  public void setProgress(int progress)
  {
    this.progress = progress;

    // Initializes and starts a timer with a task
    // which measures the duration and displays
    // a progress dialog if neccessary.
    if (timer == null && progressDialog == null)
      {
        timer = new Timer(25, null);
        timer.addActionListener(new TimerListener());
        timer.start();
      }

    // Cancels timer and hides progress dialog if the
    // maximum value is reached.
    if (progressBar != null && this.progress >= progressBar.getMaximum())
      {
        // The reason for using progressBar.getMaximum() instead of max is that
        // we want to prevent that changes to the value have any effect after the
        // progress dialog is visible (This is how the JDK behaves.).
        close();
      }

  }

  /**
   * Returns the minimum or start value of the operation.
   *
   * @return Minimum or start value of the operation.
   */
  public int getMinimum()
  {
    return min;
  }

  /**
   * <p>Use this method to set the minimum or start value of
   * your operation.</p>
   *
   * <p>For typical application like copy operation this will be
   * zero.</p>
   *
   * <p>Keep in mind that changing this value after the progress
   * dialog is made visible has no effect upon the progress bar.</p>
   *
   * @param minimum The new minimum value.
   */
  public void setMinimum(int minimum)
  {
    min = minimum;
  }

  /**
   * Return the maximum or end value of your operation.
   *
   * @return Maximum or end value.
   */
  public int getMaximum()
  {
    return max;
  }

  /**
   * <p>Sets the maximum or end value of the operation to the
   * given integer.</p>
   *
   * @param maximum
   */
  public void setMaximum(int maximum)
  {
    max = maximum;
  }

  /**
   * Returns whether the user canceled the operation.
   *
   * @return Whether the operation was canceled.
   */
  public boolean isCanceled()
  {
    // The value is predefined to false
    // and changes only when the user clicks
    // the cancel button in the progress dialog.
    return canceled;
  }

  /**
   * Returns the amount of milliseconds to wait
   * until the ProgressMonitor should decide whether
   * a progress dialog is to be shown or not.
   *
   * @return The duration in milliseconds.
   */
  public int getMillisToDecideToPopup()
  {
    return millisToDecideToPopup;
  }

  /**
   * Sets the amount of milliseconds to wait until the
   * ProgressMonitor should decide whether a progress dialog
   * is to be shown or not.
   *
   * <p>This method has no effect when the progress dialog
   * is already visible.</p>
   *
   * @param time The duration in milliseconds.
   */
  public void setMillisToDecideToPopup(int time)
  {
    millisToDecideToPopup = time;
  }

  /**
   * Returns the number of milliseconds to wait before displaying the progress
   * dialog.  The default value is 2000.
   *
   * @return The number of milliseconds.
   *
   * @see #setMillisToPopup(int)
   */
  public int getMillisToPopup()
  {
    return millisToPopup;
  }

  /**
   * Sets the number of milliseconds to wait before displaying the progress
   * dialog.
   *
   * @param time  the number of milliseconds.
   *
   * @see #getMillisToPopup()
   */
  public void setMillisToPopup(int time)
  {
    millisToPopup = time;
  }

  /**
   * Returns a message which is shown in the progress dialog.
   *
   * @return The changeable message visible in the progress dialog.
   */
  public String getNote()
  {
    return note;
  }

  /**
   * <p>Set the message shown in the progess dialog.</p>
   *
   * <p>Changing the note while the progress dialog is visible
   * is possible.</p>
   *
   * @param note A message shown in the progress dialog.
   */
  public void setNote(String note)
  {
    if (noteLabel != null)
      {
        noteLabel.setText(note);
      }
    else
      {
        this.note = note;
      }
  }

  /**
   * Internal method that creates the progress dialog.
   */
  void createDialog()
  {
    // If there is no note we suppress the generation of the
    // label.
    Object[] tmp = (note == null) ?
      new Object[]
        {
          message,
          progressBar = new JProgressBar(min, max)
        }
      :
      new Object[]
        {
          message,
          noteLabel = new JLabel(note),
          progressBar = new JProgressBar(min, max)
        };

    JOptionPane pane = new JOptionPane(tmp, JOptionPane.INFORMATION_MESSAGE);

    // FIXME: Internationalize the button
    JButton cancelButton = new JButton("Cancel");
    cancelButton.addActionListener(new ActionListener()
    {
      public void actionPerformed(ActionEvent ae)
      {
        canceled = true;
      }
    });

    pane.setOptions(new Object[] { cancelButton });

    // FIXME: Internationalize the title
    progressDialog = pane.createDialog(component, "Progress ...");
    progressDialog.setModal(false);
    progressDialog.setResizable(true);

    progressDialog.pack();
    progressDialog.setVisible(true);

  }

  /** An ActionListener implementation which does the measurements
   * and estimations of the ProgressMonitor.
   */
  class TimerListener implements ActionListener
  {
    long timestamp;

    int lastProgress;

    boolean first = true;

    TimerListener()
    {
       timestamp = System.currentTimeMillis();
    }

    public void actionPerformed(ActionEvent ae)
    {
       long now = System.currentTimeMillis();

       if (first)
       {
         if ((now - timestamp) > millisToDecideToPopup)
         {
           first = false;


           long expected = (progress - min == 0) ?
             (now - timestamp) * (max - min) :
             (now - timestamp) * (max - min) / (progress - min);

           if (expected > millisToPopup)
           {
             createDialog();
           }
         }
         else
         {
           // We have not waited long enough to make a decision,
           // so return and try again when the timer is invoked.
           return;
         }
       }
       else if (progressDialog != null)
       {
         // The progress dialog is being displayed. We now calculate
         // whether setting the progress bar to the current progress
         // value would result in a visual difference.
         int delta = progress - progressBar.getValue();

         if ((delta * progressBar.getWidth() / (max - min)) > 0)
         {
           // At least one pixel would change.
           progressBar.setValue(progress);
         }
       }
       else
       {
         // No dialog necessary
         timer.stop();
         timer = null;
       }

      timestamp = now;
    }
  }

  /**
   * Gets the accessible context.
   *
   * @return the accessible context.
   */
  public AccessibleContext getAccessibleContext()
  {
    return accessibleContext;
  }
}
