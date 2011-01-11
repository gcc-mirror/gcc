/* ClientFrame.java --
 Copyright (C) 2005 Free Software Foundation, Inc.

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

package gnu.classpath.examples.CORBA.swing.x5;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.*;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

import java.rmi.RemoteException;

import javax.rmi.PortableRemoteObject;

import javax.swing.*;
import java.awt.Dimension;

/**
 * The JFrame of the GUI client.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ClientFrame
  extends JFrame
{
  /**
   * The size of the playing field.
   */
  public final Dimension DESK_SIZE =
        new Dimension(624, 352-PlayingDesk.W);

  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  // Define the application components:

  /**
   * Central panel where the main action takes place.
   */
  PlayingDesk desk = new PlayingDesk();

  /**
   * The scroll pane for canvas.
   */
  JScrollPane scroll = new JScrollPane();

  /**
   * Will remember the manager IOR.
   */
  String mior = "";

  // The bottom panel contains the area that is used both to enter URL and
  // for chatting.
  JPanel pnBottom = new JPanel();

  BorderLayout layBottom = new BorderLayout();

  JTextField taUrl = new JTextField();

  // The top primitive chatting panel, composed from labels.
  JPanel pnChat = new JPanel();

  GridLayout layChat = new GridLayout();

  JLabel lbC3 = new JLabel();

  JLabel lbC2 = new JLabel();

  JLabel lbC1 = new JLabel();

  // The button panel.
  JPanel pnButtons = new JPanel();

  GridLayout layButtons = new GridLayout();

  JButton bLeave = new JButton();

  JButton bConnect = new JButton();

  JButton bExit = new JButton();

  JButton bReset = new JButton();

  JLabel lbState = new JLabel();

  JButton bChat = new JButton();

  JButton bPaste = new JButton();

  public ClientFrame()
  {
    try
      {
        jbInit();
      }
    catch (Exception e)
      {
        e.printStackTrace();
      }
  }

  private void jbInit()
    throws Exception
  {
    desk.frame = this;

    pnBottom.setLayout(layBottom);

    pnChat.setLayout(layChat);
    layChat.setColumns(1);
    layChat.setRows(3);

    lbC1.setText("This program needs the game server (see README on how to start it).");
    lbC2.setText("Enter the game server address (host:port)");
    lbC3.setText("Pressing \'Connect\' with the empty address will start the server on "
      + "the local machine.");
    bLeave.setEnabled(true);
    bLeave.setToolTipText("Leave if either you have lost or do not want longer to play with "
      + "this partner.");
    bLeave.setText("Leave game");
    bLeave.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        bLeave_actionPerformed(e);
      }
    });
    bConnect.setToolTipText("Connect your playing partner");
    bConnect.setText("Connect");
    bConnect.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        bConnect_actionPerformed(e);
      }
    });
    pnButtons.setLayout(layButtons);
    bExit.setToolTipText("Exit this program");
    bExit.setText("Exit");
    bExit.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        bExit_actionPerformed(e);
      }
    });
    layButtons.setHgap(2);
    bReset.setToolTipText("Restart the game. The partner may choose to exit!");
    bReset.setText("Reset game");
    bReset.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        bReset_actionPerformed(e);
      }
    });
    lbState.setText("Disconnected");
    bChat.setToolTipText("Send message to player. Reuse the address "+
                         "field to enter the message.");
    bChat.setText("Chat");
    bChat.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        bChat_actionPerformed(e);
      }
    });

    bPaste.setText("Paste");
    bPaste.setToolTipText("Paste, same as Ctrl-V");
    bPaste.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        bPaste_actionPerformed(e);
      }
    });

    desk.setMaximumSize(DESK_SIZE);
    desk.setPreferredSize(DESK_SIZE);

    scroll.getViewport().add(desk, null);
    getContentPane().add(scroll, BorderLayout.CENTER);
    getContentPane().add(pnBottom, BorderLayout.SOUTH);

    pnBottom.add(taUrl, BorderLayout.CENTER);
    pnBottom.add(pnChat, BorderLayout.NORTH);

    pnChat.add(lbC1, null);
    pnChat.add(lbC2, null);
    pnChat.add(lbC3, null);
    pnBottom.add(pnButtons, BorderLayout.SOUTH);
    pnButtons.add(lbState, null);
    pnButtons.add(bConnect, null);
    pnButtons.add(bChat, null);
    pnButtons.add(bLeave, null);
    pnButtons.add(bReset, null);
    pnButtons.add(bExit, null);
    pnButtons.add(bPaste, null);

    desk.player.set_current_state(State.DISCONNECTED);
  }

  /**
   * Handles exit procedure.
   */
  protected void processWindowEvent(WindowEvent e)
  {
    super.processWindowEvent(e);
    if (e.getID() == WindowEvent.WINDOW_CLOSING)
      {
        bExit_actionPerformed(null);
      }
  }

  /**
   * Handles the connection procedure.
   */
  void bConnect_actionPerformed(ActionEvent e)
  {
    try
      {
        int state = desk.player.get_current_state();

        if (state == State.DISCONNECTED || state == State.ERROR)
          {
            talk(ChatConstants.colors[0], "Connecting...");

            if (desk.manager == null)
              {
                mior = taUrl.getText().trim();

                // Obtain the manager object:
                org.omg.CORBA.Object object = null;

                try
                  {
                    object = desk.orb.string_to_object(mior);
                  }
                catch (Exception exc)
                  {
                    // Maybe CORBA 3.0.3 is not completely implemented?
                    if (mior.startsWith("http://") || mior.startsWith("ftp://")
                      || mior.startsWith("file://"))
                      object = desk.orb.string_to_object(IorReader.readUrl(mior));
                    else
                      throw exc;
                  }

                desk.manager = (GameManager) PortableRemoteObject.narrow(
                  object, GameManager.class);

                // Export the desk.player as a remote object.
                PortableRemoteObject.exportObject(desk.player);
              }

            desk.player.set_current_state(State.QUEUED);
            desk.manager.requestTheGame(desk.player);
          }

        // Save the specified IOR for the future use:
        File gmf = new File(OrbStarter.WRITE_URL_TO_FILE);
        FileWriter f = new FileWriter(gmf);
        BufferedWriter b = new BufferedWriter(f);

        b.write(mior);
        b.close();
      }
    catch (Exception ex)
      {
        talk(Color.red, "The manager is not reachable by this address.");
        talk(Color.red, ex.getMessage());
        desk.player.set_current_state(State.DISCONNECTED);
      }
  }

  /**
   * Display the new message with the given color. Shift the other messages over
   * the labels.
   */
  public void talk(Color color, String text)
  {
    lbC1.setText(lbC2.getText());
    lbC1.setForeground(lbC2.getForeground());

    lbC2.setText(lbC3.getText());
    lbC2.setForeground(lbC3.getForeground());

    lbC3.setText(text);
    lbC3.setForeground(color);
  }

  /**
   * Exit this program.
   */
  void bExit_actionPerformed(ActionEvent e)
  {
    try
      {
        if (desk.player.get_current_state() != State.DISCONNECTED
          && desk.player.partner != null)
          {
            desk.player.partner.receive_chat(ChatConstants.REMOTE_PLAYER,
              "I close the program!");
            desk.player.partner.disconnect();
          }
      }
    catch (RemoteException ex)
      {
        // We will print the exception because this is a demo application that
        // may be modified for learning purposes.
        ex.printStackTrace();
      }
    System.exit(0);
  }

  void bReset_actionPerformed(ActionEvent e)
  {
    if (desk.player.partner != null)
      {
        try
          {
            desk.player.partner.receive_chat(ChatConstants.REMOTE_PLAYER,
              "Your partner restarted the game.");

            desk.player.start_game(desk.player.partner, false);
            desk.player.partner.start_game(desk.player, true);
          }
        catch (RemoteException ex)
          {
            // We will print the exception because this is a demo application
            // that
            // may be modified for learning purposes.
            ex.printStackTrace();
          }
      }
    else
      talk(Color.black, "You have not started the game yet.");
  }

  void bLeave_actionPerformed(ActionEvent e)
  {
    desk.player.leave();
  }

  void bChat_actionPerformed(ActionEvent e)
  {
    try
      {
        if (desk.player.partner != null)
          {
            String message = taUrl.getText();
            desk.player.partner.receive_chat(ChatConstants.REMOTE_PLAYER, message);
            talk(ChatConstants.colors[ChatConstants.SELF], message);
            taUrl.setText("");
          }
        else
          {
            talk(Color.black, "Sorry, not connected to anybody");
          }
      }
    catch (RemoteException ex)
      {
        // We will print the exception because this is a demo application that
        // may be modified for learning purposes.
        ex.printStackTrace();
      }
  }

  /**
   * Work around our keyboard shortcut handling that is still not working
   * properly.
   */
  void bPaste_actionPerformed(ActionEvent e)
  {
    taUrl.paste();
  }
}
