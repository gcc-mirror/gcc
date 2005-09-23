/* Demo.java -- And example of copy/paste datatransfer
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
02110-1301 USA. */

package gnu.classpath.examples.datatransfer;

import java.awt.*;
import java.awt.event.*;
import java.awt.datatransfer.*;

import java.io.*;
import java.net.URL;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Random;

/**
 * An example how datatransfer works for copying and pasting data to
 * and from other programs.
 */
class Demo
  extends Frame
  implements ActionListener, ItemListener, FlavorListener
{
  public static void main(String args[])
  {
    new Demo();
  }

  private TextArea text;
  private Button copyText;
  private Button pasteText;

  private ImageComponent image;
  private Button copyImage;
  private Button pasteImage;

  private ObjectComponent object;
  private Button copyObject;
  private Button pasteObject;

  private FilesComponent files;
  private Button copyFiles;
  private Button pasteFiles;

  private FlavorsComponent flavors;
  private FlavorDetailsComponent details;

  private Demo()
  {
    super("GNU Classpath datatransfer");

    /* Add all the different panel to the main window in one row. */
    setLayout(new GridLayout(5, 1, 10, 10));
    add(createTextPanel());
    add(createImagePanel());
    add(createObjectPanel());
    add(createFilesPanel());
    add(createFlavorsPanel());

    /* Add listeners for the various buttons and events we are
       interested in. */
    addWindowListener(new WindowAdapter ()
      {
	public void windowClosing (WindowEvent e)
	{
	  dispose();
	}
      });
    flavors.addItemListener(this);
    Toolkit t = Toolkit.getDefaultToolkit();
    Clipboard c = t.getSystemClipboard();
    c.addFlavorListener(this);

    /* Show time! */
    pack();
    show();
  }

  /**
   * The Text Panel will show simple text that can be copied and pasted.
   */
  private Panel createTextPanel()
  {
    Panel textPanel = new Panel();
    textPanel.setLayout(new BorderLayout());
    text = new TextArea("GNU Everywhere!",
			2, 80,
			TextArea.SCROLLBARS_VERTICAL_ONLY);
    text.setEditable(false);
    text.setEnabled(true);
    Panel textButtons = new Panel();
    textButtons.setLayout(new FlowLayout());
    copyText = new Button("Copy text");
    copyText.addActionListener(this);
    pasteText = new Button("Paste text");
    pasteText.addActionListener(this);
    textButtons.add(copyText);
    textButtons.add(pasteText);
    textPanel.add(text, BorderLayout.CENTER);
    textPanel.add(textButtons, BorderLayout.SOUTH);
    return textPanel;
  }

  /**
   * The Image Panel shows an image that can be copied to another
   * program or be replaced by pasting in an image from another
   * application.
   */
  private Panel createImagePanel()
  {
    Panel imagePanel = new Panel();
    imagePanel.setLayout(new BorderLayout());
    URL imageurl = this.getClass()
      .getResource("/gnu/classpath/examples/icons/big-fullscreen.png");
    Image img = Toolkit.getDefaultToolkit().createImage(imageurl);
    image = new ImageComponent(img);
    Panel imageButtons = new Panel();
    copyImage = new Button("Copy image");
    copyImage.addActionListener(this);
    pasteImage = new Button("Paste image");
    pasteImage.addActionListener(this);
    imageButtons.add(copyImage);
    imageButtons.add(pasteImage);
    imagePanel.add(image, BorderLayout.CENTER);
    imagePanel.add(imageButtons, BorderLayout.SOUTH);
    return imagePanel;
  }

  /**
   * The Object Panel holds a simple (Point) object that can be copied
   * and pasted to another program that supports exchanging serialized
   * objects.
   */
  private Panel createObjectPanel()
  {
    Panel objectPanel = new Panel();
    objectPanel.setLayout(new BorderLayout());
    Random random = new Random();
    int x = (byte) random.nextInt();
    int y = (byte) random.nextInt();
    object = new ObjectComponent(new Point(x, y));
    Panel objectButtons = new Panel();
    copyObject = new Button("Copy object");
    copyObject.addActionListener(this);
    pasteObject = new Button("Paste object");
    pasteObject.addActionListener(this);
    objectButtons.add(copyObject);
    objectButtons.add(pasteObject);
    objectPanel.add(object, BorderLayout.CENTER);
    objectPanel.add(objectButtons, BorderLayout.SOUTH);
    return objectPanel;
  }
  
  /**
   * The Files Panel shows the files from the current working
   * directory. They can be copied and pasted between other
   * applications that support the exchange of file lists.
   */
  private Panel createFilesPanel()
  {
    Panel filesPanel = new Panel();
    filesPanel.setLayout(new BorderLayout());
    files = new FilesComponent(new File(".").listFiles());
    Panel filesButtons = new Panel();
    copyFiles = new Button("Copy files");
    copyFiles.addActionListener(this);
    pasteFiles = new Button("Paste files");
    pasteFiles.addActionListener(this);
    filesButtons.add(copyFiles);
    filesButtons.add(pasteFiles);
    filesPanel.add(files, BorderLayout.CENTER);
    filesPanel.add(filesButtons, BorderLayout.SOUTH);
    return filesPanel;
  }
  
  /**
   * The Flavors Panel shows the different formats (mime-types) that
   * data on the clipboard is available in. By clicking on a flavor
   * details about the representation class and object is given.
   */
  private Panel createFlavorsPanel()
  {
    Panel flavorsPanel = new Panel();
    flavorsPanel.setLayout(new BorderLayout());
    Label flavorsHeader = new Label("Flavors on clipboard:");
    Toolkit t = Toolkit.getDefaultToolkit();
    Clipboard c = t.getSystemClipboard();
    DataFlavor[] dataflavors = c.getAvailableDataFlavors();
    flavors = new FlavorsComponent(dataflavors);
    details = new FlavorDetailsComponent(null);
    flavorsPanel.add(flavorsHeader, BorderLayout.NORTH);
    flavorsPanel.add(flavors, BorderLayout.CENTER);
    flavorsPanel.add(details, BorderLayout.SOUTH);
    return flavorsPanel;
  }

  /**
   * FlavorListener implementation that updates the Flavors Panel
   * whenever a change in the mime-types available has been detected.
   */
  public void flavorsChanged(FlavorEvent event)
  {
    Toolkit t = Toolkit.getDefaultToolkit();
    Clipboard c = t.getSystemClipboard();
    DataFlavor[] dataflavors = c.getAvailableDataFlavors();
    flavors.setFlavors(dataflavors);
    details.setDataFlavor(null);
  }

  /**
   * ItemChangeListener implementation that updates the flavor details
   * whenever the user selects a different representation of the data
   * available on the clipboard.
   */
  public void itemStateChanged(ItemEvent evt)
  {
    DataFlavor df = null;
    String s = flavors.getSelectedItem();
    if (s != null)
      {
	try
	  {
	    df = new DataFlavor(s);
	  }
	catch (ClassNotFoundException cnfe)
	  {
	    cnfe.printStackTrace();
	  }
      }
    details.setDataFlavor(df);
  }
  
  /**
   * ActionListener implementations that will copy or past data
   * to/from the clipboard when the user requests that for the text,
   * image, object of file component.
   */
  public void actionPerformed (ActionEvent evt)
  {
    Button b = (Button) evt.getSource();
    Toolkit t = Toolkit.getDefaultToolkit();
    Clipboard c = t.getSystemClipboard();
    if (b == copyText)
      c.setContents(new StringSelection(text.getText()), null);

    if (b == pasteText)
      {
	String s = null;
	try
	  {
	    s = (String) c.getData(DataFlavor.stringFlavor);
	  }
	catch (UnsupportedFlavorException dfnse)
	  {
	  }
	catch (IOException ioe)
	  {
	  }
	catch (ClassCastException cce)
	  {
	  }
	if (s == null)
	  t.beep();
	else
	  text.setText(s);
      }

    if (b == copyImage)
      c.setContents(new ImageSelection(image.getImage()), null);

    if (b == pasteImage)
      {
	Image i = null;
	try
	  {
	    i = (Image) c.getData(DataFlavor.imageFlavor);
	  }
	catch (UnsupportedFlavorException dfnse)
	  {
	  }
	catch (IOException ioe)
	  {
	  }
	catch (ClassCastException cce)
	  {
	  }
	if (i == null)
	  t.beep();
	else
	  image.setImage(i);
      }

    if (b == copyObject)
      c.setContents(new ObjectSelection(object.getObject()), null);

    if (b == pasteObject)
      {
	Serializable o = null;
	try
	  {
	    o = (Serializable) c.getData(ObjectSelection.objFlavor);
	  }
	catch (UnsupportedFlavorException dfnse)
	  {
	  }
	catch (IOException ioe)
	  {
	  }
	catch (ClassCastException cce)
	  {
	  }
	if (o == null)
	  t.beep();
	else
	  object.setObject(o);
      }

    if (b == copyFiles)
      c.setContents(new FilesSelection(files.getFiles()), null);

    if (b == pasteFiles)
      {
	java.util.List fs = null;
	try
	  {
	    fs = (java.util.List) c.getData(DataFlavor.javaFileListFlavor);
	  }
	catch (UnsupportedFlavorException dfnse)
	  {
	  }
	catch (IOException ioe)
	  {
	  }
	catch (ClassCastException cce)
	  {
	  }
	if (fs == null)
	  t.beep();
	else
	  files.setFiles(fs);
      }
  }

  /**
   * Simple awt component that shows an settable image.
   */
  static class ImageComponent extends Component
  {
    private Image image;

    ImageComponent(Image image)
    {
      setSize(20, 20);
      setImage(image);
    }

    Image getImage()
    {
      return image;
    }

    void setImage(Image image)
    {
      this.image = image;
      repaint();
    }

    public void paint(Graphics g)
    {
      g.drawImage(image, 0, 0, getWidth(), getHeight(), this);
    }
  }

  /**
   * Simple awt component that shows a settable Serializable object.
   */
  static class ObjectComponent extends TextArea
  {
    private Serializable object;

    ObjectComponent(Serializable object)
    {
      super("", 2, 80, TextArea.SCROLLBARS_NONE);
      setEditable(false);
      setEnabled(false);
      setObject(object);
    }

    Serializable getObject()
    {
      return object;
    }

    void setObject(Serializable object)
    {
      this.object = object;
      setText("Class: " + object.getClass().getName()
	      + "\n"
	      + "toString(): " + object.toString());
      repaint();
    }
  }

  /**
   * Simple awt component that shows a settable list of Files.
   */
  static class FilesComponent extends List
  {
    private File[] files;
    
    FilesComponent(File[] files)
    {
      super(4, true);
      setFiles(files);
    }
    
    File[] getFiles()
    {
      String[] strings = getSelectedItems();
      if (strings == null || strings.length == 0)
	return (File[]) files.clone();
      
      File[] fs = new File[strings.length];
      for (int i = 0; i < strings.length; i++)
	fs[i] = new File(strings[i]);
      return fs;
    }
    
    void setFiles(File[] files)
    {
      this.files = files;
      removeAll();
      for (int i = 0; i < files.length; i++)
        {
	  addItem(files[i].toString());
	  select(i);
        }
    }
    
    void setFiles(java.util.List list)
    {
      File[] fs = new File[list.size()];
      int i = 0;
      Iterator it = list.iterator();
      while (it.hasNext())
	fs[i++] = (File) it.next();
      
      setFiles(fs);
    }
  }

  /**
   * Simple awt component that shows a settable list of DataFlavors.
   */
  static class FlavorsComponent extends List
  {
    FlavorsComponent(DataFlavor[] flavors)
    {
      super(4);
      setFlavors(flavors);
    }

    void setFlavors(DataFlavor[] flavors)
    {
      removeAll();
      for (int i = 0; i < flavors.length; i++)
	{
	  addItem(flavors[i].getMimeType());
	}
    }
  }

  /**
   * Simple awt component that shows the details for and an object as
   * found on the system clipboard as represented by a given
   * DataFlavor.
   */
  static class FlavorDetailsComponent extends TextArea
  {
    private DataFlavor df;

    FlavorDetailsComponent(DataFlavor df)
    {
      super("", 2, 80, TextArea.SCROLLBARS_NONE);
      setEditable(false);
      setEnabled(false);
      setDataFlavor(df);
    }

    void setDataFlavor(DataFlavor df)
    {
      if (df == this.df
	  || (df != null && df.equals(this.df)))
	return;

      this.df = df;

      if (df == null)
	setText("No flavor selected");
      else
	{
	  Object o = null;
	  Throwable exception = null;
	  try
	    {
	      Toolkit t = Toolkit.getDefaultToolkit();
	      Clipboard c = t.getSystemClipboard();
	      o = c.getData(df);
	    }
	  catch (Throwable t)
	    {
	      exception = t;
	    }
	  if (o != null)
	    {
	      setText("Data: " + o.getClass().getName()
		      + "\n"
		      + o);
	    }
	  else
	    {
	      setText("Error retrieving: " + df
		      + "\n"
		      + exception != null ? exception.toString() : "");
	    }
	}
      repaint();
    }
  }

  /**
   * Helper class to put an Image on a clipboard as
   * DataFlavor.imageFlavor.
   */
  static class ImageSelection implements Transferable
  {
    private final Image img;

    ImageSelection(Image img)
    {
      this.img = img;
    }

    static DataFlavor[] flavors = new DataFlavor[] { DataFlavor.imageFlavor };
    public DataFlavor[] getTransferDataFlavors()
    {
      return (DataFlavor[]) flavors.clone();
    }

    public boolean isDataFlavorSupported(DataFlavor flavor)
    {
      return flavor.equals(DataFlavor.imageFlavor);
    }

    public Object getTransferData(DataFlavor flavor)
      throws UnsupportedFlavorException
    {
      if (!isDataFlavorSupported(flavor))
	throw new UnsupportedFlavorException(flavor);

      return img;
    }
  }

  /**
   * Helper class to put an Object on a clipboard as Serializable
   * object.
   */
  static class ObjectSelection implements Transferable
  {
    private final Serializable obj;

    ObjectSelection(Serializable obj)
    {
      this.obj = obj;
    }

    static DataFlavor objFlavor = new DataFlavor(Serializable.class,
						 "Serialized Object");
    static DataFlavor[] flavors = new DataFlavor[] { objFlavor };
    public DataFlavor[] getTransferDataFlavors()
    {
      return (DataFlavor[]) flavors.clone();
    }

    public boolean isDataFlavorSupported(DataFlavor flavor)
    {
      return flavor.equals(objFlavor);
    }

    public Object getTransferData(DataFlavor flavor)
      throws UnsupportedFlavorException
    {
      if (!isDataFlavorSupported(flavor))
	throw new UnsupportedFlavorException(flavor);

      return obj;
    }
  }

  /**
   * Helper class to put a List of Files on the clipboard as
   * DataFlavor.javaFileListFlavor.
   */
  static class FilesSelection implements Transferable
  {
    private final File[] files;

    FilesSelection(File[] files)
    {
      this.files = files;
    }

    static DataFlavor[] flavors = new DataFlavor[]
      { DataFlavor.javaFileListFlavor };
    public DataFlavor[] getTransferDataFlavors()
    {
      return (DataFlavor[]) flavors.clone();
    }

    public boolean isDataFlavorSupported(DataFlavor flavor)
    {
      return flavor.equals(DataFlavor.javaFileListFlavor);
    }

    public Object getTransferData(DataFlavor flavor)
      throws UnsupportedFlavorException
    {
      if (!isDataFlavorSupported(flavor))
	throw new UnsupportedFlavorException(flavor);

      return Arrays.asList(files);
    }
  }
}
