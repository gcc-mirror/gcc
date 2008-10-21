/* JobAttributes.java -- 
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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


package java.awt;

import gnu.java.lang.CPStringBuilder;

/**
 * Needs documentation...
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.3
 * @status updated to 1.4, lacks documentation
 */
public final class JobAttributes implements Cloneable
{
  public static final class DefaultSelectionType extends AttributeValue
  {
    private static final String[] NAMES = { "all", "range", "selection" };
    public static final DefaultSelectionType ALL
      = new DefaultSelectionType(0);
    public static final DefaultSelectionType RANGE
      = new DefaultSelectionType(1);
    public static final DefaultSelectionType SELECTION
      = new DefaultSelectionType(2);
    private DefaultSelectionType(int value)
    {
      super(value, NAMES);
    }
  } // class DefaultSelectionType

  public static final class DestinationType extends AttributeValue
  {
    private static final String[] NAMES = { "file", "printer" };
    public static final DestinationType FILE = new DestinationType(0);
    public static final DestinationType PRINTER = new DestinationType(1);
    private DestinationType(int value)
    {
      super(value, NAMES);
    }
  } // class DestinationType

  public static final class DialogType extends AttributeValue
  {
    private static final String[] NAMES = { "common", "native", "none" };
    public static final DialogType COMMON = new DialogType(0);
    public static final DialogType NATIVE = new DialogType(1);
    public static final DialogType NONE = new DialogType(2);
    private DialogType(int value)
    {
      super(value, NAMES);
    }
  } // class DialogType

  public static final class MultipleDocumentHandlingType
    extends AttributeValue
  {
    private static final String[] NAMES = {
      "separate-documents-collated-copies",
      "separate-documents-uncollated-copies"
    };
    public static final MultipleDocumentHandlingType
      SEPARATE_DOCUMENTS_COLLATED_COPIES
      = new MultipleDocumentHandlingType(0);
    public static final MultipleDocumentHandlingType
      SEPARATE_DOCUMENTS_UNCOLLATED_COPIES
      = new MultipleDocumentHandlingType(1);
    private MultipleDocumentHandlingType(int value)
    {
      super(value, NAMES);
    }
  } // class MultipleDocumentHandlingType

  public static final class SidesType extends AttributeValue
  {
    private static final String[] NAMES
      = { "one-sided", "two-sided-long-edge", "two-sided-short-edge" };
    public static final SidesType ONE_SIDED = new SidesType(0);
    public static final SidesType TWO_SIDED_LONG_EDGE = new SidesType(1);
    public static final SidesType TWO_SIDED_SHORT_EDGE = new SidesType(2);
    private SidesType(int value)
    {
      super(value, NAMES);
    }
  } // class SidesType

  private int copies;
  private DefaultSelectionType selection;
  private DestinationType destination;
  private DialogType dialog;
  private String filename;
  private int maxPage;
  private int minPage;
  private MultipleDocumentHandlingType multiple;
  private int[][] pageRanges; // null for default value
  private int fromPage; // 0 for default value
  private int toPage; // 0 for default value
  private String printer;
  private SidesType sides;

  public JobAttributes()
  {
    copies = 1;
    selection = DefaultSelectionType.ALL;
    destination = DestinationType.PRINTER;
    dialog = DialogType.NATIVE;
    maxPage = Integer.MAX_VALUE;
    minPage = 1;
    multiple
      = MultipleDocumentHandlingType.SEPARATE_DOCUMENTS_UNCOLLATED_COPIES;
    sides = SidesType.ONE_SIDED;
  }

  public JobAttributes(JobAttributes attr)
  {
    set(attr);
  }

  public JobAttributes(int copies, DefaultSelectionType selection,
                       DestinationType destination, DialogType dialog,
                       String filename, int max, int min,
                       MultipleDocumentHandlingType multiple,
                       int[][] pageRanges, String printer, SidesType sides)
  {
    if (copies <= 0 || selection == null || destination == null
        || dialog == null || max < min || min <= 0 || multiple == null
        || sides == null)
      throw new IllegalArgumentException();
    this.copies = copies;
    this.selection = selection;
    this.destination = destination;
    this.dialog = dialog;
    this.filename = filename;
    maxPage = max;
    minPage = min;
    this.multiple = multiple;
    setPageRanges(pageRanges);
    this.printer = printer;
    this.sides = sides;
  }

  public Object clone()
  {
    return new JobAttributes(this);
  }

  public void set(JobAttributes attr)
  {
    copies = attr.copies;
    selection = attr.selection;
    destination = attr.destination;
    dialog = attr.dialog;
    filename = attr.filename;
    maxPage = attr.maxPage;
    minPage = attr.minPage;
    multiple = attr.multiple;
    pageRanges = (int[][]) attr.pageRanges.clone();
    printer = attr.printer;
    sides = attr.sides;
    fromPage = attr.fromPage;
    toPage = attr.toPage;
  }

  public int getCopies()
  {
    return copies;
  }

  public void setCopies(int copies)
  {
    if (copies <= 0)
      throw new IllegalArgumentException();
    this.copies = copies;
  }

  public void setCopiesToDefault()
  {
    copies = 1;
  }

  public DefaultSelectionType getDefaultSelection()
  {
    return selection;
  }

  public void setDefaultSelection(DefaultSelectionType selection)
  {
    if (selection == null)
      throw new IllegalArgumentException();
    this.selection = selection;
  }

  public DestinationType getDestination()
  {
    return destination;
  }

  public void setDestination(DestinationType destination)
  {
    if (destination == null)
      throw new IllegalArgumentException();
    this.destination = destination;
  }

  public DialogType getDialog()
  {
    return dialog;
  }

  public void setDialog(DialogType dialog)
  {
    if (dialog == null)
      throw new IllegalArgumentException();
    this.dialog = dialog;
  }

  public String getFileName()
  {
    return filename;
  }

  public void setFileName(String filename)
  {
    this.filename = filename;
  }

  public int getFromPage()
  {
    return fromPage != 0 ? fromPage
      : pageRanges != null ? pageRanges[0][0]
      : toPage != 0 ? toPage : minPage;
  }

  public void setFromPage(int fromPage)
  {
    if (fromPage < minPage || (fromPage > toPage && toPage != 0)
        || fromPage > maxPage)
      throw new IllegalArgumentException();
    if (pageRanges == null)
      this.fromPage = fromPage;
  }

  public int getMaxPage()
  {
    return maxPage;
  }

  public void setMaxPage(int maxPage)
  {
    if (maxPage < minPage)
      throw new IllegalArgumentException();
    this.maxPage = maxPage;
    if (maxPage < fromPage)
      fromPage = maxPage;
    if (maxPage < toPage)
      toPage = maxPage;
    if (pageRanges != null)
      {
        int i = pageRanges.length - 1;
        while (i >= 0 && maxPage < pageRanges[i][1])
          i--;
        if (maxPage >= pageRanges[++i][0])
          pageRanges[i++][1] = maxPage;
        if (i == 0)
          pageRanges = null;
        else if (i < pageRanges.length)
          {
            int[][] tmp = new int[i][];
            System.arraycopy(pageRanges, 0, tmp, 0, i);
            pageRanges = tmp;
          }
      }
  }

  public int getMinPage()
  {
    return minPage;
  }

  public void setMinPage(int minPage)
  {
    if (minPage <= 0 || minPage > maxPage)
      throw new IllegalArgumentException();
    this.minPage = minPage;
    if (minPage > toPage)
      toPage = minPage;
    if (minPage > fromPage)
      fromPage = minPage;
    if (pageRanges != null)
      {
        int size = pageRanges.length;
        int i = 0;
        while (i < size && minPage > pageRanges[i][0])
          i++;
        if (minPage <= pageRanges[i - 1][1])
          pageRanges[--i][0] = minPage;
        if (i == size)
          pageRanges = null;
        else if (i > 0)
          {
            int[][] tmp = new int[size - i][];
            System.arraycopy(pageRanges, i, tmp, 0, size - i);
            pageRanges = tmp;
          }
      }
  }

  public MultipleDocumentHandlingType getMultipleDocumentHandling()
  {
    return multiple;
  }

  public void setMultipleDocumentHandling
    (MultipleDocumentHandlingType multiple)
  {
    if (multiple == null)
      throw new IllegalArgumentException();
    this.multiple = multiple;
  }

  public void setMultipleDocumentHandlingToDefault()
  {
    multiple
      = MultipleDocumentHandlingType.SEPARATE_DOCUMENTS_UNCOLLATED_COPIES;
  }

  public int[][] getPageRanges()
  {
    if (pageRanges == null)
      return new int[][] { { getFromPage(), getToPage() } };
    // Perform a deep clone, so user code cannot affect original arrays.
    int i = pageRanges.length;
    int[][] result = new int[i][];
    while (--i >= 0)
      result[i] = (int[]) pageRanges[i].clone();
    return result;
  }

  public void setPageRanges(int[][] pageRanges)
  {
    int size = pageRanges == null ? 0 : pageRanges.length;
    if (size == 0)
      throw new IllegalArgumentException();
    while (--size >= 0)
      {
        int[] range = pageRanges[size];
        if (range == null || range.length != 2
            || range[0] < minPage || range[1] < range[0] || range[1] > maxPage
            || (size != 0 && range[0] <= pageRanges[size - 1][1]))
          throw new IllegalArgumentException();
      }
    size = pageRanges.length;
    if (fromPage > 0 && pageRanges[0][0] > fromPage)
      fromPage = pageRanges[0][0];
    if (toPage > 0 && pageRanges[size - 1][1] < toPage)
      toPage = pageRanges[size - 1][1];
    this.pageRanges = new int[size][];
    while (--size >= 0)
      this.pageRanges[size] = (int[]) pageRanges[size].clone();
  }

  public String getPrinter()
  {
    return printer;
  }

  public void setPrinter(String printer)
  {
    this.printer = printer;
  }

  public SidesType getSides()
  {
    return sides;
  }

  public void setSides(SidesType sides)
  {
    if (sides == null)
      throw new IllegalArgumentException();
    this.sides = sides;
  }

  public void setSidesToDefault()
  {
    sides = SidesType.ONE_SIDED;
  }

  public int getToPage()
  {
    return toPage != 0 ? toPage
      : pageRanges != null ? pageRanges[pageRanges.length - 1][1]
      : fromPage != 0 ? fromPage : maxPage;
  }

  public void setToPage(int toPage)
  {
    if (toPage < minPage || (fromPage > toPage && fromPage != 0)
        || toPage > maxPage)
      throw new IllegalArgumentException();
    if (pageRanges == null)
      this.toPage = toPage;
  }

  public boolean equals(Object o)
  {
    if (this == o)
      return true;
    if (! (o instanceof JobAttributes))
      return false;
    JobAttributes ja = (JobAttributes) o;
    if (copies != ja.copies || selection != ja.selection
        || destination != ja.destination || dialog != ja.dialog
        || ! filename.equals(ja.filename) || maxPage != ja.maxPage
        || minPage != ja.minPage || multiple != ja.multiple
        || fromPage != ja.fromPage || toPage != ja.toPage
        || ! printer.equals(ja.printer) || sides != ja.sides
        || (pageRanges == null) != (ja.pageRanges == null))
      return false;
    if (pageRanges != ja.pageRanges)
      for (int i = pageRanges.length; --i >= 0; )
        if (pageRanges[i][0] != ja.pageRanges[i][0]
            || pageRanges[i][1] != ja.pageRanges[i][1])
          return false;
    return true;
  }

  public int hashCode()
  {
    int hash = (selection.value << 6) ^ (destination.value << 5)
      ^ (dialog.value << 3) ^ (multiple.value << 2) ^ sides.value
      ^ (filename == null ? 0 : filename.hashCode())
      ^ (printer == null ? 0 : printer.hashCode());
    // The effect of the above fields on the hashcode match the JDK. However,
    // I am unable to reverse engineer the effect of the fields listed below,
    // so I am using my own implementation. Note that this still satisfies
    // the general contract of hashcode, it just doesn't match the JDK.
    hash ^= (copies << 27) ^ (maxPage << 22) ^ (minPage << 17);
    if (pageRanges == null)
      hash ^= (getFromPage() << 13) ^ (getToPage() << 8);
    else
      for (int i = pageRanges.length; --i >= 0; )
        hash ^= (pageRanges[i][0] << 13) ^ (pageRanges[i][1] << 8);
    return hash;
  }

  public String toString()
  {
    CPStringBuilder s = new CPStringBuilder("copies=").append(copies)
      .append(",defaultSelection=").append(selection).append(",destination=")
      .append(destination).append(",dialog=").append(dialog)
      .append(",fileName=").append(filename).append(",fromPage=")
      .append(getFromPage()).append(",maxPage=").append(maxPage)
      .append(",minPage=").append(minPage)
      .append(",multiple-document-handling=").append(multiple)
      .append(",page-ranges=[");
    if (pageRanges == null)
      s.append(minPage).append(':').append(minPage).append(']');
    else
      for (int i = 0; i < pageRanges.length; i++)
        s.append(pageRanges[i][0]).append(':').append(pageRanges[i][1])
          .append(',');
    s.setLength(s.length() - 1);
    return s.append("],printer=").append(printer).append(",sides=")
      .append(sides).append(",toPage=").append(getToPage()).toString();
  }
} // class JobAttributes
