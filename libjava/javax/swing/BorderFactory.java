package javax.swing;

import java.awt.*;
import java.awt.font.*;
import javax.swing.border.*;


public class BorderFactory
{
  public static Border createLineBorder(Color color)
  { /*
    Creates a line border withe the specified color.

    Parameters:
    color - a Color to use for the lineReturns:
    the Border object
createLineBorder
    */ 
    return null;
 }

public static Border createLineBorder(Color color,
                                      int thickness)
  { /*

    Creates a line border withe the specified color and width. The width applies to all 4 sides of the border. To specify widths individually for the top, bottom, left, and right, use createMatteBorder(int,int,int,int,Color).

    Parameters:
    color - a Color to use for the linethickness - an int specifying the width in pixelsReturns:
    the Border object
createRaisedBevelBorder
    */ 
    return null;
  }


public static Border createRaisedBevelBorder()
  { /*

    Created a border with a raised beveled edge, using brighter shades of the component's current background color for highlighting, and darker shading for shadows. (In a raised border, highlights are on top and shadows are underneath.)

    Returns:
    the Border object
createLoweredBevelBorder

    */ 
    return null;
 }

public static Border createLoweredBevelBorder()
  { /*

    Created a border with a lowered beveled edge, using brighter shades of the component's current background color for highlighting, and darker shading for shadows. (In a lowered border, shadows are on top and highlights are underneath.)

    Returns:
    the Border object
createBevelBorder

    */ 
    return null;
 }

public static Border createBevelBorder(int type)

  { /*
    Create a beveled border of the specified type, using brighter shades of the component's current background color for highlighting, and darker shading for shadows. (In a lowered border, shadows are on top and highlights are underneath.).

    Parameters:
    type - an int specifying either BevelBorder.LOWERED or BevelBorder.LOWEREDReturns:
    the Border object
createBevelBorder

    */ 
    return null;
 }

public static Border createBevelBorder(int type,
                                       Color highlight,
                                       Color shadow)
  { /*

    Create a beveled border of the specified type, using the specified highlighting and shadowing. The outer edge of the highlighted area uses a brighter shade of the highlight color. The inner edge of the shadow area uses a brighter shade of the shadaw color.

    Parameters:
    type - an int specifying either BevelBorder.LOWERED or BevelBorder.LOWEREDhighlight - a Color object for highlightsshadow - a Color object for shadowsReturns:
    the Border object
createBevelBorder

    */ 
    return null;
 }

public static Border createBevelBorder(int type,
                                       Color highlightOuter,
                                       Color highlightInner,
                                       Color shadowOuter,
                                       Color shadowInner)
  { /*

    Create a beveled border of the specified type, using the specified colors for the inner and outer highlight and shadow areas.

    Parameters:
    type - an int specifying either BevelBorder.LOWERED or BevelBorder.LOWEREDhighlightOuter - a Color object for the outer edge of the highlight areahighlightInner - a Color object for the inner edge of the highlight areashadowOuter - a Color object for the outer edge of the shadow areashadowInner - a Color object for the inner edge of the shadow areaReturns:
    the Border object
createEtchedBorder
    */ 
    return null;
 }


public static Border createEtchedBorder()
  { /*

    Create a border with an "etched" look using the component's current background color for highlighting and shading.

    Returns:
    the Border object
createEtchedBorder

    */ 
    return null;
 }

public static Border createEtchedBorder(Color highlight,
                                        Color shadow)
  { /*

    Create a border with an "etched" look using the specified highlighting and shading colors.

    Parameters:
    highlight - a Color object for the border highlightsshadow - a Color object for the border shadowsReturns:
    the Border object
createTitledBorder

    */ 
    return null;
 }

  public static TitledBorder createTitledBorder(String title)
  { /*
    Create a new title border specifying the text of the title, using the default border (etched), using the default text position (sitting on the top line) and default justification (left) and using the default font and text color determined by the current look and feel.

    Parameters:
    title - a String containing the text of the titleReturns:
    the TitledBorder object
createTitledBorder

    */ 
    return null;
 }

  public static TitledBorder createTitledBorder(Border border)
  { /*

    Create a new title border with an empty title specifying the border object, using the default text position (sitting on the top line) and default justification (left) and using the default font, text color, and border determined by the current look and feel. (The Motif and Windows look and feels use an etched border; The Java look and feel use a gray border.)

    Parameters:
    border - the Border object to add the title toReturns:
    the TitledBorder object
createTitledBorder

    */ 
    return null;
 }

public static TitledBorder createTitledBorder(Border border,
                                              String title)
  { /*

    Add a title to an existing border, specifying the text of the title, using the default positioning (sitting on the top line) and default justification (left) and using the default font and text color determined by the current look and feel.

    Parameters:
    border - the Border object to add the title totitle - a String containing the text of the titleReturns:
    the TitledBorder object
createTitledBorder

    */ 
    return null;
 }

public static TitledBorder createTitledBorder(Border border,
                                              String title,
                                              int titleJustification,
                                              int titlePosition)
  { /*

    Add a title to an existing border, specifying the text of the title along with its positioning, using the default font and text color determined by the current look and feel.

    Parameters:
    border - the Border object to add the title totitle - a String containing the text of the titletitleJustification - an int specifying the left/right position of the title -- one of TitledBorder.LEFT, TitledBorder.CENTER, or TitledBorder.RIGHT, TitledBorder.DEFAULT_JUSTIFICATION (left).titlePosition - an int specifying the vertical position of the text in relation to the border -- one of: TitledBorder.ABOVE_TOP, TitledBorder.TOP (sitting on the top line), TitledBorder.BELOW_TOP, TitledBorder.ABOVE_BOTTOM, TitledBorder.BOTTOM (sitting on the bottom line), TitledBorder.BELOW_BOTTOM, or TitledBorder.DEFAULT_POSITION (top).Returns:
    the TitledBorder object
createTitledBorder

    */ 
    return null;
 }

public static TitledBorder createTitledBorder(Border border,
                                              String title,
                                              int titleJustification,
                                              int titlePosition,
                                              Font titleFont)
  { /*

    Add a title to an existing border, specifying the text of the title along with its positioning and font, using the default text color determined by the current look and feel.

    Parameters:
    border - the Border object to add the title totitle - a String containing the text of the titletitleJustification - an int specifying the left/right position of the title -- one of TitledBorder.LEFT, TitledBorder.CENTER, or TitledBorder.RIGHT, TitledBorder.DEFAULT_JUSTIFICATION (left).titlePosition - an int specifying the vertical position of the text in relation to the border -- one of: TitledBorder.ABOVE_TOP, TitledBorder.TOP (sitting on the top line), TitledBorder.BELOW_TOP, TitledBorder.ABOVE_BOTTOM, TitledBorder.BOTTOM (sitting on the bottom line), TitledBorder.BELOW_BOTTOM, or TitledBorder.DEFAULT_POSITION (top).titleFont - a Font object specifying the title fontReturns:
    the TitledBorder object
createTitledBorder

    */ 
    return null;
 }

public static TitledBorder createTitledBorder(Border border,
                                              String title,
                                              int titleJustification,
                                              int titlePosition,
                                              Font titleFont,
                                              Color titleColor)
  { /*

    Add a title to an existing border, specifying the text of the title along with its positioning, font, and color.

    Parameters:
    border - the Border object to add the title totitle - a String containing the text of the titletitleJustification - an int specifying the left/right position of the title -- one of TitledBorder.LEFT, TitledBorder.CENTER, or TitledBorder.RIGHT, TitledBorder.DEFAULT_JUSTIFICATION (left).titlePosition - an int specifying the vertical position of the text in relation to the border -- one of: TitledBorder.ABOVE_TOP, TitledBorder.TOP (sitting on the top line), TitledBorder.BELOW_TOP, TitledBorder.ABOVE_BOTTOM, TitledBorder.BOTTOM (sitting on the bottom line), TitledBorder.BELOW_BOTTOM, or TitledBorder.DEFAULT_POSITION (top).titleFont - a Font object specifying the title fonttitleColor - a Color object specifying the title colorReturns:
    the TitledBorder object
createEmptyBorder

    */ 
    return null;
 }

public static Border createEmptyBorder()
  { /*

    Creates an empty border that takes up no space. (The width of the top, bottom, left, and right sides are all zero.)

    Returns:
    the Border object
createEmptyBorder

    */ 
    return null;
 }

public static Border createEmptyBorder(int top,
                                       int left,
                                       int bottom,
                                       int right)
  { /*

    Creates an empty border that takes up no space but which does no drawing, specifying the width of the top, left, bottom, and right sides.

    Parameters:
    top - an int specifying the width of the top in pixelsleft - an int specifying the width of the left side in pixelsbottom - an int specifying the width of the right side in pixelsright - an int specifying the width of the bottom in pixelsReturns:
    the Border object
createCompoundBorder

    */ 
    return null;
 }

public static CompoundBorder createCompoundBorder()
  { /*

    Create a compound border with a null inside edge and a null outside edge.

    Returns:
    the CompoundBorder object
createCompoundBorder
    */ 
    return null;
 }


public static CompoundBorder createCompoundBorder(Border outsideBorder,
                                                  Border insideBorder)
  { /*

    Create a compound border specifying the border objects to use for the outside and inside edges.

    Parameters:
    outsideBorder - a Border object for the outer edge of the compound borderinsideBorder - a Border object for the inner edge of the compound borderReturns:
    the CompoundBorder object
createMatteBorder
    */ 
    return null;
 }


public static MatteBorder createMatteBorder(int top,
                                            int left,
                                            int bottom,
                                            int right,
                                            Color color)
  { /*

    Create a matte-look border using a solid color. (The difference between this border and a line border is that you can specify the individual border dimensions.)

    Parameters:
    top - an int specifying the width of the top in pixelsleft - an int specifying the width of the left side in pixelsbottom - an int specifying the width of the right side in pixelsright - an int specifying the width of the bottom in pixelscolor - a Color to use for the borderReturns:
    the MatteBorder object
createMatteBorder

    */ 
    return null;
 }

public static MatteBorder createMatteBorder(int top,
                                            int left,
                                            int bottom,
                                            int right,
                                            Icon tileIcon)
  { /*

    Create a matte-look border that consists of multiple tiles of a specified icon. Multiple copies of the icon are placed side-by-side to fill up the border area.

    Note:
    If the icon doesn't load, the border area is painted gray.

    Parameters:
    top - an int specifying the width of the top in pixelsleft - an int specifying the width of the left side in pixelsbottom - an int specifying the width of the right side in pixelsright - an int specifying the width of the bottom in pixelstileIcon - the Icon object used for the border tilesReturns:
    the MatteBorder object

    */ 
    return null;
 }

}
