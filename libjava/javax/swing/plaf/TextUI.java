package javax.swing.plaf;

import javax.swing.text.*;
import java.awt.*;

public abstract class TextUI extends ComponentUI
{
    public TextUI()
    {
    }
 
    public abstract  void damageRange(JTextComponent t, int p0, int p1);
    public abstract  void damageRange(JTextComponent t, int p0, int p1, Position.Bias firstBias, Position.Bias secondBias);
    public abstract  EditorKit getEditorKit(JTextComponent t);
    public abstract  int getNextVisualPositionFrom(JTextComponent t, 
					    int pos,
					    Position.Bias b, 
					    int direction,
					    Position.Bias[] biasRet);
    public abstract  View getRootView(JTextComponent t);
    public abstract  Rectangle modelToView(JTextComponent t, int pos);
    public abstract  Rectangle modelToView(JTextComponent t, int pos, Position.Bias bias);
    public abstract  int viewToModel(JTextComponent t, Point pt);
    public abstract  int viewToModel(JTextComponent t, Point pt, Position.Bias[] biasReturn);
 
}
