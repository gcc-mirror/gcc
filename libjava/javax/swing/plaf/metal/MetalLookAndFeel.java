package javax.swing.plaf.metal;

import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.*;


public class MetalLookAndFeel extends LookAndFeel
 {	   
     public boolean isNativeLookAndFeel()        { return true; }
     public boolean isSupportedLookAndFeel()     { return true; }
     public String getDescription()              { return "Metal look and feel"; }
     public String getID()                       { return "MetalLookAndFeel"; }
     public String getName()                     { return "MetalLookAndFeel"; }
     
     
     UIDefaults LAF_defaults;
     
     public MetalLookAndFeel()
     {
     }

     public UIDefaults getDefaults()
	 {
	   if (LAF_defaults == null)
	     LAF_defaults = new BasicDefaults();
	     
	     //      Returns the default values for this look and feel. 
	     return LAF_defaults;
	 }
 };
