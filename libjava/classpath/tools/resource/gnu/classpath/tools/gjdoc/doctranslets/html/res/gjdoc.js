/* gjdoc.js
 Copyright (C) 2004 Free Software Foundation, Inc.

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
 02111-1307 USA. */

/**
 *  Sets a new stylesheet for a single frame.
 */
function setStyleSheetForFrame(title, frame_name)
{
   var i, num, a, main;
   f = eval("top.frames." + frame_name);
   for(i=0; (a = f.document.getElementsByTagName("link")[i]); i++) {
      if(a.getAttribute("rel").indexOf("style") != -1
         && a.getAttribute("title")) {
         a.disabled = true;
         a.disabled = (a.getAttribute("title") != title);
      }
   }
}

/**
 *  Sets a new stylesheet for a single document.
 */
function setStyleSheetForDocument(title) 
{
   var i, num, a, main;
   for(i=0; (a = top.document.getElementsByTagName("link")[i]); i++) {
      if(a.getAttribute("rel").indexOf("style") != -1
         && a.getAttribute("title")) {
         a.disabled = true;
         a.disabled = (a.getAttribute("title") != title);        
      }
   }
}

/**
 *  Sets a new stylesheets for all frames.
 */
function setStyleSheet(title)
{
   var i, num, a, main;
   num = top.frames.length;
   if (num > 0) {
      for (i=0; i<num; i++) {
         setStyleSheetForFrame(title, top.frames[i].name);
      }
   }
   else {
      setStyleSheetForDocument(title);
   }
   top.selectedSheet = title;
   return false;
}

/**
 *  Returns the title of the set of stylesheets that is active in the
 *  given frame.
 */
function getActiveStylesheetTitle(frame)
{ 
   for(i=0; (a = frame.document.getElementsByTagName("link")[i]); i++) {
      if(a.getAttribute("rel").indexOf("style") != -1
         && a.getAttribute("title")) {
         if (!a.disabled) {
            return a.getAttribute("title");
         }    
      }
   }
   return "";
}

/**
 *  Invoked when a page in the main content frame has been loaded.
 */
function contentPageLoaded(title) 
{ 
   top.document.title = title;
   if (top.frames.length > 0) 
      setStyleSheet(getActiveStylesheetTitle(top.frames.packages), "content");
   else if (top.selectedSheet) { 
      setStyleSheet(top.selectedSheet, "content");
   }
}

/**
 *  Invoked when a page in one of the two frames on the left hand has
 *  been loaded.
 */
function secondaryPageLoaded(frame_name)
{ 
   setStyleSheet(getActiveStylesheetTitle(top.frames.packages), frame_name);
}
