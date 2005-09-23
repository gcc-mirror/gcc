/* eventmethods.cpp --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

#ifdef I_KNOW_WHAT_IM_DOING

bool draw;
  
private:
  JavaVM* vm;
  jobject target;
  jclass componentCls;

  void setup(JNIEnv *env, jobject obj)
  {
    env->GetJavaVM(&vm);
    componentCls = NULL;
    target = env->NewGlobalRef(obj);
    componentCls = (jclass)env->NewGlobalRef(env->GetObjectClass( target ));
    setMouseTracking( true );
    draw = true;
  }

  void destroy()
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    env->DeleteGlobalRef(target);
    env->DeleteGlobalRef(componentCls);
  }

  void callVoidMethod(char *methodName)
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    jmethodID fireEventID = env->GetMethodID( componentCls,
					      methodName,
					      "()V" );
    env->CallVoidMethod( target, fireEventID );
  }

  void callMouseMethod(char *methodName, 
		       int modifiers, int x, int y, int clickCount)
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    jmethodID fireEventID = env->GetMethodID( componentCls,
					      methodName,
					      "(IIII)V" );
    env->CallVoidMethod( target, fireEventID, modifiers, x, y, clickCount );
  }

protected:

  void closeEvent( QCloseEvent *e )
  {
    PARENT::closeEvent(e);
    callVoidMethod("closeEvent");
  }
  
  void focusInEvent( QFocusEvent *e )
  {
    PARENT::focusInEvent(e);
    callVoidMethod("focusInEvent");
  }

  void focusOutEvent( QFocusEvent *e )
  {
    PARENT::focusOutEvent(e);
    callVoidMethod("focusOutEvent");
  }

  void enterEvent( QEvent *e )
  {
    PARENT::enterEvent(e);
    QPoint p = mapFromGlobal( QCursor::pos() );
    int modifiers = getKeyModifiers( QApplication::keyboardModifiers() );
    callMouseMethod("enterEvent", modifiers, p.x(), p.y(), 0);
  }

  void keyPressEvent( QKeyEvent *e )
  {
    PARENT::keyPressEvent(e);
    int modifiers, x, y;
    modifiers = getKeyModifiers(e->modifiers());
    x = mapKeyCode(e);
    y = getUnicode(e);
    callMouseMethod("keyPressEvent", modifiers, x, y, 0);
  }

  void keyReleaseEvent( QKeyEvent *e )
  {
    PARENT::keyReleaseEvent(e);
    int modifiers, x, y;
    modifiers = getKeyModifiers(e->modifiers());
    x = mapKeyCode(e);
    y = getUnicode(e);
    callMouseMethod("keyReleaseEvent", modifiers, x, y, 0);
  }

  void leaveEvent( QEvent *e )
  {
    PARENT::leaveEvent(e);
    QPoint p = mapFromGlobal( QCursor::pos() );
    int modifiers = getKeyModifiers( QApplication::keyboardModifiers() );
    callMouseMethod("leaveEvent", modifiers, p.x(), p.y(), 0);
  }

  void mouseDoubleClickEvent( QMouseEvent *e )
  {
    PARENT::mouseDoubleClickEvent(e);
    int modifiers, x, y, clickCount;
    clickCount = 2;
    modifiers = getMouseModifiers(e);
    x = e->x();
    y = e->y();
    callMouseMethod("mouseDoubleClickEvent", modifiers, x, y, clickCount);
  }

  void mouseMoveEvent( QMouseEvent *e )
  {
    PARENT::mouseMoveEvent(e);
    int modifiers, x, y, clickCount;
    clickCount = 0;
    modifiers = getMouseModifiers(e);
    x = e->x();
    y = e->y();
    callMouseMethod("mouseMoveEvent", modifiers, x, y, clickCount);
  }

  void mousePressEvent( QMouseEvent *e )
  {
    PARENT::mousePressEvent(e);
    int modifiers, x, y, clickCount;
    clickCount = 0;
    modifiers = getMouseModifiers(e);
    x = e->x();
    y = e->y();
    callMouseMethod("mousePressEvent", modifiers, x, y, clickCount);
  }

  void mouseReleaseEvent( QMouseEvent *e )
  {
    PARENT::mouseReleaseEvent(e);
    int modifiers, x, y, clickCount;
    modifiers = 0;

    modifiers |= getReleaseModifiers( e );
    x = e->x();
    y = e->y();
    callMouseMethod("mouseReleaseEvent", modifiers, x, y, 0);
  }

  void moveEvent( QMoveEvent *e )
  {
    PARENT::moveEvent(e);
    callMouseMethod("moveEvent", e->pos().x(), e->pos().y(),
		    e->oldPos().x(), e->oldPos().y());
  }

  void resizeEvent( QResizeEvent *e )
  {
    PARENT::resizeEvent(e);
    callMouseMethod("resizeEvent", 
		    e->oldSize().width(), e->oldSize().height(),
		    e->size().width(), e->size().height());
  }

  void hideEvent( QHideEvent *e )
  {
    PARENT::hideEvent(e);
    callVoidMethod("hideEvent");
  }

  void showEvent( QShowEvent *e )
  {
    PARENT::showEvent(e);
    callVoidMethod("showEvent");
  }

  void paintEvent ( QPaintEvent * e )
  {
    PARENT::paintEvent( e );
    if ( draw )
      {
	// Create a QPainter
	GraphicsPainter painter( this );
	int x, y, w, h;
	e->rect().getRect ( &x, &y, &w, &h );

	// Get the environment.
	JNIEnv *env;
	vm->GetEnv((void **)&env, JNI_VERSION_1_1);

	// create a QtGraphics wrapper for the QPainter
	jclass cls = env->FindClass( "gnu/java/awt/peer/qt/QtComponentGraphics" );
	jmethodID mid = env->GetMethodID(cls, "<init>", "(JLgnu/java/awt/peer/qt/QtComponentPeer;IIII)V");
	jobject graphics = env->NewObject(cls, mid, (jlong)&painter, target,
					  (jint)x, (jint)y, (jint)w, (jint)h);

	// call QtComponentPeer.paintEvent()
	jmethodID paintEventID = env->GetMethodID( componentCls,
						   "paint",
						   "(Ljava/awt/Graphics;)V" );
	env->CallVoidMethod( target, paintEventID, graphics );
	env->DeleteLocalRef( cls );
	env->DeleteLocalRef( graphics );
	painter.end();
      }
  }

#endif
