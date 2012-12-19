/* ClassRmicCompiler.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004, 2005, 2012
   Free Software Foundation, Inc.

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

package gnu.classpath.tools.rmic;

import gnu.java.rmi.server.RMIHashes;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.rmi.MarshalException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.UnexpectedException;
import java.rmi.UnmarshalException;
import java.rmi.server.Operation;
import java.rmi.server.RemoteCall;
import java.rmi.server.RemoteObject;
import java.rmi.server.RemoteRef;
import java.rmi.server.RemoteStub;
import java.rmi.server.Skeleton;
import java.rmi.server.SkeletonMismatchException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Label;
import org.objectweb.asm.Type;

public class ClassRmicCompiler
  implements RmicBackend
{
  private String[] args;
  private int next;
  private List<Exception> errors = new ArrayList<Exception>();
  private boolean need11Stubs = true;
  private boolean need12Stubs = true;
  private boolean verbose;
  private boolean noWrite;
  private String destination;
  private ClassLoader loader;
  private int errorCount = 0;

  private Class<?> clazz;
  private String classname;
  private String classInternalName;
  private String fullclassname;
  private MethodRef[] remotemethods;
  private String stubname;
  private String skelname;
  private List<Class<?>> mRemoteInterfaces;

  /**
   * @return true if run was successful
   */
  public boolean run(String[] inputFiles)
  {
    args = inputFiles;

    if (next >= args.length)
      return false;

    for (int i = next; i < args.length; i++)
      {
        try
          {
            if (verbose)
              System.out.println("[Processing class " + args[i] + ".class]");
            processClass(args[i].replace(File.separatorChar, '.'));
          }
        catch (IOException e)
          {
            errors.add(e);
          }
        catch (RMICException e)
          {
            errors.add(e);
          }
      }
    if (errors.size() > 0)
      {
        for (Iterator<Exception> it = errors.iterator(); it.hasNext(); )
          {
            Exception ex = it.next();
            logError(ex);
          }
      }

    return errorCount == 0;
  }

  private void processClass(String cls) throws IOException, RMICException
  {
    // reset class specific vars
    clazz = null;
    classname = null;
    classInternalName = null;
    fullclassname = null;
    remotemethods = null;
    stubname = null;
    skelname = null;
    mRemoteInterfaces = new ArrayList<Class<?>>();

    analyzeClass(cls);
    generateStub();
    if (need11Stubs)
      generateSkel();
  }

  private void analyzeClass(String cname)
    throws RMICException
  {
    if (verbose)
      System.out.println("[analyze class " + cname + "]");
    int p = cname.lastIndexOf('.');
    if (p != -1)
      classname = cname.substring(p + 1);
    else
      classname = cname;
    fullclassname = cname;

    findClass();
    findRemoteMethods();
  }

  /**
   * @deprecated
   */
  public Exception getException()
  {
    return errors.size() == 0 ? null : errors.get(0);
  }

  private void findClass()
    throws RMICException
  {
    ClassLoader cl = (loader == null
                      ? ClassLoader.getSystemClassLoader()
                      : loader);
    try
      {
        clazz = Class.forName(fullclassname, false, cl);
      }
    catch (ClassNotFoundException cnfe)
      {
        throw new RMICException
          ("Class " + fullclassname + " not found in classpath", cnfe);
      }

    if (! Remote.class.isAssignableFrom(clazz))
      {
        throw new RMICException
          ("Class " + clazz.getName()
           + " does not implement a remote interface.");
      }
  }

  private static Type[] typeArray(Class<?>[] cls)
  {
    Type[] t = new Type[cls.length];
    for (int i = 0; i < cls.length; i++)
      {
        t[i] = Type.getType(cls[i]);
      }

    return t;
  }

  private static String[] internalNameArray(Type[] t)
  {
    String[] s = new String[t.length];
    for (int i = 0; i < t.length; i++)
      {
        s[i] = t[i].getInternalName();
      }

    return s;
  }

  private static String[] internalNameArray(Class[] c)
  {
    return internalNameArray(typeArray(c));
  }

  private static final String forName = "class$";

  private static List<Object> param(Method m, int argIndex)
  {
    List<Object> l = new ArrayList<Object>();
    l.add(m);
    l.add(Integer.valueOf(argIndex));
    return l;
  }

  private static void generateClassForNamer(ClassVisitor cls)
  {
    MethodVisitor cv =
      cls.visitMethod
      (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC, forName,
       Type.getMethodDescriptor
       (Type.getType(Class.class), new Type[] { Type.getType(String.class) }),
       null, null);

    Label start = new Label();
    cv.visitLabel(start);
    cv.visitVarInsn(Opcodes.ALOAD, 0);
    cv.visitMethodInsn
      (Opcodes.INVOKESTATIC,
       Type.getInternalName(Class.class),
       "forName",
       Type.getMethodDescriptor
       (Type.getType(Class.class), new Type[] { Type.getType(String.class) }));
    cv.visitInsn(Opcodes.ARETURN);

    Label handler = new Label();
    cv.visitLabel(handler);
    cv.visitVarInsn(Opcodes.ASTORE, 1);
    cv.visitTypeInsn(Opcodes.NEW, typeArg(NoClassDefFoundError.class));
    cv.visitInsn(Opcodes.DUP);
    cv.visitVarInsn(Opcodes.ALOAD, 1);
    cv.visitMethodInsn
      (Opcodes.INVOKEVIRTUAL,
       Type.getInternalName(ClassNotFoundException.class),
       "getMessage",
       Type.getMethodDescriptor(Type.getType(String.class), new Type[] {}));
    cv.visitMethodInsn
      (Opcodes.INVOKESPECIAL,
       Type.getInternalName(NoClassDefFoundError.class),
       "<init>",
       Type.getMethodDescriptor
       (Type.VOID_TYPE, new Type[] { Type.getType(String.class) }));
    cv.visitInsn(Opcodes.ATHROW);
    cv.visitTryCatchBlock
      (start, handler, handler,
       Type.getInternalName(ClassNotFoundException.class));
    cv.visitMaxs(-1, -1);
  }

  private void generateClassConstant(MethodVisitor cv, Class<?> cls) {
    if (cls.isPrimitive())
      {
        Class<?> boxCls;
        if (cls.equals(Boolean.TYPE))
          boxCls = Boolean.class;
        else if (cls.equals(Character.TYPE))
          boxCls = Character.class;
        else if (cls.equals(Byte.TYPE))
          boxCls = Byte.class;
        else if (cls.equals(Short.TYPE))
          boxCls = Short.class;
        else if (cls.equals(Integer.TYPE))
          boxCls = Integer.class;
        else if (cls.equals(Long.TYPE))
          boxCls = Long.class;
        else if (cls.equals(Float.TYPE))
          boxCls = Float.class;
        else if (cls.equals(Double.TYPE))
          boxCls = Double.class;
        else if (cls.equals(Void.TYPE))
          boxCls = Void.class;
        else
          throw new IllegalArgumentException("unknown primitive type " + cls);

        cv.visitFieldInsn
          (Opcodes.GETSTATIC, Type.getInternalName(boxCls), "TYPE",
           Type.getDescriptor(Class.class));
        return;
      }
    cv.visitLdcInsn(cls.getName());
    cv.visitMethodInsn
      (Opcodes.INVOKESTATIC, classInternalName, forName,
       Type.getMethodDescriptor
       (Type.getType(Class.class),
        new Type[] { Type.getType(String.class) }));
  }

  private void generateClassArray(MethodVisitor code, Class<?>[] classes)
  {
    code.visitLdcInsn(new Integer(classes.length));
    code.visitTypeInsn(Opcodes.ANEWARRAY, typeArg(Class.class));
    for (int i = 0; i < classes.length; i++)
      {
        code.visitInsn(Opcodes.DUP);
        code.visitLdcInsn(new Integer(i));
        generateClassConstant(code, classes[i]);
        code.visitInsn(Opcodes.AASTORE);
      }
  }

  private void fillOperationArray(MethodVisitor clinit)
  {
    // Operations array
    clinit.visitLdcInsn(new Integer(remotemethods.length));
    clinit.visitTypeInsn(Opcodes.ANEWARRAY, typeArg(Operation.class));
    clinit.visitFieldInsn
      (Opcodes.PUTSTATIC, classInternalName, "operations",
       Type.getDescriptor(Operation[].class));

    for (int i = 0; i < remotemethods.length; i++)
      {
        Method m = remotemethods[i].meth;

        StringBuilder desc = new StringBuilder();
        desc.append(getPrettyName(m.getReturnType()) + " ");
        desc.append(m.getName() + "(");

        // signature
        Class<?>[] sig = m.getParameterTypes();
        for (int j = 0; j < sig.length; j++)
          {
            desc.append(getPrettyName(sig[j]));
            if (j + 1 < sig.length)
                desc.append(", ");
          }

        // push operations array
        clinit.visitFieldInsn
          (Opcodes.GETSTATIC, classInternalName, "operations",
           Type.getDescriptor(Operation[].class));

        // push array index
        clinit.visitLdcInsn(new Integer(i));

        // instantiate operation and leave a copy on the stack
        clinit.visitTypeInsn(Opcodes.NEW, typeArg(Operation.class));
        clinit.visitInsn(Opcodes.DUP);
        clinit.visitLdcInsn(desc.toString());
        clinit.visitMethodInsn
          (Opcodes.INVOKESPECIAL,
           Type.getInternalName(Operation.class),
           "<init>",
           Type.getMethodDescriptor
           (Type.VOID_TYPE, new Type[] { Type.getType(String.class) }));

        // store in operations array
        clinit.visitInsn(Opcodes.AASTORE);
      }
  }

  private void generateStaticMethodObjs(MethodVisitor clinit)
  {
    for (int i = 0; i < remotemethods.length; i++)
      {
        Method m = remotemethods[i].meth;

        /*
         * $method_<i>m.getName()</i>_<i>i</i> =
         *   <i>m.getDeclaringClass()</i>.class.getMethod
         *     (m.getName(), m.getParameterType())
         */
        String methodVar = "$method_" + m.getName() + "_" + i;
        generateClassConstant(clinit, m.getDeclaringClass());
        clinit.visitLdcInsn(m.getName());
        generateClassArray(clinit, m.getParameterTypes());
        clinit.visitMethodInsn
          (Opcodes.INVOKEVIRTUAL,
           Type.getInternalName(Class.class),
           "getMethod",
           Type.getMethodDescriptor
           (Type.getType(Method.class),
            new Type[] { Type.getType(String.class),
                         Type.getType(Class[].class) }));

        clinit.visitFieldInsn
          (Opcodes.PUTSTATIC, classInternalName, methodVar,
           Type.getDescriptor(Method.class));
      }
  }

  private void generateStub()
    throws IOException
  {
    stubname = fullclassname + "_Stub";
    File file = new File((destination == null ? "." : destination)
                         + File.separator
                         + stubname.replace('.', File.separatorChar)
                         + ".class");

    if (verbose)
      System.out.println("[Generating class " + stubname + "]");

    final ClassWriter stub = new ClassWriter(true);
    classInternalName = stubname.replace('.', '/');
    final String superInternalName =
      Type.getType(RemoteStub.class).getInternalName();

    String[] remoteInternalNames =
      internalNameArray(mRemoteInterfaces.toArray(new Class[mRemoteInterfaces.size()]));
    stub.visit
      (Opcodes.V1_2, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, classInternalName,
       null, superInternalName, remoteInternalNames);

    if (need12Stubs)
      {
        stub.visitField
          (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL, "serialVersionUID",
           Type.LONG_TYPE.getDescriptor(), null, new Long(2L));
      }

    if (need11Stubs)
      {
        stub.visitField
          (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
           "interfaceHash", Type.LONG_TYPE.getDescriptor(), null,
           new Long(RMIHashes.getInterfaceHash(clazz)));

        if (need12Stubs)
          {
            stub.visitField
              (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC, "useNewInvoke",
               Type.BOOLEAN_TYPE.getDescriptor(), null, null);
          }

        stub.visitField
          (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL,
           "operations", Type.getDescriptor(Operation[].class), null, null);
      }

    // Set of method references.
    if (need12Stubs)
      {
        for (int i = 0; i < remotemethods.length; i++)
          {
            Method m = remotemethods[i].meth;
            String slotName = "$method_" + m.getName() + "_" + i;
            stub.visitField
              (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC, slotName,
               Type.getDescriptor(Method.class), null, null);
          }
      }

    MethodVisitor clinit = stub.visitMethod
      (Opcodes.ACC_STATIC, "<clinit>",
       Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {}), null, null);

    if (need11Stubs)
      {
        fillOperationArray(clinit);
        if (! need12Stubs)
          clinit.visitInsn(Opcodes.RETURN);
      }

    if (need12Stubs)
      {
        // begin of try
        Label begin = new Label();

        // beginning of catch
        Label handler = new Label();
        clinit.visitLabel(begin);

        // Initialize the methods references.
        if (need11Stubs)
          {
            /*
             * RemoteRef.class.getMethod("invoke", new Class[] {
             *   Remote.class, Method.class, Object[].class, long.class })
             */
            generateClassConstant(clinit, RemoteRef.class);
            clinit.visitLdcInsn("invoke");
            generateClassArray
              (clinit, new Class[] { Remote.class, Method.class,
                                     Object[].class, long.class });
            clinit.visitMethodInsn
              (Opcodes.INVOKEVIRTUAL,
               Type.getInternalName(Class.class),
               "getMethod",
               Type.getMethodDescriptor
               (Type.getType(Method.class),
                new Type[] { Type.getType(String.class),
                             Type.getType(Class[].class) }));

            // useNewInvoke = true
            clinit.visitInsn(Opcodes.ICONST_1);
            clinit.visitFieldInsn
              (Opcodes.PUTSTATIC, classInternalName, "useNewInvoke",
               Type.BOOLEAN_TYPE.getDescriptor());
          }

        generateStaticMethodObjs(clinit);

        // jump past handler
        clinit.visitInsn(Opcodes.RETURN);
        clinit.visitLabel(handler);
        if (need11Stubs)
          {
            // useNewInvoke = false
            clinit.visitInsn(Opcodes.ICONST_0);
            clinit.visitFieldInsn
              (Opcodes.PUTSTATIC, classInternalName, "useNewInvoke",
               Type.BOOLEAN_TYPE.getDescriptor());
            clinit.visitInsn(Opcodes.RETURN);
          }
        else
          {
            // throw NoSuchMethodError
            clinit.visitTypeInsn(Opcodes.NEW, typeArg(NoSuchMethodError.class));
            clinit.visitInsn(Opcodes.DUP);
            clinit.visitLdcInsn("stub class initialization failed");
            clinit.visitMethodInsn
              (Opcodes.INVOKESPECIAL,
               Type.getInternalName(NoSuchMethodError.class),
               "<init>",
               Type.getMethodDescriptor
               (Type.VOID_TYPE,
                new Type[] { Type.getType(String.class) }));
            clinit.visitInsn(Opcodes.ATHROW);
          }

        clinit.visitTryCatchBlock
          (begin, handler, handler,
           Type.getInternalName(NoSuchMethodException.class));

      }

    clinit.visitMaxs(-1, -1);

    generateClassForNamer(stub);

    // Constructors
    if (need11Stubs)
      {
        // no arg public constructor
        MethodVisitor code = stub.visitMethod
          (Opcodes.ACC_PUBLIC, "<init>",
           Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {}),
           null, null);
        code.visitVarInsn(Opcodes.ALOAD, 0);
        code.visitMethodInsn
          (Opcodes.INVOKESPECIAL, superInternalName, "<init>",
           Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {}));
        code.visitInsn(Opcodes.RETURN);

        code.visitMaxs(-1, -1);
      }

    // public RemoteRef constructor
    MethodVisitor constructor = stub.visitMethod
      (Opcodes.ACC_PUBLIC, "<init>",
       Type.getMethodDescriptor
       (Type.VOID_TYPE, new Type[] {Type.getType(RemoteRef.class)}),
       null, null);
    constructor.visitVarInsn(Opcodes.ALOAD, 0);
    constructor.visitVarInsn(Opcodes.ALOAD, 1);
    constructor.visitMethodInsn
      (Opcodes.INVOKESPECIAL, superInternalName, "<init>",
       Type.getMethodDescriptor
       (Type.VOID_TYPE, new Type[] {Type.getType(RemoteRef.class)}));
    constructor.visitInsn(Opcodes.RETURN);
    constructor.visitMaxs(-1, -1);

    // Method implementations
    for (int i = 0; i < remotemethods.length; i++)
      {
        Method m = remotemethods[i].meth;
        Class<?>[] sig = m.getParameterTypes();
        Class<?> returntype = m.getReturnType();
        Class<?>[] except = sortExceptions
          (remotemethods[i].exceptions.toArray(new Class<?>[remotemethods[i].exceptions.size()]));

        MethodVisitor code = stub.visitMethod
          (Opcodes.ACC_PUBLIC,
           m.getName(),
           Type.getMethodDescriptor(Type.getType(returntype), typeArray(sig)),
           null,
           internalNameArray(typeArray(except)));

        final Variables var = new Variables();

        // this and parameters are the declared vars
        var.declare("this");
        for (int j = 0; j < sig.length; j++)
          var.declare(param(m, j), size(sig[j]));

        Label methodTryBegin = new Label();
        code.visitLabel(methodTryBegin);

        if (need12Stubs)
          {
            Label oldInvoke = new Label();
            if (need11Stubs)
              {
                // if not useNewInvoke jump to old invoke
                code.visitFieldInsn
                  (Opcodes.GETSTATIC, classInternalName, "useNewInvoke",
                   Type.getDescriptor(boolean.class));
                code.visitJumpInsn(Opcodes.IFEQ, oldInvoke);
              }

            // this.ref
            code.visitVarInsn(Opcodes.ALOAD, var.get("this"));
            code.visitFieldInsn
              (Opcodes.GETFIELD, Type.getInternalName(RemoteObject.class),
               "ref", Type.getDescriptor(RemoteRef.class));

            // "this" is first arg to invoke
            code.visitVarInsn(Opcodes.ALOAD, var.get("this"));

            // method object is second arg to invoke
            String methName = "$method_" + m.getName() + "_" + i;
            code.visitFieldInsn
              (Opcodes.GETSTATIC, classInternalName, methName,
               Type.getDescriptor(Method.class));

            // args to remote method are third arg to invoke
            if (sig.length == 0)
              code.visitInsn(Opcodes.ACONST_NULL);
            else
              {
                // create arg Object[] (with boxed primitives) and push it
                code.visitLdcInsn(new Integer(sig.length));
                code.visitTypeInsn(Opcodes.ANEWARRAY, typeArg(Object.class));

                var.allocate("argArray");
                code.visitVarInsn(Opcodes.ASTORE, var.get("argArray"));

                for (int j = 0; j < sig.length; j++)
                  {
                    int insn = loadOpcode(sig[j]);
                    Class<?> box = sig[j].isPrimitive() ? box(sig[j]) : null;

                    code.visitVarInsn(Opcodes.ALOAD, var.get("argArray"));
                    code.visitLdcInsn(new Integer(j));

                    // put argument on stack
                    if (box != null)
                      {
                        code.visitTypeInsn(Opcodes.NEW, typeArg(box));
                        code.visitInsn(Opcodes.DUP);
                        code.visitVarInsn(insn, var.get(param(m, j)));
                        code.visitMethodInsn
                          (Opcodes.INVOKESPECIAL,
                           Type.getInternalName(box),
                           "<init>",
                           Type.getMethodDescriptor
                           (Type.VOID_TYPE,
                            new Type[] { Type.getType(sig[j]) }));
                      }
                    else
                      code.visitVarInsn(insn, var.get(param(m, j)));

                    code.visitInsn(Opcodes.AASTORE);
                  }

                code.visitVarInsn(Opcodes.ALOAD, var.deallocate("argArray"));
              }

            // push remote operation opcode
            code.visitLdcInsn(Long.valueOf(remotemethods[i].hash));
            code.visitMethodInsn
              (Opcodes.INVOKEINTERFACE,
               Type.getInternalName(RemoteRef.class),
               "invoke",
               Type.getMethodDescriptor
               (Type.getType(Object.class),
                new Type[] { Type.getType(Remote.class),
                             Type.getType(Method.class),
                             Type.getType(Object[].class),
                             Type.LONG_TYPE }));

            if (! returntype.equals(Void.TYPE))
              {
                int retcode = returnOpcode(returntype);
                Class<?> boxCls =
                  returntype.isPrimitive() ? box(returntype) : null;
                code.visitTypeInsn
                  (Opcodes.CHECKCAST, typeArg(boxCls == null ? returntype : boxCls));
                if (returntype.isPrimitive())
                  {
                    // unbox
                    code.visitMethodInsn
                      (Opcodes.INVOKEVIRTUAL,
                       Type.getType(boxCls).getInternalName(),
                       unboxMethod(returntype),
                       Type.getMethodDescriptor
                       (Type.getType(returntype), new Type[] {}));
                  }

                code.visitInsn(retcode);
              }
            else
              code.visitInsn(Opcodes.RETURN);


            if (need11Stubs)
              code.visitLabel(oldInvoke);
          }

        if (need11Stubs)
          {

            // this.ref.newCall(this, operations, index, interfaceHash)
            code.visitVarInsn(Opcodes.ALOAD, var.get("this"));
            code.visitFieldInsn
              (Opcodes.GETFIELD,
               Type.getInternalName(RemoteObject.class),
               "ref",
               Type.getDescriptor(RemoteRef.class));

            // "this" is first arg to newCall
            code.visitVarInsn(Opcodes.ALOAD, var.get("this"));

            // operations is second arg to newCall
            code.visitFieldInsn
              (Opcodes.GETSTATIC, classInternalName, "operations",
               Type.getDescriptor(Operation[].class));

            // method index is third arg
            code.visitLdcInsn(new Integer(i));

            // interface hash is fourth arg
            code.visitFieldInsn
              (Opcodes.GETSTATIC, classInternalName, "interfaceHash",
               Type.LONG_TYPE.getDescriptor());

            code.visitMethodInsn
              (Opcodes.INVOKEINTERFACE,
               Type.getInternalName(RemoteRef.class),
               "newCall",
               Type.getMethodDescriptor
               (Type.getType(RemoteCall.class),
                new Type[] { Type.getType(RemoteObject.class),
                             Type.getType(Operation[].class),
                             Type.INT_TYPE,
                             Type.LONG_TYPE }));

            // store call object on stack and leave copy on stack
            var.allocate("call");
            code.visitInsn(Opcodes.DUP);
            code.visitVarInsn(Opcodes.ASTORE, var.get("call"));

            Label beginArgumentTryBlock = new Label();
            code.visitLabel(beginArgumentTryBlock);

            // ObjectOutput out = call.getOutputStream();
            code.visitMethodInsn
              (Opcodes.INVOKEINTERFACE,
               Type.getInternalName(RemoteCall.class),
               "getOutputStream",
               Type.getMethodDescriptor
               (Type.getType(ObjectOutput.class), new Type[] {}));

            for (int j = 0; j < sig.length; j++)
              {
                // dup the ObjectOutput
                code.visitInsn(Opcodes.DUP);

                // get j'th arg to remote method
                code.visitVarInsn(loadOpcode(sig[j]), var.get(param(m, j)));

                Class<?> argCls =
                  sig[j].isPrimitive() ? sig[j] : Object.class;

                // out.writeFoo
                code.visitMethodInsn
                  (Opcodes.INVOKEINTERFACE,
                   Type.getInternalName(ObjectOutput.class),
                   writeMethod(sig[j]),
                   Type.getMethodDescriptor
                   (Type.VOID_TYPE,
                    new Type[] { Type.getType(argCls) }));
              }

            // pop ObjectOutput
            code.visitInsn(Opcodes.POP);

            Label iohandler = new Label();
            Label endArgumentTryBlock = new Label();
            code.visitJumpInsn(Opcodes.GOTO, endArgumentTryBlock);
            code.visitLabel(iohandler);

            // throw new MarshalException(msg, ioexception);
            code.visitVarInsn(Opcodes.ASTORE, var.allocate("exception"));
            code.visitTypeInsn(Opcodes.NEW, typeArg(MarshalException.class));
            code.visitInsn(Opcodes.DUP);
            code.visitLdcInsn("error marshalling arguments");
            code.visitVarInsn(Opcodes.ALOAD, var.deallocate("exception"));
            code.visitMethodInsn
              (Opcodes.INVOKESPECIAL,
               Type.getInternalName(MarshalException.class),
               "<init>",
               Type.getMethodDescriptor
               (Type.VOID_TYPE,
                new Type[] { Type.getType(String.class),
                             Type.getType(Exception.class) }));
            code.visitInsn(Opcodes.ATHROW);

            code.visitLabel(endArgumentTryBlock);
            code.visitTryCatchBlock
              (beginArgumentTryBlock, iohandler, iohandler,
               Type.getInternalName(IOException.class));

            // this.ref.invoke(call)
            code.visitVarInsn(Opcodes.ALOAD, var.get("this"));
            code.visitFieldInsn
              (Opcodes.GETFIELD, Type.getInternalName(RemoteObject.class),
               "ref", Type.getDescriptor(RemoteRef.class));
            code.visitVarInsn(Opcodes.ALOAD, var.get("call"));
            code.visitMethodInsn
              (Opcodes.INVOKEINTERFACE,
               Type.getInternalName(RemoteRef.class),
               "invoke",
               Type.getMethodDescriptor
               (Type.VOID_TYPE,
                new Type[] { Type.getType(RemoteCall.class) }));

            // handle return value
            boolean needcastcheck = false;

            Label beginReturnTryCatch = new Label();
            code.visitLabel(beginReturnTryCatch);

            int returncode = returnOpcode(returntype);

            if (! returntype.equals(Void.TYPE))
              {
                // call.getInputStream()
                code.visitVarInsn(Opcodes.ALOAD, var.get("call"));
                code.visitMethodInsn
                  (Opcodes.INVOKEINTERFACE,
                   Type.getInternalName(RemoteCall.class),
                   "getInputStream",
                   Type.getMethodDescriptor
                   (Type.getType(ObjectInput.class), new Type[] {}));

                Class<?> readCls =
                  returntype.isPrimitive() ? returntype : Object.class;
                code.visitMethodInsn
                  (Opcodes.INVOKEINTERFACE,
                   Type.getInternalName(ObjectInput.class),
                   readMethod(returntype),
                   Type.getMethodDescriptor
                   (Type.getType(readCls), new Type[] {}));

                boolean castresult = false;

                if (! returntype.isPrimitive())
                  {
                    if (! returntype.equals(Object.class))
                      castresult = true;
                    else
                      needcastcheck = true;
                  }

                if (castresult)
                  code.visitTypeInsn(Opcodes.CHECKCAST, typeArg(returntype));

                // leave result on stack for return
              }

            // this.ref.done(call)
            code.visitVarInsn(Opcodes.ALOAD, var.get("this"));
            code.visitFieldInsn
              (Opcodes.GETFIELD,
               Type.getInternalName(RemoteObject.class),
               "ref",
               Type.getDescriptor(RemoteRef.class));
            code.visitVarInsn(Opcodes.ALOAD, var.deallocate("call"));
            code.visitMethodInsn
              (Opcodes.INVOKEINTERFACE,
               Type.getInternalName(RemoteRef.class),
               "done",
               Type.getMethodDescriptor
               (Type.VOID_TYPE,
                new Type[] { Type.getType(RemoteCall.class) }));

            // return; or return result;
            code.visitInsn(returncode);

            // exception handler
            Label handler = new Label();
            code.visitLabel(handler);
            code.visitVarInsn(Opcodes.ASTORE, var.allocate("exception"));

            // throw new UnmarshalException(msg, e)
            code.visitTypeInsn(Opcodes.NEW, typeArg(UnmarshalException.class));
            code.visitInsn(Opcodes.DUP);
            code.visitLdcInsn("error unmarshalling return");
            code.visitVarInsn(Opcodes.ALOAD, var.deallocate("exception"));
            code.visitMethodInsn
              (Opcodes.INVOKESPECIAL,
               Type.getInternalName(UnmarshalException.class),
               "<init>",
               Type.getMethodDescriptor
               (Type.VOID_TYPE,
                new Type[] { Type.getType(String.class),
                             Type.getType(Exception.class) }));
            code.visitInsn(Opcodes.ATHROW);

            // catch IOException
            code.visitTryCatchBlock
              (beginReturnTryCatch, handler, handler,
               Type.getInternalName(IOException.class));

            if (needcastcheck)
              {
                // catch ClassNotFoundException
                code.visitTryCatchBlock
                  (beginReturnTryCatch, handler, handler,
                   Type.getInternalName(ClassNotFoundException.class));
              }
          }

        Label rethrowHandler = new Label();
        code.visitLabel(rethrowHandler);
        // rethrow declared exceptions
        code.visitInsn(Opcodes.ATHROW);

        boolean needgeneral = true;
        for (int j = 0; j < except.length; j++)
          {
            if (except[j] == Exception.class)
              needgeneral = false;
          }

        for (int j = 0; j < except.length; j++)
          {
            code.visitTryCatchBlock
              (methodTryBegin, rethrowHandler, rethrowHandler,
               Type.getInternalName(except[j]));
          }

        if (needgeneral)
          {
            // rethrow unchecked exceptions
            code.visitTryCatchBlock
              (methodTryBegin, rethrowHandler, rethrowHandler,
               Type.getInternalName(RuntimeException.class));

            Label generalHandler = new Label();
            code.visitLabel(generalHandler);
            String msg = "undeclared checked exception";

            // throw new java.rmi.UnexpectedException(msg, e)
            code.visitVarInsn(Opcodes.ASTORE, var.allocate("exception"));
            code.visitTypeInsn(Opcodes.NEW, typeArg(UnexpectedException.class));
            code.visitInsn(Opcodes.DUP);
            code.visitLdcInsn(msg);
            code.visitVarInsn(Opcodes.ALOAD, var.deallocate("exception"));
            code.visitMethodInsn
              (Opcodes.INVOKESPECIAL,
               Type.getInternalName(UnexpectedException.class),
               "<init>",
               Type.getMethodDescriptor
               (Type.VOID_TYPE,
                new Type [] { Type.getType(String.class),
                              Type.getType(Exception.class) }));
            code.visitInsn(Opcodes.ATHROW);

            code.visitTryCatchBlock
              (methodTryBegin, rethrowHandler, generalHandler,
               Type.getInternalName(Exception.class));
          }

        code.visitMaxs(-1, -1);
      }

    stub.visitEnd();
    byte[] classData = stub.toByteArray();
    if (!noWrite)
      {
        if (file.exists())
          file.delete();
        if (file.getParentFile() != null)
          file.getParentFile().mkdirs();
        FileOutputStream fos = new FileOutputStream(file);
        fos.write(classData);
        fos.flush();
        fos.close();
      }
  }

  private void generateSkel() throws IOException
  {
    skelname = fullclassname + "_Skel";
    File file = new File(destination == null ? "" : destination
                         + File.separator
                         + skelname.replace('.', File.separatorChar)
                         + ".class");
    if (verbose)
      System.out.println("[Generating class " + skelname + "]");

    final ClassWriter skel = new ClassWriter(true);
    classInternalName = skelname.replace('.', '/');
    skel.visit
      (Opcodes.V1_1, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL,
       classInternalName, Type.getInternalName(Object.class), null,
       new String[] { Type.getType(Skeleton.class).getInternalName() });

    skel.visitField
      (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL, "interfaceHash",
       Type.LONG_TYPE.getDescriptor(), null,
       new Long(RMIHashes.getInterfaceHash(clazz)));

    skel.visitField
      (Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC + Opcodes.ACC_FINAL, "operations",
       Type.getDescriptor(Operation[].class), null, null);

    MethodVisitor clinit = skel.visitMethod
      (Opcodes.ACC_STATIC, "<clinit>",
       Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {}), null, null);

    fillOperationArray(clinit);
    clinit.visitInsn(Opcodes.RETURN);

    clinit.visitMaxs(-1, -1);

    // no arg public constructor
    MethodVisitor init = skel.visitMethod
      (Opcodes.ACC_PUBLIC, "<init>",
       Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {}), null, null);
    init.visitVarInsn(Opcodes.ALOAD, 0);
    init.visitMethodInsn
      (Opcodes.INVOKESPECIAL, Type.getInternalName(Object.class), "<init>",
       Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {}));
    init.visitInsn(Opcodes.RETURN);
    init.visitMaxs(-1, -1);

    /*
     * public Operation[] getOperations()
     * returns a clone of the operations array
     */
    MethodVisitor getOp = skel.visitMethod
      (Opcodes.ACC_PUBLIC, "getOperations",
       Type.getMethodDescriptor
       (Type.getType(Operation[].class), new Type[] {}),
       null, null);
    getOp.visitFieldInsn
      (Opcodes.GETSTATIC, classInternalName, "operations",
       Type.getDescriptor(Operation[].class));
    getOp.visitMethodInsn
      (Opcodes.INVOKEVIRTUAL, Type.getInternalName(Object.class),
       "clone", Type.getMethodDescriptor(Type.getType(Object.class),
                                         new Type[] {}));
    getOp.visitTypeInsn(Opcodes.CHECKCAST, typeArg(Operation[].class));
    getOp.visitInsn(Opcodes.ARETURN);
    getOp.visitMaxs(-1, -1);

    // public void dispatch(Remote, RemoteCall, int opnum, long hash)
    MethodVisitor dispatch = skel.visitMethod
      (Opcodes.ACC_PUBLIC,
       "dispatch",
       Type.getMethodDescriptor
       (Type.VOID_TYPE,
        new Type[] { Type.getType(Remote.class),
                     Type.getType(RemoteCall.class),
                     Type.INT_TYPE, Type.LONG_TYPE }), null,
       new String[] { Type.getInternalName(Exception.class) });

    Variables var = new Variables();
    var.declare("this");
    var.declare("remoteobj");
    var.declare("remotecall");
    var.declare("opnum");
    var.declareWide("hash");

    /*
     * if opnum >= 0
     * XXX it is unclear why there is handling of negative opnums
     */
    dispatch.visitVarInsn(Opcodes.ILOAD, var.get("opnum"));
    Label nonNegativeOpnum = new Label();
    Label opnumSet = new Label();
    dispatch.visitJumpInsn(Opcodes.IFGE, nonNegativeOpnum);

    for (int i = 0; i < remotemethods.length; i++)
      {
        // assign opnum if hash matches supplied hash
        dispatch.visitVarInsn(Opcodes.LLOAD, var.get("hash"));
        dispatch.visitLdcInsn(Long.valueOf(remotemethods[i].hash));
        Label notIt = new Label();
        dispatch.visitInsn(Opcodes.LCMP);
        dispatch.visitJumpInsn(Opcodes.IFNE, notIt);

        // opnum = <opnum>
        dispatch.visitLdcInsn(new Integer(i));
        dispatch.visitVarInsn(Opcodes.ISTORE, var.get("opnum"));
        dispatch.visitJumpInsn(Opcodes.GOTO, opnumSet);
        dispatch.visitLabel(notIt);
      }

    // throw new SkeletonMismatchException
    Label mismatch = new Label();
    dispatch.visitJumpInsn(Opcodes.GOTO, mismatch);

    dispatch.visitLabel(nonNegativeOpnum);

    // if opnum is already set, check that the hash matches the interface
    dispatch.visitVarInsn(Opcodes.LLOAD, var.get("hash"));
    dispatch.visitFieldInsn
      (Opcodes.GETSTATIC, classInternalName,
       "interfaceHash", Type.LONG_TYPE.getDescriptor());
    dispatch.visitInsn(Opcodes.LCMP);
    dispatch.visitJumpInsn(Opcodes.IFEQ, opnumSet);

    dispatch.visitLabel(mismatch);
    dispatch.visitTypeInsn
      (Opcodes.NEW, typeArg(SkeletonMismatchException.class));
    dispatch.visitInsn(Opcodes.DUP);
    dispatch.visitLdcInsn("interface hash mismatch");
    dispatch.visitMethodInsn
      (Opcodes.INVOKESPECIAL,
       Type.getInternalName(SkeletonMismatchException.class),
       "<init>",
       Type.getMethodDescriptor
       (Type.VOID_TYPE, new Type[] { Type.getType(String.class) }));
    dispatch.visitInsn(Opcodes.ATHROW);

    // opnum has been set
    dispatch.visitLabel(opnumSet);

    dispatch.visitVarInsn(Opcodes.ALOAD, var.get("remoteobj"));
    dispatch.visitTypeInsn(Opcodes.CHECKCAST, typeArg(clazz));
    dispatch.visitVarInsn(Opcodes.ASTORE, var.get("remoteobj"));

    Label deflt = new Label();
    Label[] methLabels = new Label[remotemethods.length];
    for (int i = 0; i < methLabels.length; i++)
      methLabels[i] = new Label();

    // switch on opnum
    dispatch.visitVarInsn(Opcodes.ILOAD, var.get("opnum"));
    dispatch.visitTableSwitchInsn
      (0, remotemethods.length - 1, deflt, methLabels);

    // Method dispatch
    for (int i = 0; i < remotemethods.length; i++)
      {
        dispatch.visitLabel(methLabels[i]);
        Method m = remotemethods[i].meth;
        generateMethodSkel(dispatch, m, var);
      }

    dispatch.visitLabel(deflt);
    dispatch.visitTypeInsn(Opcodes.NEW, typeArg(UnmarshalException.class));
    dispatch.visitInsn(Opcodes.DUP);
    dispatch.visitLdcInsn("invalid method number");
    dispatch.visitMethodInsn
      (Opcodes.INVOKESPECIAL,
       Type.getInternalName(UnmarshalException.class),
       "<init>",
       Type.getMethodDescriptor
       (Type.VOID_TYPE, new Type[] { Type.getType(String.class) }));
    dispatch.visitInsn(Opcodes.ATHROW);

    dispatch.visitMaxs(-1, -1);

    skel.visitEnd();
    byte[] classData = skel.toByteArray();
    if (!noWrite)
      {
        if (file.exists())
          file.delete();
        if (file.getParentFile() != null)
          file.getParentFile().mkdirs();
        FileOutputStream fos = new FileOutputStream(file);
        fos.write(classData);
        fos.flush();
        fos.close();
      }
  }

  private void generateMethodSkel(MethodVisitor cv, Method m, Variables var)
  {
    Class<?>[] sig = m.getParameterTypes();

    Label readArgs = new Label();
    cv.visitLabel(readArgs);

    boolean needcastcheck = false;

    // ObjectInput in = call.getInputStream();
    cv.visitVarInsn(Opcodes.ALOAD, var.get("remotecall"));
    cv.visitMethodInsn
      (Opcodes.INVOKEINTERFACE,
       Type.getInternalName(RemoteCall.class), "getInputStream",
       Type.getMethodDescriptor
       (Type.getType(ObjectInput.class), new Type[] {}));
    cv.visitVarInsn(Opcodes.ASTORE, var.allocate("objectinput"));

    for (int i = 0; i < sig.length; i++)
      {
        // dup input stream
        cv.visitVarInsn(Opcodes.ALOAD, var.get("objectinput"));

        Class<?> readCls = sig[i].isPrimitive() ? sig[i] : Object.class;

        // in.readFoo()
        cv.visitMethodInsn
          (Opcodes.INVOKEINTERFACE,
           Type.getInternalName(ObjectInput.class),
           readMethod(sig[i]),
           Type.getMethodDescriptor
           (Type.getType(readCls), new Type [] {}));

        if (! sig[i].isPrimitive() && ! sig[i].equals(Object.class))
          {
            needcastcheck = true;
            cv.visitTypeInsn(Opcodes.CHECKCAST, typeArg(sig[i]));
          }

        // store arg in variable
        cv.visitVarInsn
          (storeOpcode(sig[i]), var.allocate(param(m, i), size(sig[i])));
      }

    var.deallocate("objectinput");

    Label doCall = new Label();
    Label closeInput = new Label();

    cv.visitJumpInsn(Opcodes.JSR, closeInput);
    cv.visitJumpInsn(Opcodes.GOTO, doCall);

    // throw new UnmarshalException
    Label handler = new Label();
    cv.visitLabel(handler);
    cv.visitVarInsn(Opcodes.ASTORE, var.allocate("exception"));
    cv.visitTypeInsn(Opcodes.NEW, typeArg(UnmarshalException.class));
    cv.visitInsn(Opcodes.DUP);
    cv.visitLdcInsn("error unmarshalling arguments");
    cv.visitVarInsn(Opcodes.ALOAD, var.deallocate("exception"));
    cv.visitMethodInsn
      (Opcodes.INVOKESPECIAL,
       Type.getInternalName(UnmarshalException.class),
       "<init>",
       Type.getMethodDescriptor
       (Type.VOID_TYPE, new Type[] { Type.getType(String.class),
                                     Type.getType(Exception.class) }));
    cv.visitVarInsn(Opcodes.ASTORE, var.allocate("toThrow"));
    cv.visitJumpInsn(Opcodes.JSR, closeInput);
    cv.visitVarInsn(Opcodes.ALOAD, var.get("toThrow"));
    cv.visitInsn(Opcodes.ATHROW);

    cv.visitTryCatchBlock
      (readArgs, handler, handler, Type.getInternalName(IOException.class));
    if (needcastcheck)
      {
        cv.visitTryCatchBlock
          (readArgs, handler, handler,
           Type.getInternalName(ClassCastException.class));
      }

    // finally block
    cv.visitLabel(closeInput);
    cv.visitVarInsn(Opcodes.ASTORE, var.allocate("retAddress"));
    cv.visitVarInsn(Opcodes.ALOAD, var.get("remotecall"));
    cv.visitMethodInsn
      (Opcodes.INVOKEINTERFACE,
       Type.getInternalName(RemoteCall.class),
       "releaseInputStream",
       Type.getMethodDescriptor(Type.VOID_TYPE, new Type[] {}));
    cv.visitVarInsn(Opcodes.RET, var.deallocate("retAddress"));
    var.deallocate("toThrow");

    // do the call using args stored as variables
    cv.visitLabel(doCall);
    cv.visitVarInsn(Opcodes.ALOAD, var.get("remoteobj"));
    for (int i = 0; i < sig.length; i++)
      cv.visitVarInsn(loadOpcode(sig[i]), var.deallocate(param(m, i)));
    cv.visitMethodInsn
      (Opcodes.INVOKEVIRTUAL, Type.getInternalName(clazz), m.getName(),
       Type.getMethodDescriptor(m));

    Class<?> returntype = m.getReturnType();
    if (! returntype.equals(Void.TYPE))
      {
        cv.visitVarInsn
          (storeOpcode(returntype), var.allocate("result", size(returntype)));
      }

    // write result to result stream
    Label writeResult = new Label();
    cv.visitLabel(writeResult);
    cv.visitVarInsn(Opcodes.ALOAD, var.get("remotecall"));
    cv.visitInsn(Opcodes.ICONST_1);
    cv.visitMethodInsn
      (Opcodes.INVOKEINTERFACE,
       Type.getInternalName(RemoteCall.class),
       "getResultStream",
       Type.getMethodDescriptor
       (Type.getType(ObjectOutput.class),
        new Type[] { Type.BOOLEAN_TYPE }));

    if (! returntype.equals(Void.TYPE))
      {
        // out.writeFoo(result)
        cv.visitVarInsn(loadOpcode(returntype), var.deallocate("result"));
        Class<?> writeCls = returntype.isPrimitive() ? returntype : Object.class;
        cv.visitMethodInsn
          (Opcodes.INVOKEINTERFACE,
           Type.getInternalName(ObjectOutput.class),
           writeMethod(returntype),
           Type.getMethodDescriptor
           (Type.VOID_TYPE, new Type[] { Type.getType(writeCls) }));
      }

    cv.visitInsn(Opcodes.RETURN);

    // throw new MarshalException
    Label marshalHandler = new Label();
    cv.visitLabel(marshalHandler);
    cv.visitVarInsn(Opcodes.ASTORE, var.allocate("exception"));
    cv.visitTypeInsn(Opcodes.NEW, typeArg(MarshalException.class));
    cv.visitInsn(Opcodes.DUP);
    cv.visitLdcInsn("error marshalling return");
    cv.visitVarInsn(Opcodes.ALOAD, var.deallocate("exception"));
    cv.visitMethodInsn
      (Opcodes.INVOKESPECIAL,
       Type.getInternalName(MarshalException.class),
       "<init>",
       Type.getMethodDescriptor
       (Type.VOID_TYPE, new Type[] { Type.getType(String.class),
                                     Type.getType(Exception.class) }));
    cv.visitInsn(Opcodes.ATHROW);
    cv.visitTryCatchBlock
      (writeResult, marshalHandler, marshalHandler,
       Type.getInternalName(IOException.class));
  }

  private static String typeArg(Class<?> cls)
  {
    if (cls.isArray())
      return Type.getDescriptor(cls);

    return Type.getInternalName(cls);
  }

  private static String readMethod(Class<?> cls)
  {
    if (cls.equals(Void.TYPE))
      throw new IllegalArgumentException("can not read void");

    String method;
    if (cls.equals(Boolean.TYPE))
      method = "readBoolean";
    else if (cls.equals(Byte.TYPE))
      method = "readByte";
    else if (cls.equals(Character.TYPE))
      method = "readChar";
    else if (cls.equals(Short.TYPE))
      method = "readShort";
    else if (cls.equals(Integer.TYPE))
      method = "readInt";
    else if (cls.equals(Long.TYPE))
      method = "readLong";
    else if (cls.equals(Float.TYPE))
      method = "readFloat";
    else if (cls.equals(Double.TYPE))
      method = "readDouble";
    else
      method = "readObject";

    return method;
  }

  private static String writeMethod(Class<?> cls)
  {
    if (cls.equals(Void.TYPE))
      throw new IllegalArgumentException("can not read void");

    String method;
    if (cls.equals(Boolean.TYPE))
      method = "writeBoolean";
    else if (cls.equals(Byte.TYPE))
      method = "writeByte";
    else if (cls.equals(Character.TYPE))
      method = "writeChar";
    else if (cls.equals(Short.TYPE))
      method = "writeShort";
    else if (cls.equals(Integer.TYPE))
      method = "writeInt";
    else if (cls.equals(Long.TYPE))
      method = "writeLong";
    else if (cls.equals(Float.TYPE))
      method = "writeFloat";
    else if (cls.equals(Double.TYPE))
      method = "writeDouble";
    else
      method = "writeObject";

    return method;
  }

  private static int returnOpcode(Class<?> cls)
  {
    int returncode;
    if (cls.equals(Boolean.TYPE))
      returncode = Opcodes.IRETURN;
    else if (cls.equals(Byte.TYPE))
      returncode = Opcodes.IRETURN;
    else if (cls.equals(Character.TYPE))
      returncode = Opcodes.IRETURN;
    else if (cls.equals(Short.TYPE))
      returncode = Opcodes.IRETURN;
    else if (cls.equals(Integer.TYPE))
      returncode = Opcodes.IRETURN;
    else if (cls.equals(Long.TYPE))
      returncode = Opcodes.LRETURN;
    else if (cls.equals(Float.TYPE))
      returncode = Opcodes.FRETURN;
    else if (cls.equals(Double.TYPE))
      returncode = Opcodes.DRETURN;
    else if (cls.equals(Void.TYPE))
      returncode = Opcodes.RETURN;
    else
      returncode = Opcodes.ARETURN;

    return returncode;
  }

  private static int loadOpcode(Class<?> cls)
  {
    if (cls.equals(Void.TYPE))
      throw new IllegalArgumentException("can not load void");

    int loadcode;
    if (cls.equals(Boolean.TYPE))
      loadcode = Opcodes.ILOAD;
    else if (cls.equals(Byte.TYPE))
      loadcode = Opcodes.ILOAD;
    else if (cls.equals(Character.TYPE))
      loadcode = Opcodes.ILOAD;
    else if (cls.equals(Short.TYPE))
      loadcode = Opcodes.ILOAD;
    else if (cls.equals(Integer.TYPE))
      loadcode = Opcodes.ILOAD;
    else if (cls.equals(Long.TYPE))
      loadcode = Opcodes.LLOAD;
    else if (cls.equals(Float.TYPE))
      loadcode = Opcodes.FLOAD;
    else if (cls.equals(Double.TYPE))
      loadcode = Opcodes.DLOAD;
    else
      loadcode = Opcodes.ALOAD;

    return loadcode;
  }

  private static int storeOpcode(Class<?> cls)
  {
    if (cls.equals(Void.TYPE))
      throw new IllegalArgumentException("can not load void");

    int storecode;
    if (cls.equals(Boolean.TYPE))
      storecode = Opcodes.ISTORE;
    else if (cls.equals(Byte.TYPE))
      storecode = Opcodes.ISTORE;
    else if (cls.equals(Character.TYPE))
      storecode = Opcodes.ISTORE;
    else if (cls.equals(Short.TYPE))
      storecode = Opcodes.ISTORE;
    else if (cls.equals(Integer.TYPE))
      storecode = Opcodes.ISTORE;
    else if (cls.equals(Long.TYPE))
      storecode = Opcodes.LSTORE;
    else if (cls.equals(Float.TYPE))
      storecode = Opcodes.FSTORE;
    else if (cls.equals(Double.TYPE))
      storecode = Opcodes.DSTORE;
    else
      storecode = Opcodes.ASTORE;

    return storecode;
  }

  private static String unboxMethod(Class<?> primitive)
  {
    if (! primitive.isPrimitive())
      throw new IllegalArgumentException("can not unbox nonprimitive");

    String method;
    if (primitive.equals(Boolean.TYPE))
      method = "booleanValue";
    else if (primitive.equals(Byte.TYPE))
      method = "byteValue";
    else if (primitive.equals(Character.TYPE))
      method = "charValue";
    else if (primitive.equals(Short.TYPE))
      method = "shortValue";
    else if (primitive.equals(Integer.TYPE))
      method = "intValue";
    else if (primitive.equals(Long.TYPE))
      method = "longValue";
    else if (primitive.equals(Float.TYPE))
      method = "floatValue";
    else if (primitive.equals(Double.TYPE))
      method = "doubleValue";
    else
      throw new IllegalStateException("unknown primitive class " + primitive);

    return method;
  }

  public static Class<?> box(Class<?> cls)
  {
    if (! cls.isPrimitive())
      throw new IllegalArgumentException("can only box primitive");

    Class<?> box;
    if (cls.equals(Boolean.TYPE))
      box = Boolean.class;
    else if (cls.equals(Byte.TYPE))
      box = Byte.class;
    else if (cls.equals(Character.TYPE))
      box = Character.class;
    else if (cls.equals(Short.TYPE))
      box = Short.class;
    else if (cls.equals(Integer.TYPE))
      box = Integer.class;
    else if (cls.equals(Long.TYPE))
      box = Long.class;
    else if (cls.equals(Float.TYPE))
      box = Float.class;
    else if (cls.equals(Double.TYPE))
      box = Double.class;
    else
      throw new IllegalStateException("unknown primitive type " + cls);

    return box;
  }

  private static int size(Class<?> cls) {
    if (cls.equals(Long.TYPE) || cls.equals(Double.TYPE))
      return 2;
    else
      return 1;
  }

  /**
   * Sort exceptions so the most general go last.
   */
  private Class<?>[] sortExceptions(Class<?>[] except)
  {
    for (int i = 0; i < except.length; i++)
      {
        for (int j = i + 1; j < except.length; j++)
          {
            if (except[i].isAssignableFrom(except[j]))
              {
                Class<?> tmp = except[i];
                except[i] = except[j];
                except[j] = tmp;
              }
          }
      }
    return (except);
  }

  public void setup(boolean keep, boolean need11Stubs, boolean need12Stubs,
                    boolean iiop, boolean poa, boolean debug, boolean warnings,
                    boolean noWrite, boolean verbose, boolean force, String classpath,
                    String bootclasspath, String extdirs, String outputDirectory)
  {
    this.need11Stubs = need11Stubs;
    this.need12Stubs = need12Stubs;
    this.verbose = verbose;
    this.noWrite = noWrite;

    // Set up classpath.
    StringTokenizer st =
      new StringTokenizer(classpath, File.pathSeparator);
    URL[] u = new URL[st.countTokens()];
    for (int i = 0; i < u.length; i++)
      {
        String path = st.nextToken();
        File f = new File(path);
        try
          {
            u[i] = f.toURL();
          }
        catch (java.net.MalformedURLException mue)
          {
            logError("malformed classpath component " + path);
            return;
          }
      }
    loader = new URLClassLoader(u);

    destination = outputDirectory;
  }

  private void findRemoteMethods()
    throws RMICException
  {
    List<Method> rmeths = new ArrayList<Method>();
    for (Class<?> cur = clazz; cur != null; cur = cur.getSuperclass())
      {
        Class<?>[] interfaces = cur.getInterfaces();
        for (int i = 0; i < interfaces.length; i++)
          {
            if (java.rmi.Remote.class.isAssignableFrom(interfaces[i]))
              {
                Class<?> remoteInterface = interfaces[i];
                if (verbose)
                  System.out.println
                    ("[implements " + remoteInterface.getName() + "]");

                // check if the methods declare RemoteExceptions
                Method[] meths = remoteInterface.getMethods();
                for (int j = 0; j < meths.length; j++)
                  {
                    Method m = meths[j];
                    Class[] exs = m.getExceptionTypes();

                    boolean throwsRemote = false;
                    for (int k = 0; k < exs.length; k++)
                      {
                        if (exs[k].isAssignableFrom(RemoteException.class))
                          throwsRemote = true;
                      }

                    if (! throwsRemote)
                      {
                        throw new RMICException
                          ("Method " + m + " in interface " + remoteInterface
                           + " does not throw a RemoteException");
                      }

                    rmeths.add(m);
                  }

                mRemoteInterfaces.add(remoteInterface);
              }
          }
      }

    // intersect exceptions for doubly inherited methods
    boolean[] skip = new boolean[rmeths.size()];
    for (int i = 0; i < skip.length; i++)
      skip[i] = false;
    List<MethodRef> methrefs = new ArrayList<MethodRef>();
    for (int i = 0; i < rmeths.size(); i++)
      {
        if (skip[i]) continue;
        Method current = rmeths.get(i);
        MethodRef ref = new MethodRef(current);
        for (int j = i+1; j < rmeths.size(); j++)
          {
            Method other = (Method) rmeths.get(j);
            if (ref.isMatch(other))
              {
                ref.intersectExceptions(other);
                skip[j] = true;
              }
          }
        methrefs.add(ref);
      }

    // Convert into a MethodRef array and sort them
    remotemethods =
      methrefs.toArray(new MethodRef[methrefs.size()]);
    Arrays.sort(remotemethods);
  }

  /**
   * Prints an error to System.err and increases the error count.
   */
  private void logError(Exception theError)
  {
    logError(theError.getMessage());
    if (verbose)
      theError.printStackTrace(System.err);
  }

  /**
   * Prints an error to System.err and increases the error count.
   */
  private void logError(String theError)
  {
    errorCount++;
    System.err.println("error: " + theError);
  }

  private static String getPrettyName(Class cls)
  {
    StringBuilder str = new StringBuilder();
    for (int count = 0;; count++)
      {
        if (! cls.isArray())
          {
            str.append(cls.getName());
            for (; count > 0; count--)
              str.append("[]");
            return (str.toString());
          }
        cls = cls.getComponentType();
      }
  }

  private static class MethodRef
    implements Comparable
  {
    Method meth;
    long hash;
    List<Class<?>> exceptions;
    private String sig;

    MethodRef(Method m) {
      meth = m;
      sig = Type.getMethodDescriptor(meth);
      hash = RMIHashes.getMethodHash(m);
      // add exceptions removing subclasses
      exceptions = removeSubclasses(m.getExceptionTypes());
    }

    public int compareTo(Object obj) {
      MethodRef that = (MethodRef) obj;
      int name = this.meth.getName().compareTo(that.meth.getName());
      if (name == 0) {
        return this.sig.compareTo(that.sig);
      }
      return name;
    }

    public boolean isMatch(Method m)
    {
      if (!meth.getName().equals(m.getName()))
        return false;

      Class[] params1 = meth.getParameterTypes();
      Class[] params2 = m.getParameterTypes();
      if (params1.length != params2.length)
        return false;

      for (int i = 0; i < params1.length; i++)
        if (!params1[i].equals(params2[i])) return false;

      return true;
    }

    private static List<Class<?>> removeSubclasses(Class<?>[] classes)
    {
      List<Class<?>> list = new ArrayList<Class<?>>();
      for (int i = 0; i < classes.length; i++)
        {
          Class<?> candidate = classes[i];
          boolean add = true;
          for (int j = 0; j < classes.length; j++)
            {
              if (classes[j].equals(candidate))
                continue;
              else if (classes[j].isAssignableFrom(candidate))
                add = false;
            }
          if (add) list.add(candidate);
        }

      return list;
    }

    public void intersectExceptions(Method m)
    {
      List<Class<?>> incoming = removeSubclasses(m.getExceptionTypes());

      List<Class<?>> updated = new ArrayList<Class<?>>();

      for (int i = 0; i < exceptions.size(); i++)
        {
          Class<?> outer = exceptions.get(i);
          boolean addOuter = false;
          for (int j = 0; j < incoming.size(); j++)
            {
              Class<?> inner = incoming.get(j);

              if (inner.equals(outer) || inner.isAssignableFrom(outer))
                addOuter = true;
              else if (outer.isAssignableFrom(inner))
                updated.add(inner);
            }

          if (addOuter)
            updated.add(outer);
        }

      exceptions = updated;
    }
  }
}
