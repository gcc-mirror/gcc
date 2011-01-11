/***
 * ASM: a very small and fast Java bytecode manipulation framework
 * Copyright (c) 2000-2005 INRIA, France Telecom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.objectweb.asm.optimizer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

/**
 * A Jar file optimizer.
 *
 * @author Eric Bruneton
 */
public class JarOptimizer {

    public static void main(final String[] args) throws IOException {
        File f = new File(args[0]);
        optimize(f);
    }

    static void optimize(final File f) throws IOException {
        if (f.isDirectory()) {
            File[] files = f.listFiles();
            for (int i = 0; i < files.length; ++i) {
                optimize(files[i]);
            }
        } else if (f.getName().endsWith(".jar")) {
            File g = new File(f.getParentFile(), f.getName() + ".new");
            ZipFile zf = new ZipFile(f);
            ZipOutputStream out = new ZipOutputStream(new FileOutputStream(g));
            Enumeration e = zf.entries();
            byte[] buf = new byte[10000];
            while (e.hasMoreElements()) {
                ZipEntry ze = (ZipEntry) e.nextElement();
                if (ze.isDirectory()) {
                    continue;
                }
                out.putNextEntry(ze);
                InputStream is = zf.getInputStream(ze);
                int n;
                do {
                    n = is.read(buf, 0, buf.length);
                    if (n != -1) {
                        out.write(buf, 0, n);
                    }
                } while (n != -1);
                out.closeEntry();
            }
            out.close();
            zf.close();
            f.delete();
            g.renameTo(f);
        }
    }
}
