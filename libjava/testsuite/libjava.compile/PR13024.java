import java.io.*;
import java.util.zip.*;

class PR13024 {
    void isZipOrJarArchive(File file) throws IOException {
        ZipFile zipFile = null;

        try {
            zipFile = new ZipFile(file);
        } finally {
            if (zipFile != null) {
                try {
                    zipFile.close();
                } catch (IOException ignored) {}
            }
        }
    }
}
