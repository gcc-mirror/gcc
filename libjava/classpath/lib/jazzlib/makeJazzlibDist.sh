#!/bin/zsh

echo "----- Run this from the classpath/java/util/zip directory -----"
echo "-----                                                     -----"
echo "----- Options:                                            -----"
echo "-----                                                     -----"
echo "----- juz -- build .tar.gz and .zip archives in java.util.zip namespace"
echo "----- jar -- build .jar file in java.util.zip and java.util.jar namespaces"
echo "-----     -- build .tar.gz. and .zip archive for net.sf.jazzlib namespace"
echo "-----                                                     -----"
echo "----- Edit this script to change the release number       -----"
echo "----- Do rm -rf dist when you're finished                 -----"
echo "----- 30 May 2002 John Leuner <jewel@debian.org>      -----"

RELEASE_NUMBER=07

# $1 is the archive command, eg "tar czvf" or "zip" or "jar cf"
# $2 is the archive suffix, eg ".zip" or ".tar.gz"
# $3 is the "-binary" flag, which may be empty
# $4 is the "-juz" suffix, which may be empty
# $5 is the set of files that need to be md5-summed
# $6 is the set of files in addition to $5 that are to be archived

function create_archive {
    md5sum ${=5} > md5sums
    gpg --clearsign md5sums
    ${=1} jazzlib${3}-0.$RELEASE_NUMBER${4}${2} ${=5} ${=6}
    rm -f md5sums
    rm -f md5sums.asc
}  

# $1 is the package name, ie java.util.zip or net.sf.jazzlib

function make_javadoc {
    rm -rf javadoc
    mkdir javadoc
    javadoc -sourcepath . -d javadoc/ $1
}

case "$1" in
    juz)
    mkdir -p dist/java/util/zip

    #make source archive
    cp *.java dist/java/util/zip
    pushd dist
    
    make_javadoc java.util.zip

    cp ../../../../COPYING .

    foo=(java/util/zip/*.java)
    create_archive "tar czvf" ".tar.gz" "" "-juz" "$foo" "javadoc md5sums md5sums.asc COPYING" 
    create_archive "zip" ".zip" "" "-juz" "$foo" "javadoc md5sums md5sums.asc COPYING" 
    
    popd

    #make binary distro second
    cp ../../../lib/java/util/zip/*.class dist/java/util/zip
    pushd dist

    foo=(java/util/zip/*.class)
    create_archive "tar czvf" ".tar.gz" "-binary" "-juz" "$foo" "javadoc md5sums md5sums.asc COPYING"
    create_archive "zip" ".zip" "-binary" "-juz" "$foo" "javadoc md5sums md5sums.asc COPYING"
    
    popd
    ;;
    jar)
    mkdir -p dist/java/util/zip
    mkdir -p dist/java/util/jar

    #make binary distro second
    cp ../../../lib/java/util/zip/*.class dist/java/util/zip
    cp ../../../lib/java/util/zip/../jar/*.class dist/java/util/jar
    pushd dist

    cp ../../../../COPYING .
    foo=(java/util/zip/*.class)
    foo=($foo java/util/jar/*.class)

    create_archive "fastjar cf" ".jar" "-binary" "-juz" "$foo" "md5sums md5sums.asc COPYING"
    
    popd
    ;;
    *)
    #copy files to dist directory and make net.sf.jazzlib the package name
    
    mkdir -p dist/net/sf/jazzlib
    cp *.java dist/net/sf/jazzlib
    for i in dist/net/sf/jazzlib/*.java ; do 
	sed -e "s/java\.util\.zip/net.sf.jazzlib/" < $i > $i.tmp ;
	mv $i.tmp $i;
    done
	
    pushd dist
	
    make_javadoc "net.sf.jazzlib"

    cp ../../../../COPYING .

    foo=(net/sf/jazzlib/*.java)
    create_archive "tar czvf" ".tar.gz" "" "" "$foo" "javadoc md5sums md5sums.asc COPYING" 
    create_archive "zip" ".zip" "" "" "$foo" "javadoc md5sums md5sums.asc COPYING" 

    #compile the source
    javac net/sf/jazzlib/*.java

    foo=(net/sf/jazzlib/*.class)
    create_archive "tar czvf" ".tar.gz" "-binary" "" "$foo" "javadoc md5sums md5sums.asc COPYING"
    create_archive "zip" ".zip" "-binary" "" "$foo" "javadoc md5sums md5sums.asc COPYING"

    #back to dir
    popd

    ;;
esac
ls -la dist/{*.tar.gz,*.jar,*.zip}




