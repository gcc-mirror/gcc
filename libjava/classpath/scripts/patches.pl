#!/usr/bin/perl -w
# Purpose is to move patches from upload directory to 
# public patches directory.  Any file not matching the correct 
# pattern is deleted.  Any patch file without a README and the
# file was last modified more than 120 minutes ago is deleted.
# Any README file without a patch file which was last modified
# more than 120 minutes ago is deleted.
#
# notes to self: as long as this runs as root do not worry
# about quota problems or disk space

use strict;

my ($upload_dir) = "/home/ftp/classpath/incoming";
my ($public_dir) = "/home/ftp/classpath/pub/patches";
my ($user) = "classpath";
my ($group) = "classpath";
my ($mode_dir) = "775";
my ($mode_file) = "664";
my (@patches) = ();

use vars qw($upload_dir $public_dir @patches $user $group
	    $mode_dir $mode_file);

# main
{
    @patches = &getPatches();
    &movePatches(@patches);
}

#---------------------------------------------------------------
# Purpose: To remove files not matching the correct pattern.
#   To remove README files without patches (last modified greater
#   than 2 hours).  To remove patches without README files (last
#   modified greater than 2 hours).
#---------------------------------------------------------------
sub getPatches
{
    my (@patches) = ();
    my (@entries) = ();
    my (%maybe) = ();
    my ($entry, $debug, $prefix, $junk, $file, $patch, $readme) = "";
    my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime,
	$mtime, $ctime, $blksize, $blocks) = "";

    $debug = 1;

    opendir(INCOMING, "$upload_dir") || die "could not open $upload_dir\n";
    @entries = grep( !/^\.\S+/, readdir(INCOMING)); # no .*
    closedir(INCOMING);
    foreach $entry (sort @entries)
    {
        if (($entry eq ".") || ($entry eq "..")) { next; }
        if (-d "$upload_dir/$entry")
        {
	    print "Directory: $upload_dir/$entry/\n";
        }
        elsif (-e "$upload_dir/$entry")
        {
            if ($entry eq ".message") { next; }
            if ($entry eq "README") { next; }
	    if ($entry !~ /^\w+-\d\d\d\d\d\d-\d+\.patch\.(gz|README)$/)
	    {
		print "REGEX FAILED: $entry\n";
		unlink("$upload_dir/$entry");
	    }
	    else
            {
                ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,
                 $ctime,$blksize,$blocks) = stat("$upload_dir/$entry");
                if ($size > 512000)
                {
                    print "LARGE PATCH: $entry\n";
                    unlink("$upload_dir/$entry");
                }
                else 
                {
                    ($prefix,$junk) = split(/(\.gz|\.README)/, $entry, 2);
                    $maybe{$prefix} += 1;
                }
            }
	}
    }

    foreach $entry (keys(%maybe))
    {
        if ($maybe{$entry} == 2)
        { 
            $patch = "$entry.gz"; 
            $readme = "$entry.README"; 

            ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,
             $ctime,$blksize,$blocks) = stat($patch);
            if (time-$mtime > 900)
            {
                ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,
                 $ctime,$blksize,$blocks) = stat($readme);
                if (time-$mtime > 900)
                {
                    $patches[$#patches+1] = $entry;
                }
            }
        }
        else
        {
	    if (-e "$upload_dir/$entry.gz")
            { 
                unlink("$upload_dir/$entry.gz");
                print "STALE PATCH: $entry.gz\n";
            }
            elsif (-e "$upload_dir/$entry.README") 
            {
                unlink("$upload_dir/$entry.README");
                print "STALE README: $entry.README\n";
            }
        }
    }
    return (@patches);
}

#---------------------------------------------------------------
# Purpose: To move the patches to the proper directory and set
#   the permissions correctly.
#---------------------------------------------------------------
sub movePatches
{
    my (@patches) = @_;
    my ($patch) = "";
    my ($fail) = 0;

    if (!(-d "$public_dir"))
    {
	system("mkdir -p $public_dir");
	system("chown $user.$group $public_dir");
	system("chmod $mode_dir $public_dir");
    }
    foreach $patch (@patches)
    {
	if (-e "$public_dir/$patch.gz")
	{
	    print "Patch exists: $public_dir/$patch.gz\n";
	    $fail = 1;
	}
	if (-e "$public_dir/$patch.README")
	{
	    print "README exists: $public_dir/$patch.README\n";
	    $fail = 1;
	}
	if ($fail == 0)
	{
	    system("mv $upload_dir/$patch.gz $public_dir/$patch.gz");
	    system("mv $upload_dir/$patch.README $public_dir/$patch.README");
	    system("chown $user.$group $public_dir/*");
	    system("chmod $mode_file $public_dir/*");
            open(MAIL, "|mail -s \"Classpath: $patch uploaded\" core\@classpath.org") || die "could not open mail\n";
            print MAIL "GNU Classpath FTP Maintenance\n";
	    print MAIL "\n";
            print MAIL "Added Files:\n";
            print MAIL "ftp://ftp.classpath.org/pub/patches/$patch.gz\n";
            print MAIL "ftp://ftp.classpath.org/pub/patches/$patch.README\n\n";
            close(MAIL);
	}
    }
}
