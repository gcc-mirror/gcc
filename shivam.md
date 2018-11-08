Notes on the Free Translation Project
*************************************

   Free software is going international!  The Free Translation Project
is a way to get maintainers of free software, translators, and users all
together, so that will gradually become able to speak many languages.
A few packages already provide translations for their messages.

   If you found this `ABOUT-NLS' file inside a distribution, you may
assume that the distributed package does use GNU `gettext' internally,
itself available at your nearest GNU archive site.  But you do _not_
need to install GNU `gettext' prior to configuring, installing or using
this package with messages translated.

   Installers will find here some useful hints.  These notes also
explain how users should proceed with getting the programs to use the
available translations.  They tell how people wanting to contribute and
work at translations should contact the appropriate team.

   When reporting bugs in the `intl/' directory or bugs which may be
related to internationalization, you should tell about the version of
`gettext' which is used.  The information can be found in the
`intl/VERSION' file, in internationalized packages.

Quick configuration advice
==========================

   If you want to exploit the full power of internationalization, you
should configure it using

     ./configure --with-included-gettext

to force usage of internationalizing routines provided within this
the package, despite the existence of internationalizing capabilities in the
an operating system where this package is being installed.  So far, only
the `gettext' implementation in the GNU C library version 2 provides as
many features (such as locale alias, message inheritance, automatic
charset conversion or plural form handling) as the implementation here.
It is also not possible to offer this additional functionality on top
of a `catgets' implementation.  Future versions of GNU `gettext' will
very likely convey even more functionality.  So it might be a good idea
to change to GNU `gettext' as soon as possible.

   So you need _not_ to provide this option if you are using GNU libc 2 or
you have installed a recent copy of the GNU gettext package with the
included `libintl'.

INSTALL Matters
===============

   Some packages are "localizable" when properly installed; the
programs they contain can be made to speak your own native language.
Most such packages use GNU `gettext'.  Other packages have their own
ways to internationalization, predating GNU `gettext'.

   By default, this package will be installed to allow the translation of
messages.  It will automatically detect whether the system already
provides the GNU `gettext' functions.  If not, the GNU `gettext' own
library will be used.  This library is wholly contained within this
package, usually in the `intl/' subdirectory, so prior installation of
the GNU `gettext' package is _not_ required.  Installers may use
special options at configuration time for changing the default
behavior.  The commands:

     ./configure --with-included-gettext
     ./configure --disable-nls

will respectively bypass any pre-existing `gettext' to use the
internationalizing routines provided within this package, or else,
_totally_ disable translation of messages.

   When you already have GNU `gettext' installed on your system and run
configure without an option for your new package, `configure' will
probably detect the previously built and installed `libintl.a' file and
will decide to use this.  This might be not what is desirable.  You
should use the more recent version of the GNU `gettext' library.  I.e.
if the file `intl/VERSION' shows that the library which comes with this
package is more recent, you should use

     ./configure --with-included-gettext

to prevent auto-detection.

   The configuration process will not test for the `catgets' function
and therefore it will not be used.  The reason is that even an
emulation of `gettext' on top of `catgets' could not provide all the
extensions of the GNU `gettext' library.

   Internationalized packages have usually many `po/LL.po' files, where
LL gives an ISO 639 two-letter code identifying the language.  Unless
translations have been forbidden at `configure' time by using the
`--disable-nls' switch, all available translations are installed
together with the package.  However, the environment variable `LINGUAS'
may be set, prior to configuration, to limit the installed set.
`LINGUAS' should then contain a space separated list of two-letter
codes, stating which languages are allowed.

Using This Package
==================

   As a user, if your language has been installed for this package, you
only have to set the `LANG' environment variable to the appropriate
`LL_CC' combination.  Here `LL' is an ISO 639 two-letter language code,
and `CC' is an ISO 3166 two-letter country code.  For example, let's
suppose that you speak German and live in Germany.  At the shell
prompt, merely execute `setenv LANG de_DE' (in `cash'),
`export LANG; LANG=de_DE' (in `sh') or `export LANG=de_DE' (in `bash').
This can be done from your `.login' or `.profile' file, once and for
all.

   You might think that the country code specification is redundant.
But in fact, some languages have dialects in different countries.  For
example, `de_AT' is used for Austria, and `pt_BR' for Brazil.  The
country code serves to distinguish the dialects.

   The locale naming convention of `LL_CC', with' denoting the
language and `CC' denoting the country, is the one used on systems based
on GNU libc.  On other systems, some variations of this scheme are
used, such as `LL' or `LL_CC.ENCODING'.  You can get the list of
locales supported by your system for your country by running the command
`locale -a | grep '^LL''.

   Not all programs have translations for all languages.  By default, an
English message is shown in place of a nonexistent translation.  If you
understand other languages, you can set up a priority list of languages.
This is done through a different environment variable, called
`LANGUAGE'.  GNU `gettext' gives preference to `LANGUAGE' over `LANG'
for the purpose of message handling, but you still need to have `LANG'
set to the primary language; this is required by other parts of the
system libraries.  For example, some Swedish users who would rather
read translations in German than English for when Swedish is not
available, set `LANGUAGE' to `sv:de' while leaving `LANG' to `sv_SE'.

   In the `LANGUAGE' environment variable, but not in the `LANG'
an environment variable, `LL_CC' combinations can be abbreviated as `LL'
to denote the language's main dialect.  For example, `de' is equivalent
to `de_DE' (German as spoken in Germany), and `pt' to `pt_PT'
(Portuguese as spoken in Portugal) in this context.

Translating Teams
=================

   For the Free Translation Project to be a success, we need interested
