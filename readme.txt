=============================================================
Context Scripting Suite for Delphi 7,2005,2006,2007,2009,2010,XE,XE2,XE3,XE4,XE5
Copyright (c) 2004-2010, Michael Baytalsky
All Rights Reserved
-------------------------------------------------------------
Version: 1.40


   WHAT'S IN THIS FILE
   ===================
   I. ABOUT CONTEXT SCRIPTING SUITE
   II. LIST OF COMPONENTS BY UNIT
   III. INSTALLATION INSTRUCTIONS
   IV. REGISTRATION

------------------------------------------
I. ABOUT CONTEXT SCRIPTING SUITE

Context Scripting Suite is desinged to compile into PCode and
execute scripts using TCtxScript component. Context Scripting
is shipped with Object Pascal-like script compiler (TCtxPasCompiler).
It only supprts Variant variables and uses variant array for execution
stack. Context Scripting is extremely flexible and resembles
a real compiler. It supports the following features:

  - NEW! Sophisticated text/html reporting component, based on 
    Context Scripting capabilities.
  - Fast script compilation into PCode & execution. 
  - Virtual compiler allows to add more languages.
  - Script debugging: stopping, resuming, evaluating
    watch expressions, inspection of call stack and more (see
    DemoIDE project in demos).
  - Fast and one time only name resolution done with 
    virtualized and extensible introspectors, that allow to
    extend and create your own object model.
  - Small footprint.
  - Easily extensible array of external functions.
  - Easy to create custom object model and publish methods of 
    existing classes.
  - Supports dinamic variant arrays.
  - Supports array properties, default array properties, including
    strings, collections, etc.
  - Parameters can be passed by value and by address (var, out)
  - Supports const parameters.
  - Allows to invoke Ole automation objects via IDispatch interface
    (for instance, this could be used to automate MS Word, MS Excel, etc.)
  - Allows to compile and evaluate procedures, functions and 
    simple expressions.
  - Support exception handling with try except finally raise
    directives and exception variable, containing ExceptObject.
  - The following syntaxis is implemented by default TCtxScriptCompiler:
    while...do, repeat...until, for...to, for...downto, if...then[...else],
    goto, @<variable> statements.

Please see Demo projects included in this package to learn how to
add Context Scripting to your projects. Enjoy!

ATTENTION!! Some of the demos require freeware SynEdit v1.1 component.
It is available at source forge: http://synedit.sourceforge.net
You can also download it directly from
http://www.torry.net/vcl/edits/enh/ossynedit.zip

II. LIST OF COMPONENTS BY UNIT
==============================

  Unit                  Depend on       Components
  ----------            ---------       --------------------
  CtxScript             None            TCtxScript         
                        CtxPasCompiler  TCtxScriptCode     
                                        TCtxCompiler
                                        TCtxIntrospector(s)

  CtxPasCompiler        CtxScript       TCtxPasCompiler

  CtxUnit               CtxScript       TCtxUnit
                                        TCtxUnitIntrospector 

  CtxUnitEditor         CtxScript       EditCtxUnit function
                        CtxUnit         & TfrmCtxUnitEditor

  CtxPkgClasses         CtxScript       RTL & VCL Imports
  CtxPkgDB
  CtxPkgSysUtils

  CtxActions            CtxScript       TCtxTriggerAction
                                        TCtxScriptAction

  CtxTextReport                 CtxScript       TCtxTextReporter

III. INSTALLATION INSTRUCTIONS
==============================
                            
Installing Components
~~~~~~~~~~~~~~~~~~~~~
Uninstall previously installed versions of this package from
Delphi IDE. Close Delphi IDE and start it over to ensure, that none
of the prior versions of the above named components are installed.
Each package name marks version of Delphi this package is build
for. Default installation procedure will perform all installation 
tasks automatically, however, you can also install packages
manually if automatic installation failed for some reason.

When installing for your particular version of Delphi, always choose
a package with appropriate name, i.e. CtxScriptPkgD7.dpk for Delphi 7,
CtxScriptPkgD2006.dpk for Delphi 2006, etc. These packages are located in
corresponding folders, i.e. libd7, libd2005, libd2007, etc.

1. Install CtxScriptPkgDx.dpk, containing database independant components.

 - Use "File\Open..." menu item of Delphi IDE to open design-time
   package CtxScriptPkgDx.dpk. 

 - In the "Package..." window click "Compile" button to compile the
   package and then "Install" button to register Context Scripting
   package in the component palette. When package is installed the
   components will appear on the 'Context Scripting' page.

2. Use "Tools\Environment Options" menu item of Delphi IDE to 
access Library tab page. Make sure, that path to the 'libdX',
folders and 'source' folder (if you have version with the full
source code) are added to the Library Search & Browse Paths settings.
It should be added there automatically during the installation.
Default installation folder (recommended) is
C:\Program Files\Context Software\Scripting

Installing Help File
~~~~~~~~~~~~~~~~~~~~
Help file should be automatically integrated into Delphi IDE
during the installation. If you're installing Context Scripting
into C++ Builder or unsupported version of Delphi (such as 3 or 4)
you should performs the steps below to register the help file
manually.

To include a help file into Delphi/BCB IDE help system environment
and make it available in design time you need to make
several steps:

1. Run Borland Delphi/CB, select item Help | Customize... from the
main menu. 

2. Choose Index tab to include index information from ctxdbext.hlp file
into Delphi common help system. Pick Edit | Add Files from menu
and then select ctxscript.hlp file in the File Open dialog and press OK.

3. Choose Link tab and do the same to include help topics into the Delphi
help system as well.

4. Go to Contents tab and add ctxscript.cnt file.

5. Select File | Save Project from the main menu to save changes you made
and close the OpenHelp utility.

The included help file will be available immediately within Delphi/BCB IDE.


IV. REGISTRATION
================

You may NOT DISTRIBUTE the source code conatined in this package.
You also may not sell, lease or rent these components "as is" or
with any changes. You may freely use it for trial purposes or to
create software for your own use (see license.txt for more
information).

You must REGISTER your copy of the Context Scripting Suite in
order to use it to design, develop and deploy commercial
software. In order to register Context Scripting Suite, please
visit our web site at http://www.contextsoft.com/products/ctxscript
and fill in electronic ordering form. If you have questions
or problems accessing our website, please contact me directly
(via e-mail, fax or regular mail) and you will be given
instruction on how to register using other means of communication.

Thank you for using Context Scripting Suite!

If you have any questions, comments or suggestions, please
contact me at mike@contextsoft.com.


Sincerely,
Michael Baytalsky

