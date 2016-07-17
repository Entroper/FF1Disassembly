
FF Disassembly readme!
 v1.0   Complete!

  Disch
--------------------------------------------


This disassembly has been an on-again off-again (mostly off) project of mine for literally
years.  I finally got around to finishing it!

This is a fully commented and documented, reassemblable disassembly of the US release of
Final Fantasy for the NES.  Everything you need to reassemble is included.

This can be used for whatever means you want, but the goal was for it to allow for a
ridiculous easy way to perform ASM hacks and bugfixes to the game.

Final Fantasy has numerous bugs.  As I was going through this disassembly, I uncovered several.
I tried to label them all with "BUGGED".  So if you do a "find in files" for that, you will
be able to see most of them.  I try to get into detail of how they're bugged and usually
offer a suggested fix.


The *.asm files are pretty self explanitory.  However there are a few other files:

- build.bat is a batch file to generate a ROM from the source.  So just make your changes to
   the asm, run build.bat, and your hack is done!  **NOTE** that build.bat will delete any
   "bank_*.bin" and "*.o" files in the root directory as part of its cleanup.

- macros.inc includes a handful of macros that are [sparesely] used.  Really the main ones are the
   CRITPAGECHECK and PAGECHECK which error/warning if a timed loop (branch) crosses a page boundary,
   as this would screw with the timing of the routine (crossing a page adds another cycle)

- Constants.inc includes various constants.  Should be self explanitory mostly.  I tried to keep the
   number of addresses listed in there to a minimum.  The ones that are in there are primarily
   for addresses that point somewhere in the middle of a block of binary data, so it's hard
   to create them with labels.

- variables.inc is the closest thing to a RAM map that I have.  It's not *really* as organized
   as it could be, and some of the names are questionable, but hopefully get the point
   across well enough.

- "some formats.txt" is some very sketchy notes I made about some less-than-obvious formats used
  by the game.  Specifically tile attributes for overworld and standard maps, and the battle formation
  data.  This is a holdover from when I started this project.  As I got further into it, I started
  putting these descriptions in large block comments in the actual code -- since that seemed to
  make more sense and was more organized.

  
  
As mentioned, since work for this has spread over several years, my style may be inconsistent in
parts.  Sorry!  I like to think I improved as time went on.


For large blocks of data that's intermingled in the code, I've been trying to put those into seperate
binary files and .INCBIN'ing them.  The /bin/ directory has them.  The naming is a little
inconsistent, but one thing to note is that if they have an address in them ("0D_storytext_A800.bin"
for example) then they include a pointer table at the start.


Anyway that's about it.  A lot of this isn't polished yet, as I was going to go over everything and
clean it up/make it prettier once I finished all the commenting.


Have fun!

-Disch-


CHANGES:

v1.0
----------------------
- Completed documentation of ALL code
- Renamed .bin files to be more consistent

v0.4
----------------------
- don't remember  =x

v0.3
----------------------
- Fixed so it actually assembles now (was missing files before)
- Intro screen and music driver commented.


v0.2
----------------------
- converted it back to 16K bank sizes (rather than the previous 8K nonsense)
- much more stuff commented


First Release:
----------------------
- first release!