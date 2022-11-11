</index> Home

```UF
                    88        88 88888888888  
                    88        88 88           
                    88        88 88           
                    88        88 88aaaaa      
                    88        88 88"""""      
                    88        88 88           
                    Y8a.    .a8P 88           
                     `"Y8888Y"'  88           
```


Version: 6


"UF" is a traditional Forth system for the "uxn/varvara" virtual
computer. Uf is written in Tal and itself and provides an interactive
text interface, uxn assembler, decompiler and screen editor. The
implementation strategy is a mixture of subroutine-threading and
native code generation, but currently only minimal optimizations
are performed.  The mapping of Forth source code to VM instructions
is nevertheless relatively direct, so performance should be more
than sufficient in all but the most speed-critical situations. Cells
are 16 bits, the full system consists of around 20K, with around
4K needed for screen memory and buffers.

Suggestions for improvement or patches providing enhancements and
corrections are very welcome, see the User's manual for information
on how to contact the author.

This software was written by Felix L. Winkelmann and has been released 
into the public domain. Do with it whatever you like.

<file:/uf/README> Installation instructions and general information

<archive:/uf/uf.rom> Download precompiled ROM

<archive:/uf/uf-6.tgz> Get the source code

Release History:

Version 7:
    * Fixed off-by-one errors in `>=` and `<='.

Version 6:
    * Added `include` and `included` (suggested by Alex Wennerberg).
    * Dropped ^i handling (uxnemu already shows stacks on F2).
    * Use text color for cursor to be visible in monochromatic mode.

Version 5:
    * Added `audio` to select current audio device.
    * `page` was not exposed to the default dictionary (thanks to Éric 
      Ortie).
    * `]` didn't restore the proper vocabulary order.

Version 4:
    * Fixed serious bug in logic for input-grab (used by ^r, ^g and
      ^m) which caused return stack underflows.
    * ^d joins next line if followed by whitespace.
    * Unrecognized control-code combinations are ignored now.
    * Loading a block prints the block number.
    * A warning is shown when a word is redefined.
    * Added `-;`.
    * `thru` ignores non-existent blocks.

Version 3:
    * Added ^l (page) shortcut (suggested by Devine Lu Linvega).
    * Added support for multiline snarf + paste (^ENTER, ^x).
    * Newlines in snarfed text spanning multiple lines and pasted
      text are correctly handled now.
    * New words `2variable`, `2constant` and `where`.
    * Made some small optimizations in the kernel (courtesy of 
      Devine Lu Linvega).
    * Exposed `stdin` to override console input events.
    * Using the scroll-wheel in the editor jumps to previous or next 
      block.
    * Sh-Up recalls last entered line in interactive mode.
    * Shift + Arrow-keys replace block-navigation keys ^h, ^n and ^p.
    * Added ^y as an alias for ^v.
    * ^g, followed by a number of digits allows 
      direct jump to indicated block.
    * Removed `copy`, use ^r to copy block interactively.
    * Added ^m to move block.
    * Fixed missing argument in `filedelete`.

Version 2:
    * Small optimizations in kernel, suggested by Devine Lu Linvega.
    * Typo fixes in documentation, thanks to Devine Lu Linvega and
      Prof. Dr. Dr. Feinfinger.
    * Fixed wrong calculation in `depth`.
    * New words `snarf`, `yank`, `theme` expose editor functionality
      for general use.
    * `.s` prints the contents of the working stack now to the 
      graphical console instead of just invoking the system's debug
      port.
    * New words: `2r@`, `pick`, `.vocs`, `marker`, `list` and `new`.
    * `unused` is now available in the full non-graphical environment.
    * Added minimal support for shadow (documentation) blocks: `Doc` 
      and `Code` (and ctrl-key ^h) switch between a block and the
      corresponfing shadow block (blocks 1000+).
    * `filename` doesn't use pad but dedicated 256 byte buffer.
    * The cursor position in the editor is retained now properly
      between block changes.
    * Fixed bug in `scan` (thanks to Alex Wennerberg).
    * The tabulator key inserts spaces instead of overwriting
      following text.
    * Block lines starting with "\" are drawn in a different color.
    * Fixed bug in `postpone`.

Version 1: 
    * Initial release
