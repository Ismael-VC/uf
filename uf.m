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


Version: 3


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

<archive:/uf/uf-2.tgz> Get the source code

Release History:

Version 3:
    * Added ^l (page) shortcut (suggested by Devine Lu Linvega).
    * Added ^Ret (mark) shortcut and support for multiline snarf/yank.
    * Made some small optimizations in the kernel, courtesy of Devine.

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
