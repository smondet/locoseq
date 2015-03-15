### Bugs: ###

  * GuiEditor: X.org kills the application when drawn track becomes too big !

### Features: ###

  * **done** port this to the project's wiki
  * implement the quasi-generic track editor
    * **almost done** make it usable
    * make it beautiful
  * document code:
    * use explicit variable names in code (no btn, sngnam, etc...)
    * **started**  in the .ml's
    * **done** a CSS for code documentation
    * document CLI options
  * make up documentation framework
    * **done** OMakefile for pdflatex and htmlByHevea
    * **done** \filename
    * **done** \ShellcmdWithResult (but cannot use verbatim, and fancyvrb seems incompatible with hevea)
    * \OptionDescr
    * **done** \SourceCodeOCaml -> packge listing
    * **done** \SourceCodeC -> idem
  * **done** re-do the README without txt2tags
  * decide what to do with "play" action in InputMgr
  * **done** 'main' with options (the file to load, jack client name, see licence, ...)
  * **done** s/alsa/jack\_midi/g (no more libasound dependency)
  * **almost done** re-implement the tracker engine (still lack of some useful meta events)
  * **done** Compile against any version of JACK (`nframes` issue)


**-> 0.1**

  * optimize gui space for lists of tracks and interactions.
  * keyboard short cuts (open, save, quit...)
  * port portability inspection to latex or xml

**-> 0.2**

  * import/collapse songs
  * save "playing" state of a track (if not save on file, allow set before play)
  * undo stack

**-> 0.3**

  * jack transport (for tempo, play, stop)

**-> 0.99**


  * real-life testing & debug
    * test with dummy midi input

**-> 1.0rcs**

  * more debug ;-)

**-> 1.0**

  * export2midi functionnality (take one (meta)track and render one midi file)
  * allow operations on input arguments (add, multiply...)
  * s/gtk/whatever/g **?**

**-> 2.0**

