**Locoseq** is a midi sequencer designed for live playing looping tracks.  You can
create control tracks (called _"meta-tracks"_) that drive the sequencer (schedule
track _x_, mute, change tempo...) and you can configure how the sequencer
reacts on a given input (midi-in port or keyboard).


The application is build on top of the
[jack](http://jackaudio.org/) API, it is written in
[ocaml](http://www.ocaml.org) (except some C code)
and uses GTK+ and glade
([lablgtk2](http://wwwfun.kurims.kyoto-u.ac.jp/soft/lsl/lablgtk.html))
for its GUI.


The source code is available under a classic MIT license.


It is still under development, so only SVN access is available. If you
have suggestions, remarks or whatever, send an email to the project [owner](http://seb.mondet.org).
