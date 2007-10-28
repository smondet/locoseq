(******************************************************************************)
(*      Copyright (c) 2007, Sebastien MONDET                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)


(** 
 
OCaml types and functions providing high level access to the jack sequencer

@author S. Mondet

 *)

(** {3 The sequencer object} *)

(** The sequencer object (abstract type) *)
type sequencer


(**
 The sequencer constructor
 should be called as  {[
 let my_seq = make_sequencer "client_name" 
 [| "input_port_A" ; "input_port_B" |]
   [| "out1" ; "out2" ; "outN" |] in
   ]}
 *)
external make_sequencer: string -> string array -> string array -> sequencer
= "ml_jackseq_make"

external close_sequencer: sequencer -> unit
= "ml_jackseq_close"

external output_event:
  sequencer -> port:int -> stat:int -> chan:int ->
  dat1:int -> dat2:int -> unit
= "ml_jackseq_output_event_bytecode" "ml_jackseq_output_event"

external get_input:
  sequencer -> (int*int*int*int*int) array
  = "ml_jackseq_get_input"




let test_jack_seq () = (

  let seq = 
    make_sequencer "JackSeqTest" 
    [| "in0"; "in1"; "in2"; "in3"; "in4"; "in5"; "in6"; "in7"; "in8"; "in9"; "in10";|]
    [| "out0"; "out1"; "out2"; "out3"; "out4"; "out5"; "out6"; "out7"; "out8";|]
  in
  for i = 0 to 25000000 do
    Thread.delay 0.02;
    let input = get_input seq in
    Array.iter (fun (port, stat, chan, dat1, dat2) ->
      Printf.printf "[%d] port:%d stat:%x chan:%d dat1:%d dat2:%d\n%!" i
      port stat chan dat1 dat2;
      output_event seq ~port:0 ~stat ~chan~dat1 ~dat2;
    ) input;
  done;

  close_sequencer seq;
  Unix.sleep 3;
)

(* 
 
ocamlfind ocamlopt -package unix,threads.posix -linkpkg  -cclib -ljack -thread jackseq.c jackSequencer.ml

 *)
