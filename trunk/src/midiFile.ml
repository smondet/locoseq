
(******************************************************************************)
(* 
 * Parse a midi file with ocaml.
 * 
 * 
 *
 * - a good recapitulation of the StandardMidiFile format:
 *   http://www.ccarh.org/courses/253/handout/smf/
 * - some things about meta events:
 *   http://www.ccarh.org/courses/253/handout/smf/
 *
  *
 * *)
(******************************************************************************)


(******************************************************************************)
(* 
 * The parsing exception:
 * *)
exception Parsing_error of string ;;


(******************************************************************************)
(*
 * Practical Renamings:
 *)
let pr = Log.p ;;

module I = Int32 ;;

(******************************************************************************)
(* 
 *
 * Useful general functions:
 * read_long        :  reads a 32 bits value         -> int32
 * read_short       :  reads a 16 bits value         -> int
 * read_var_length  :  reads a variable length value -> int
 *
 * *)
let read_long fin =
  let ca = Int32.of_int (input_byte fin) in
  let cb = Int32.of_int (input_byte fin) in
  let cc = Int32.of_int (input_byte fin) in
  let cd = Int32.of_int (input_byte fin) in

  let ret = ref 0l in
  ret := Int32.add !ret (Int32.shift_left ca 24) ;
  ret := Int32.add !ret (Int32.shift_left cb 16) ;
  ret := Int32.add !ret (Int32.shift_left cc  8) ;
  ret := Int32.add !ret                   cd  ;
  !ret 
;;
let write_long fout lg = (
  output_byte fout (I.to_int (I.shift_right (I.logand lg 0xFF000000l) 24)) ;
  output_byte fout (I.to_int (I.shift_right (I.logand lg 0x00FF0000l) 16)) ;
  output_byte fout (I.to_int (I.shift_right (I.logand lg 0x0000FF00l)  8)) ;
  output_byte fout (I.to_int (              (I.logand lg 0x000000FFl)   )) ;
)

let read_short fin =
  let ca = input_byte fin in
  let cb = input_byte fin in
  let ret = ref 0 in
  ret :=  !ret + (ca * 256) ;
  ret :=  !ret + cb  ;
  !ret 
;;
let write_short fout sh = (
  output_byte fout ((sh land 0x0000FF00) / 256) ;
  output_byte fout (sh land 0x000000FF) ;
)
(* The (ugly) midi variable length value: *)
let read_var_length fin =  (
  let ret_val = ref 0l in
  let read_byte = ref 0l in
  let finish = ref false in
  let nb_read = ref 0 in
  while not !finish do
    read_byte := Int32.of_int (input_byte fin) ;
    incr nb_read ;
    ret_val := (
      Int32.add (Int32.shift_left !ret_val 7) (Int32.rem !read_byte 128l)) ; 
      finish := !read_byte < 128l ;
  done;
  if ( Int32.of_int max_int ) <= !ret_val 
  then (
    raise (Parsing_error "I'm trying to convert a 32 bits integer to a 31 bits one!");
  );
  (Int32.to_int !ret_val , !nb_read)
)


(* See pseudo-code: http://www.borg.com/~jglatt/tech/midifile/vari.htm => IS FALSE !!*)
let write_var_length_in_buffer buf lg = (
  let result = ref lg in
  let values = [| 0 ; 0 ; 0 ; 0 |] in
  let count = ref 0 in
  let f = ref false in

  f := !result < 128l ;
  while not !f do
    values.(!count) <- (I.to_int (I.logand !result 0x7Fl))  ;
    incr count ;
    result := I.shift_right !result 7 ;

    f := !result < 128l ;
  done;

  values.(!count) <- (I.to_int !result) land 0x7F ;
  incr count ;
  while !count >  1 do
    Buffer.add_char buf (char_of_int (values.(!count - 1) lor 0x80)) ;
    decr count;
  done;
  Buffer.add_char buf (char_of_int (values.(!count - 1)         )) ;
)


(******************************************************************************)
(* 
 * Midi File Parsing functions:
 * parse_smf 
 *   calls once read_header
 *   calls many times read_track 
 *     which uses make_* functions
 *
 * NOTE: only read_{header,track} functions call "input" functions
 * (other functions do not touch the file handle)
 *
 * *)

let read_header data fin =
  data.Midi.file_id           <- read_long fin  ;
  data.Midi.header_length     <- read_long fin  ;
  data.Midi.midi_format       <- read_short fin ;
  data.Midi.track_number      <- read_short fin ;
  data.Midi.per_quarter_note  <- read_short fin ;
;;

let make_meta_event tik serv ev_data =
  Midi.MetaEvent {
    Midi.meta_ticks = tik ;
    Midi.service_id = serv ;
    Midi.service_data = ev_data ;
}
;;


let read_track track fin =
  track.Midi.track_id <- read_long fin  ;
  if ( track.Midi.track_id <> 0x4D54726Bl) 
  then (
    raise( Parsing_error (Printf.sprintf 
    "Invalid Track Id: %lx" track.Midi.track_id));
  ) ;

  (* warn: it's the length in bytes (not in events) *)
  track.Midi.track_length <- read_long fin  ;

  (* We do not know the number of events so put them in a stack *)
  let event_stack = Stack.create () in


  let cur_ticks = ref 0 in
  let running_status = ref 0 in
  let read_byte = ref 0 in
  (* We need to count read bytes *)
  let nb_bytes_read = ref 0 in

  while !nb_bytes_read < (Int32.to_int track.Midi.track_length) - 1 do

    let tik,nbb =  read_var_length fin in
    cur_ticks := tik ;
    nb_bytes_read := !nb_bytes_read + nbb ;

    read_byte := input_byte fin ;
    (* Log.p "(read_byte=0x%x)\n" !read_byte ; *)
    incr nb_bytes_read ;

    if not ((!read_byte land 0x80) = 0)
    then (
      (* it is a status byte *)
      running_status := !read_byte ;
      read_byte := input_byte fin ;
      incr nb_bytes_read ;
    ) else (
      Log.p "Using the preceding (read_byte=0x%x)\n" !read_byte ;
    );
    
    let next_event =
      match !running_status with
      (* It is a META EVENT:  *)
      | rs when  (0xFF = rs) -> (
        let serv = !read_byte in
        let size,nbr = read_var_length fin in
        nb_bytes_read := !nb_bytes_read + nbr;
        let ev_data = Array.init  size (
          fun x -> incr nb_bytes_read ; input_byte fin ) in
        make_meta_event !cur_ticks serv ev_data ;
      )
      (* It is a 2 bytes MIDI EVENT: *)
      | rs when (((0x80 <= rs) && (rs <= 0xBF)) 
             || ((0xE0 <= rs) && (rs <= 0xEF))) -> (
        let note = !read_byte in
        let velo = input_byte fin in
        incr nb_bytes_read ;
        (* let cmd = make_2B_cmd rs note velo in *)
        Midi.MidiEvent {
          Midi.ticks = !cur_ticks ;
          Midi.status = rs land 0xF0 ;
          Midi.channel =  !running_status mod 16 ;
          Midi.data_1 = note ;
          Midi.data_2 = velo ;
        }
      )
      (* It is a 1 byte MIDI EVENT: *)
      | rs when ((0xB0 <= rs) && (rs <= 0xCF)) -> (
        let note = !read_byte in
        let velo = -1 in
        (* let cmd = make_1B_cmd rs !read_byte in *)
        Midi.MidiEvent {
          Midi.ticks = !cur_ticks ;
          Midi.status = rs land 0xF0 ;
          Midi.channel =  !running_status mod 16 ;
          Midi.data_1 = note ;
          Midi.data_2 = velo ;
        }
      )
      | rs -> (
        pr "Unknown event: %x (at tick: %d) !! !\n" rs tik ;
        raise (Parsing_error "Error: unknown event !!") ;
      ) 

    in
    Stack.push next_event event_stack ;
  done;
  (* Creation of the array of events: *)
  let ev_nb = Stack.length event_stack in
  track.Midi.events <- Midi.make_events ev_nb ;
  for i = ev_nb - 1 downto 0 do
    track.Midi.events.(i) <- Stack.pop event_stack ;
  done ;
  ()
;;


(* 
 * The "main" parsing function:
 * 
 * *)
let parse_smf file = (
  (* open_in_bin may behave like open_in on many systems *)
  let fin = open_in_bin file in
  let ret = Midi.empty_midi_data () in

  (* Read the header: *)
  read_header ret fin ;

  (* Read all the tracks: *)
  ret.Midi.tracks <- Midi.make_tracks ret.Midi.track_number ;
  Log.p "%d tracks.\n" ret.Midi.track_number ;
  for cur_track = 0 to ret.Midi.track_number - 1 do
    read_track ret.Midi.tracks.(cur_track) fin ;
  done; 
  ret
)

let write_header data chan = (
  write_long  chan  data.Midi.file_id           ;
  write_long  chan  data.Midi.header_length     ;
  write_short chan  data.Midi.midi_format       ;
  write_short chan  data.Midi.track_number      ;
  write_short chan  data.Midi.per_quarter_note  ;
)
let write_track track fout  = (


  let buffer = Buffer.create 10028 in
  let append_chari i = Buffer.add_char buffer (char_of_int i) in

  Array.iteri (
    fun i ev ->
      match ev with
      Midi.EmptyEvent | Midi.SysEvent _ -> () |
      Midi.MidiEvent e -> (
        (* Log.p "Here it is ! stat:%d chan: %d write: %x \n" e.Midi.status e.Midi.channel (e.Midi.status lor e.Midi.channel); *)
        write_var_length_in_buffer buffer (I.of_int e.Midi.ticks);
        append_chari (e.Midi.status lor e.Midi.channel) ;
        let rs = e.Midi.status in
        if (
          ((0x80 <= rs) && (rs <= 0xBF)) || ((0xE0 <= rs) && (rs <= 0xEF))
        ) then (
          append_chari e.Midi.data_1 ;
          append_chari e.Midi.data_2 ;
        ) else (
          Log.p "Special status:%d\n" rs ;
          append_chari e.Midi.data_1 ;
        );
      ) |
      Midi.MetaEvent e -> (
        if (e.Midi.meta_ticks = 1 ) then Log.p "Here it is ! \n"  ;
        (* Log.p "Write Meta Ev: ticks:%d id:%d\n" e.Midi.meta_ticks  e.Midi.service_id ; *)
        write_var_length_in_buffer buffer (I.of_int e.Midi.meta_ticks);
        append_chari 0xFF ;(* status of meta events... *)
        append_chari e.Midi.service_id ;
        write_var_length_in_buffer buffer
        (I.of_int (Array.length e.Midi.service_data));
        Array.iter ( fun c ->
          append_chari c ;
        ) e.Midi.service_data ;
      )

  ) track.Midi.events ;

  let len = I.of_int (Buffer.length buffer) in
  if (len <> track.Midi.track_length) then (
    Log.p "Grrr... must be a problem: %ld <> %ld\n" len track.Midi.track_length;
  );

  write_long fout track.Midi.track_id ;
  write_long fout len ;
  Buffer.output_buffer fout buffer ;

)


let write_smf data file = (
  let fout = open_out_bin file in
  write_header data fout ;
  for cur_track = 0 to data.Midi.track_number - 1 do
    write_track data.Midi.tracks.(cur_track) fout  ;
  done; 
  close_out fout ;
)



