(* Each schedule below is printed into a program that builds it again from
   that text, in the scope its pp documents. *)

module Flow = Eio_net_chaos.Flow
module Datagram = Eio_net_chaos.Datagram
module Net = Eio_net_chaos.Net

let flows =
  Flow.
    [
      Schedule.none;
      Schedule.script ();
      Schedule.script
        ~reads:[ Deliver 3; Deliver (-1); Coalesce 2; Stall 1; Reset; Eof ]
        ~writes:[ Accept 2; Accept 0; Fail ]
        ();
      Schedule.seeded 7;
      Schedule.seeded ~dribble:4 ~stall:2 ~short_write:3 ~reset_after:10
        ~reset_kind:`Eof ~fail_write_after:20 (-1234);
    ]

let datagrams =
  Datagram.
    [
      Schedule.none;
      Schedule.script
        [ Pass; Drop 2; Dup; Reorder; Truncate (-1); Garble 4; Delay 5 ];
      Schedule.seeded 7;
      Schedule.seeded ~loss:1. 7;
      Schedule.seeded ~loss:0.1 ~dup:(1. /. 3.) ~reorder:1e-5 ~truncate:(-0.)
        ~garble:0.25 ~delay:3 7;
    ]

let nets =
  Net.
    [
      Schedule.v ();
      Schedule.v ~flow:(Flow.Schedule.script ~reads:[ Deliver 1 ] ()) ();
      Schedule.seeded ~dribble:3 ~max_accepts:2
        ~connect:[ Refuse; Timeout; Slow 2; Open ]
        9;
    ]

(* [case ~pp ~path ~scope v] checks [v] against the text [pp] prints for it,
   read back inside the local opens [scope] of [path]. *)
let case pp ~path ~scope v =
  let printed = Fmt.str "%a" pp v in
  let opens = String.concat "" (List.map (fun m -> m ^ ".(") scope) in
  let closes = String.make (List.length scope) ')' in
  Fmt.pr "let () = check %S (Fmt.str \"%%a\" %s.pp %s%s%s)@." printed path opens
    printed closes

let () =
  Fmt.pr
    "let failed = ref false@.@.let check printed rebuilt =@.  if not \
     (String.equal printed rebuilt) then begin@.    failed := true;@.    \
     Fmt.epr \"printed: %%s@@.rebuilt: %%s@@.\" printed rebuilt@.  end@.@.";
  List.iter
    (case Flow.Schedule.pp ~path:"Eio_net_chaos.Flow.Schedule"
       ~scope:[ "Eio_net_chaos.Flow"; "Schedule" ])
    flows;
  List.iter
    (case Datagram.Schedule.pp ~path:"Eio_net_chaos.Datagram.Schedule"
       ~scope:[ "Eio_net_chaos.Datagram"; "Schedule" ])
    datagrams;
  List.iter
    (case Net.Schedule.pp ~path:"Eio_net_chaos.Net.Schedule"
       ~scope:[ "Eio_net_chaos"; "Net"; "Schedule" ])
    nets;
  Fmt.pr "@.let () = if !failed then exit 1@."
