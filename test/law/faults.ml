let all =
  Fault.
    [
      Short 1;
      Stall 1.;
      Hang;
      Reset;
      Eof;
      Refuse;
      Timeout;
      Unreachable;
      No_descriptor;
      Drop;
      Dup;
      Reorder;
      Garble 3;
      Cancel 1;
    ]
