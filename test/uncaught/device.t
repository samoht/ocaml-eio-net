An uncaught device report prints as a run's does: a table of the
sequences that differ, and a summary.

  $ ./device.exe
  Fatal error: exception Eio_net_chaos.device: 1 of 44,159 sequences differ from Linux
    scenario              expected                     got
    ------------------------------------------------------------------------------
    failed connect: path  Not_found EADDRINUSE         Not_found EADDRINUSE
                          EADDRINUSE then EADDRINUSE   Refused then EADDRINUSE
    44,158 passed, 1 differed, 132 left out: 13 that wait 60 seconds for a port to
    be free again, 5 that use up every port, 114 that send off the loopback
    interface
  [2]
