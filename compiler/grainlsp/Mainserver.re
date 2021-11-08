/* Will wait up to 100ms */
let canRead = desc => {
  let (r, _, _) = Unix.select([desc], [], [], 0.5);
  r != [];
};

let run = log => {
  let stdin_descr = Unix.descr_of_in_channel(stdin);

  log("Starting main loop");
  let rec loop = (~isShuttingDown, ~documents, ~compiledCode) =>
    // let state = tick(state);
    if (canRead(stdin_descr)) {
      switch (Rpc.readMessage(log, stdin)) {
      | _ => loop(~isShuttingDown, ~documents, ~compiledCode)
      };
    } else {
      // log("looping....");
      loop(~isShuttingDown, ~documents, ~compiledCode);
    };

  let initialize = (~documents, ~compiledCode) =>
    switch (Rpc.readMessage(log, stdin)) {
    // | Notification(_) => log("")
    | Message(id, "initialize", _) =>
      log("initialize");

      Rpc.sendCapabilities(log, stdout, id);

      loop(~isShuttingDown=false, ~documents, ~compiledCode);

    | _ => failwith("Client must send 'initialize' as first event")
    };

  let documents = Hashtbl.create(128);
  let compiledCode = Hashtbl.create(128);

  initialize(~documents, ~compiledCode);

  log("After initialise");
};
