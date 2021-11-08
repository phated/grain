open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

/* Will wait up to 100ms */
let canRead = desc => {
  let (r, _, _) = Unix.select([desc], [], [], 0.1);
  r != [];
};

let lens_sig = (t: Types.type_expr) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Printtyp.type_expr(ppf, t);
  Format.pp_print_flush(ppf, ());
  let sigStr = Buffer.contents(buf);
  sigStr;
};

let run = log => {
  let stdin_descr = Unix.descr_of_in_channel(stdin);

  log("Starting main loop");
  let rec loop = (~isShuttingDown, ~documents, ~compiledCode) =>
    // let state = tick(state);
    if (canRead(stdin_descr)) {
      switch (Rpc.readMessage(log, stdin)) {
      | Message(id, action, json) =>
        log("received message " ++ action);
        switch (action) {
        | "textDocument/hover" =>
          Messages.getHover(log, id, json, compiledCode)
        | "textDocument/codeLens" =>
          Messages.processGetLenses(log, id, json, compiledCode)
        | "textDocument/completion" =>
          Completion.processCompletion(log, id, json, compiledCode, documents)
        | _ => ()
        };
        loop(~isShuttingDown, ~documents, ~compiledCode);
      | Notification(method, json) =>
        log("received notification " ++ method);
        switch (method) {
        | "textDocument/didOpen"
        | "textDocument/didChange" =>
          Notifications.textDocument_didOpenOrChange(
            log,
            json,
            documents,
            compiledCode,
          )

        | _ => ()
        };
        loop(~isShuttingDown, ~documents, ~compiledCode);
      | Error(_) =>
        log("!!!Received Error");
        loop(~isShuttingDown=true, ~documents, ~compiledCode);
      };
    } else {
      loop(~isShuttingDown, ~documents, ~compiledCode);
    };

  let initialize = (~documents, ~compiledCode) =>
    switch (Rpc.readMessage(log, stdin)) {
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
