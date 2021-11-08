let getTextDocumenUriAndPosition = json => {
  let params = Yojson.Safe.Util.member("params", json);

  let textDocument = Yojson.Safe.Util.member("textDocument", params);

  let position = Yojson.Safe.Util.member("position", params);

  let uri =
    Yojson.Safe.Util.member("uri", textDocument)
    |> Yojson.Safe.Util.to_string_option;

  let line =
    Yojson.Safe.Util.member("line", position)
    |> Yojson.Safe.Util.to_int_option;

  let char =
    Yojson.Safe.Util.member("character", position)
    |> Yojson.Safe.Util.to_int_option;

  (uri, line, char);
};

let getHover = (log, id, json, compiledCode) => {
  let params = Yojson.Safe.Util.member("params", json);

  switch (getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    if (Hashtbl.mem(compiledCode, uri)) {
      let ln = line + 1;

      let compiledCode = Hashtbl.find(compiledCode, uri);
      let node = Utils.findBestMatch(compiledCode, ln, char);
      switch (node) {
      | Some(stmt) =>
        let (signature, loc) = Utils.getHoverFromStmt(log, stmt, ln, char);

        let range =
          switch (loc) {
          | None => Utils.loc_to_range(stmt.ttop_loc)
          | Some(l) => Utils.loc_to_range(l)
          };
        Rpc.sendHover(log, stdout, id, signature, range);
        ();
      | None => ()
      };
    } else {
      ();
    }
  | _ => ()
  };
};
