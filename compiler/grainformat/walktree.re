open Grain_parsing;

type node_t =
  | Code(Grain_parsing.Location.t)
  | Comment(Grain_parsing.Location.t);

let allLocations: ref(list(node_t)) = ref([]);

let getNodeLoc = (node: node_t): Grain_parsing.Location.t =>
  switch (node) {
  | Code(loc) => loc
  | Comment(loc) => loc
  };

let getCommentLoc = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt) => cmt.cmt_loc
  | Block(cmt) => cmt.cmt_loc
  | Doc(cmt) => cmt.cmt_loc
  | Shebang(cmt) => cmt.cmt_loc
  };

// iterate the tree to make an ordered list of locations
// we can then find expressions between locations

//

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let print_loc = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = get_raw_pos_info(loc.loc_end);
  /*let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in*/

  if (startchar >= 0) {
    if (line == endline) {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ ","
        ++ string_of_int(endchar),
      );
    } else {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ " - "
        ++ string_of_int(endline)
        ++ ":"
        ++ string_of_int(endchar),
      );
    };
  };
};

let walktree =
    (
      statements: list(Grain_parsing.Parsetree.toplevel_stmt),
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  //let iterator = {...Ast_iterator.default_iterator,};

  let comment_locations =
    List.map(c => Comment(getCommentLoc(c)), comments);

  allLocations := comment_locations;

  let iter_location = (self, location) => {
    // print_loc("walked location:", location);
    allLocations := List.append(allLocations^, [Code(location)]);
  };

  let iterator = {...Ast_iterator.default_iterator, location: iter_location};

  List.iter(iterator.toplevel(iterator), statements);

  allLocations :=
    List.sort(
      (node1: node_t, node2: node_t) => {
        let loc1 = getNodeLoc(node1);

        let loc2 = getNodeLoc(node2);

        let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
        let (_, raw2l, raw2c, _) = get_raw_pos_info(loc2.loc_start);

        if (raw1l < raw2l) {
          (-1);
        } else if (raw1l == raw2l) {
          if (raw1c < raw2c) {
            (-1);
          } else {
            1;
          };
        } else {
          1;
        };
      },
      allLocations^,
    );
  // List.iter(l => print_loc("Sorted loc:", l), allLocations^);

  List.iter(
    n =>
      switch (n) {
      | Comment(l) => print_loc("comment", l)
      | Code(l) => print_loc("code", l)
      },
    allLocations^,
  );
};

let get_loc_node_after =
    (loc: Grain_parsing.Location.t, allLocations: list(node_t))
    : option(node_t) => {
  // print_loc("looking for a node afte", loc);
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc.loc_start);
  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc.loc_end);

  let afterloc: option(node_t) =
    List.fold_left(
      (acc, node: node_t) => {
        switch (acc) {
        | Some(_) => acc // nothing more to do
        | None =>
          let l = getNodeLoc(node);
          let (_, raw2l, raw2c, _) = get_raw_pos_info(l.loc_start);
          // let (_, raw2le, raw2ce, _) = get_raw_pos_info(l.loc_end);

          if (raw2l > raw1l) {
            //  print_loc("A match", l);
            Some(node);
          } else if (raw2l == raw1l) {
            if (raw2c > raw1ce) {
              //  print_loc("B match", l);
              Some(node);
            } else {
              None;
            };
          } else {
            None;
          };
        }
      },
      None,
      allLocations,
    );

  afterloc;
};

let get_node_after = (loc: Grain_parsing.Location.t): option(node_t) => {
  let nodes =
    List.filter(
      n =>
        switch (n) {
        | Code(_) => true
        | Comment(_) => false
        },
      allLocations^,
    );
  get_loc_node_after(loc, nodes);
};

let get_comment_after = (loc: Grain_parsing.Location.t): option(node_t) => {
  let nodes =
    List.filter(
      n =>
        switch (n) {
        | Code(_) => false
        | Comment(_) => true
        },
      allLocations^,
    );
  get_loc_node_after(loc, nodes);
};

let get_next_after = (loc: Grain_parsing.Location.t): option(node_t) => {
  get_loc_node_after(loc, allLocations^);
};