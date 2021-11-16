open Grain;
open Compile;
open Grain_parsing;
open Grain_diagnostics;

type node_t =
  | Expression(Typedtree.expression)
  | Pattern(Typedtree.pattern)
  | NoNode(string);

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

// let is_point_inside_stmt = (loc1: Grain_parsing.Location.t, line: int) => {
//   let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);

//   let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);

//   if (line == raw1l || line == raw1le) {
//     true;
//   } else if (line > raw1l && line < raw1le) {
//     true;
//   } else {
//     false;
//   };
// };

// The assumption is if a comment starts inside a location, it must end inside it too

let is_point_inside_location =
    (log, loc1: Grain_parsing.Location.t, line: int, char: int) => {
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);

  let res =
    if (line == raw1l) {
      if (char >= raw1c) {
        if (line == raw1le) {
          if (char <= raw1ce) {
            true;
          } else {
            false;
          };
        } else {
          true;
        };
      } else {
        false;
      };
    } else if (line == raw1le) {
      if (char <= raw1ce) {
        true;
      } else {
        false;
      };
    } else if (line > raw1l && line < raw1le) {
      true;
    } else {
      false;
    };

  res;
};

let is_location_inside_statement =
    (log, location: Grain_parsing.Location.t, stmt: Parsetree.toplevel_stmt) => {
  let (_, raw1l, raw1c, _) = get_raw_pos_info(stmt.ptop_loc.loc_start);
  let (_, raw1le, raw1ce, _) = get_raw_pos_info(stmt.ptop_loc.loc_end);

  let (_, line, char, _) = get_raw_pos_info(location.loc_start);

  let res =
    if (line == raw1l) {
      if (char >= raw1c) {
        if (line == raw1le) {
          if (char <= raw1ce) {
            true;
          } else {
            false;
          };
        } else {
          true;
        };
      } else {
        false;
      };
    } else if (line == raw1le) {
      if (char <= raw1ce) {
        true;
      } else {
        false;
      };
    } else if (line > raw1l && line < raw1le) {
      true;
    } else {
      false;
    };

  res;
};

let is_location_inside_location =
    (
      log,
      location: Grain_parsing.Location.t,
      outer_location: Grain_parsing.Location.t,
    ) => {
  let (_, raw1l, raw1c, _) = get_raw_pos_info(outer_location.loc_start);
  let (_, raw1le, raw1ce, _) = get_raw_pos_info(outer_location.loc_end);

  let (_, line, char, _) = get_raw_pos_info(location.loc_start);

  let res =
    if (line == raw1l) {
      if (char >= raw1c) {
        if (line == raw1le) {
          if (char <= raw1ce) {
            true;
          } else {
            false;
          };
        } else {
          true;
        };
      } else {
        false;
      };
    } else if (line == raw1le) {
      if (char <= raw1ce) {
        true;
      } else {
        false;
      };
    } else if (line > raw1l && line < raw1le) {
      true;
    } else {
      false;
    };

  res;
};

let debug_comments = comments =>
  if (List.length(comments) < 1) {
    print_endline("No comments");
  } else {
    let _ =
      List.map(
        c => {
          let comment_string = Comments.get_comment_source(c);
          print_endline(comment_string);
        },
        comments,
      );
    ();
  };
