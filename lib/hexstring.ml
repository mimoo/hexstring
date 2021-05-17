(* encoding bytearray -> hexstring *)

let encode (bytearray : bytes) : string =
  let dummy_char = '_' in
  let start_of_digit_0_in_ascii_table = 0x30 in
  let start_of_lower_case_a_in_ascii_table = 0x61 in
  let hex_digit_of_int (x : int) : char =
    assert (x >= 0);
    assert (x < 16);
    char_of_int
      (if x < 10 then x + start_of_digit_0_in_ascii_table
      else x - 10 + start_of_lower_case_a_in_ascii_table)
  in
  let rec aux bytearray len cur_pos buf =
    if cur_pos < len then (
      let x = int_of_char @@ Bytes.get bytearray cur_pos in
      let c1 = hex_digit_of_int (x lsr 4) in
      let c2 = hex_digit_of_int (x land 0x0F) in
      Bytes.set buf (cur_pos * 2) c1;
      Bytes.set buf ((cur_pos * 2) + 1) c2;
      aux bytearray len (succ cur_pos) buf)
  in
  let len = Bytes.length bytearray in
  let buf_len = 2 * len in
  let buf = Bytes.make buf_len dummy_char in
  aux bytearray len 0 buf;
  Bytes.to_string buf

let%test "encoding" =
  let bytearray = Bytes.of_string "\x01\x02\x03\x04\x0a\xbd\xff" in
  let hexstring = encode bytearray in
  hexstring = "010203040abdff"

(* helper to decode a byte *)

let decode_1char = function
  | '0' -> Some 0
  | '1' -> Some 1
  | '2' -> Some 2
  | '3' -> Some 3
  | '4' -> Some 4
  | '5' -> Some 5
  | '6' -> Some 6
  | '7' -> Some 7
  | '8' -> Some 8
  | '9' -> Some 9
  | 'a' -> Some 10
  | 'b' -> Some 11
  | 'c' -> Some 12
  | 'd' -> Some 13
  | 'e' -> Some 14
  | 'f' -> Some 15
  | _ -> None

let decode_2chars ((c1, c2) : char * char) : (char, string) result =
  let fst = decode_1char c1 in
  let snd = decode_1char c2 in
  match (fst, snd) with
  | None, _ | _, None -> Error "nope"
  | Some fst, Some snd ->
      let res = (fst lsl 4) lxor snd in
      Ok (char_of_int res)

let%test "decoding two chars 01" =
  let d = decode_2chars ('0', '1') in
  d = Ok '\x01'

let%test "decoding two chars ff" =
  let d = decode_2chars ('f', 'f') in
  d = Ok '\xff'

let%test "decoding two erroneous chars" =
  let d = decode_2chars ('z', 'f') in
  Result.is_error d

(* decoding hexstring -> bytearray *)

let rec parse_string (res : bytes) (offset : int) (ss : string) =
  match String.length ss with
  | 0 -> Ok res
  | _ -> (
      let byte = Str.first_chars ss 2 in
      let rest = Str.string_after ss 2 in
      let c1 = byte.[0] in
      let c2 = byte.[1] in
      match decode_2chars (c1, c2) with
      | Error err -> Error err
      | Ok c ->
          Bytes.set res offset c;
          parse_string res (offset + 1) rest)

let decode hexstring =
  match String.length hexstring with
  | len when len mod 2 <> 0 -> Error "length must be a multiple of 2"
  | len ->
      let res = Bytes.make (len / 2) '\x00' in
      parse_string res 0 hexstring

let%test "decoding empty hexstring" =
  let d = decode "" in
  d = Ok Bytes.empty

let%test "decoding bad-length hexstring" =
  let d = decode "1" in
  Result.is_error d

let%test "decoding bad-char hexstring" =
  let d = decode "120z" in
  Result.is_error d

let%test "decoding valid hexstring" =
  let d = decode "01020304" in
  d = Ok (Bytes.of_string "\x01\x02\x03\x04")
