#!/usr/bin/env boros

let iter f l =
  let end = length l in
  let go i =
    if i != end then
      (f l.[i]; go $ i + 1)
  in
  go 0
in

let map f l =
  let end = length l in
  let go i =
    if i != end then
      [f l.[i]] + go (i + 1)
    else
      []
  in
  go 0
in

let wrap n =
  if n == 256 then
    0
  else if n == -1 then
    255
  else
    n
in

let state = { tape = [0], crr = 0 } in

let incr _ = state.tape.[state.crr] <- wrap $ state.tape.[state.crr] + 1 in
let decr _ = state.tape.[state.crr] <- wrap $ state.tape.[state.crr] - 1 in

let movL _ =
  if state.crr == 0 then
    state.tape <- [0] + state.tape
  else
    state.crr <- state.crr - 1
in

let movR _ =
  if state.crr == length state.tape - 1 then
    state.tape <- state.tape + [0];

  state.crr <- state.crr + 1
in

let read _ = state.tape.[state.crr] <- getChar () in
let write _ = putChar state.tape.[state.crr] in

let loop ops _ =
  if state.tape.[state.crr] then
    evalOps ops |> loop ops

and evalOps ops = iter (op -> op ()) ops
in

let parse ops =
  let end = length ops in
  let go scope stack i =
    if i == end then
      (if not stack then
        reverse scope
      else
        throw "Unmatched '['")
    else if ops.[i] == "+" then
      go ([incr] + scope) stack (i + 1)
    else if ops.[i] == "\-" then
      go ([decr] + scope) stack (i + 1)
    else if ops.[i] == "<" then
      go ([movL] + scope) stack (i + 1)
    else if ops.[i] == ">" then
      go ([movR] + scope) stack (i + 1)
    else if ops.[i] == "," then
      go ([read'] + scope) stack (i + 1)
    else if ops.[i] == "." then
      go ([write] + scope) stack (i + 1)
    else if ops.[i] == "[" then
      go [] ([scope] + stack) (i + 1)
    else if ops.[i] == "]" then
      (if not stack then
        throw "Unmatched ']'"
      else
        let parent = pop stack in
        go ([loop (reverse scope)] + parent) stack (i + 1))
    else
      go scope stack (i + 1)
  in
  go [] [] 0
in

evalOps $ parse $ explode $ readFile args.[0]
