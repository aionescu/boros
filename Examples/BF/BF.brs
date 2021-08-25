#!/usr/bin/env boros

let wrap n =
  if n == 256 then
    0
  else if n == -1 then
    255
  else
    n
in

let state = {- { tape = [0], crr = 0 } -} in

let evalOp op =
  if op == '+' then
    state.tape.[state.crr] <- wrap $ state.tape.[state.crr] + 1
  else if op == '-' then
    state.tape.[state.crr] <- wrap $ state.tape.[state.crr] - 1
  else if op == '<' then
    if state.crr == 0 then (
      state.crr <- length state.tape - 1;
      state.tape <- replicate (length state.tape) 0 + state.tape
    ) else
      state.crr <- state.crr - 1
  else if op == '>' then (
    if state.crr == length state.tape - 1 then
      state.tape <- state.tape + replicate (length state.tape) 0;

    state.crr <- state.crr + 1
  ) else if op == ',' then
    state.tape.[state.crr] <- getChar ()
  else if op == '.' then
    putChar state.tape.[state.crr]
  else
    if state.tape.[state.crr] then
      (iter evalOp op; evalOp op)
in

let parse ops =
  let _ = { scope = [], stack = [] } in

  ops |> iter (op ->
    if contains op "+-<>,." then
      _.scope <- _.scope + [op]
    else if op == '[' then (
      _.stack <- [_.scope] + _.stack;
      _.scope <- []
    ) else if op == ']' then
      if not _.stack then
        throw "Unmatched ']'"
      else
        _.scope <- pop _.stack + [_.scope]
  );

  if not _.stack then
    _.scope
  else
    throw "Unmatched '['"
in

let ops = {- parse $ readFile args.[0] -} in

if not ops then
  halt ();

evalOp $ pop ops;

comments.[0] <- state;
comments.[1] <- ops
