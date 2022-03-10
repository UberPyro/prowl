open Batteries

let ast = 
    Parse.program Lex.token
    % Lexing.from_channel
