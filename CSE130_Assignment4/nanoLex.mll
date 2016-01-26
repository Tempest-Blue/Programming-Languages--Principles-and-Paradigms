{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
  | [' ' '\n' '\r' '\t']			{token lexbuf}
  | "let"											{LET}
  | "rec" 										{REC}
  | "="												{EQ}
  | "in"											{IN}
  | "fun" 										{FUN}
  | "->"											{ARROW}
  | "if"											{IF}
  | "then"										{THEN}
  | "else"										{ELSE}
  | "+"												{PLUS}
  | "-"												{MINUS}
  | "*"												{MUL}
  | "/"												{DIV}
  | "<"												{LT}
  | "<="											{LE}
  | "!="											{NE}	
  | "&&"											{AND}
  | "||"											{OR}
  | "("												{LPAREN}
  | ")"												{RPAREN}
  | "["												{LBRAC}
  | "]"												{RBRAC}
  | ";"												{SEMI}
  | "::"											{COLONCOLON}
  | ['0'-'9']+ as number      {Num (int_of_string number)}
  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']* as str {Id str}
  | "true"                    {TRUE}
  | "false"                   {FALSE}
  | eof         							{EOF}
  | _           							{raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'"))}