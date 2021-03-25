type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
val matchedString = ref ""
val insideString = ref false

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos)
	    in if !commentDepth > 0
	       then (ErrorMsg.error pos "unclosed comment"; Tokens.EOF(pos,pos))
	       else if !insideString
	       then (ErrorMsg.error pos "unclosed string"; Tokens.EOF(pos,pos))
	       else Tokens.EOF(pos,pos)
	    end

%%
%s COMMENT STRING;
alpha         = [a-zA-Z];
alphanumeric  = [a-zA-Z0-9_];
digit         = [0-9];
triple_digits = [0-9][0-9][0-9];
whitespace    = [ \t\n\r];
%%
<INITIAL, COMMENT>\n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STRING>\n           => (ErrorMsg.error yypos "newline in string";
			 lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());


<INITIAL, COMMENT>{whitespace}+ => (continue());


<INITIAL>type     => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>var      => (Tokens.VAR(yypos,yypos+3));
<INITIAL>function => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>break    => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>of       => (Tokens.OF(yypos,yypos+2));
<INITIAL>end      => (Tokens.END(yypos,yypos+3));
<INITIAL>in       => (Tokens.IN(yypos,yypos+2));
<INITIAL>nil      => (Tokens.NIL(yypos,yypos+3));
<INITIAL>let      => (Tokens.LET(yypos,yypos+3));
<INITIAL>do       => (Tokens.DO(yypos,yypos+2));
<INITIAL>to       => (Tokens.TO(yypos,yypos+2));
<INITIAL>for      => (Tokens.FOR(yypos,yypos+3));
<INITIAL>while    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>else     => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>then     => (Tokens.THEN(yypos,yypos+4));
<INITIAL>if       => (Tokens.IF(yypos,yypos+2));
<INITIAL>array    => (Tokens.ARRAY(yypos,yypos+5));


<INITIAL>":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"|"  => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"  => (Tokens.AND(yypos,yypos+1));
<INITIAL>">=" => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"  => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"  => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="  => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"/"  => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"  => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"-"  => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"+"  => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"."  => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"}"  => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"{"  => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"]"  => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"["  => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>")"  => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"("  => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>";"  => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"  => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","  => (Tokens.COMMA(yypos,yypos+1));


<INITIAL>{digit}+ => (Tokens.INT(let val SOME x = Int.fromString yytext in x end,
				 yypos,yypos+size yytext));


<INITIAL>{alpha}{alphanumeric}* => (Tokens.ID(yytext,yypos,yypos+size yytext));


<INITIAL>\"                 => (YYBEGIN STRING; matchedString := ""; insideString := true;
				continue());
<STRING>\"                  => (YYBEGIN INITIAL; insideString := false;
				Tokens.STRING(!matchedString,yypos,yypos+size(!matchedString)));
<STRING>\\n                 => (matchedString := !matchedString ^ "\n"; continue());
<STRING>\\t                 => (matchedString := !matchedString ^ "\t"; continue());
<STRING>\\\^[@A-Z\[\\\]^_]  => (let val encoding = (ord (String.sub(yytext,2))) - 64
				in matchedString := !matchedString ^ (str(chr encoding))
				end; continue());
<STRING>\\{triple_digits}   => (let val SOME encoding = Int.fromString(substring(yytext,1,3))
				in matchedString := !matchedString ^ (str(chr encoding))
				end; continue());
<STRING>\\\"                => (matchedString := !matchedString ^ "\""; continue());
<STRING>\\\\                => (matchedString := !matchedString ^ "\\"; continue());
<STRING>\\{whitespace}+\\   => (continue());
<STRING>\\                  => (ErrorMsg.error yypos "illegal string escape"; continue());
<STRING>.                   => (matchedString := !matchedString ^ yytext; continue());


<INITIAL>"/*" => (YYBEGIN COMMENT; commentDepth := 1; continue());
<INITIAL>"*/" => (ErrorMsg.error yypos "unmatched comment"; continue());
<COMMENT>"/*" => (commentDepth := !commentDepth+1; continue());
<COMMENT>"*/" => (commentDepth := !commentDepth-1;
		  if !commentDepth=0
		  then (YYBEGIN INITIAL; continue())
		  else (continue()));
<COMMENT>.    => (continue());


. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
