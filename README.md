# 441RacketProject 
UMKC 441 
# Introduction
This project is a parser checker for the given grammar that was assigned. It reads in a file, and outputs if that file's contents followed the rules of the grammar or not. If not, it also provides the line where the error occured. 
# Sources
There were several key sources that helped me build this parser.

This detailed on how to build a lexer to make the parsing much easier
> https://matt.might.net/articles/lexers-in-racket/

This helped me learn how to program in racket. It was one of my goto source for any general questions about racket.
> https://beautifulracket.com/

This contains documentation over the parsing library that I used.
> https://docs.racket-lang.org/parser-tools/Lexers.html#%28part._.Lexer_.S.R.E_.Operators%29

This is the racket documentation site which I primarly used to look up functions
> https://docs.racket-lang.org/

# Grammar
The grammar is as the following:
> program -> linelist $$  
> linelist -> line linelist | epsilon  
> line -> label stmt linetail  
> label -> id: | epsilon  
> linetail -> stmt+ | epsilon  
> stmt -> id = expr;  
> | if (boolean) stmt;  
> | while (boolean) linelist endwhile;  
> | read id;  
> | write expr;| goto id;  
> | gosub id;  
> | return;  
> | break;  
> | end;  
> boolean -> true | false | expr bool-op expr  
> bool-op -> < | > | >= | <= | <> | =  
> expr -> id etail | num etail | (expr)  
> etail -> + expr | - expr | * expr | / expr | epsilon  
> id -> [a-zA-Z][a-zA-Z0-9]*  
> num -> numsign digit digit*  
> numsign -> + | - | epsilon  

In addition, reservered words cannot be used as a statement label.
> gotohere: if (x > 5) y = y+1  //is valid;  
> goto: if (x > 5) y = y+1      //is a syntax error.  

# Gramamr "Issues"
This grammar has some interesting, "quirks". For one, an if statement has to end with two semi-colons. This is because the if rule has a semi-colon at the end, but it also calls another statement...which will have a semi-colon. This means the following code is invalid.
> if(x < 5) y = 3;

The correct code would be this:
> if(x < 5) y = 3;;


Another issue is that the Commutative Property is not followed at times. The most glaring offence is the following.
> x = (3 * 2) * 3;

This code is invalid! The correct code would be instead this:
> x = 3 * (3 * 2);

Huh? The reason for this is that in the grammar, an expression can be of three forms. The first two are fine as they call on etail which itself can be an operator plus an expression. This allows us to have multiple expressions in one expression. The third form, however, is a big problem. The expression gets terminated after the parenthesis. This means that multiple expressions can't be had. There can only be one. What does this entail? Well, in our example, having an etail follow the expression, which should be valid in any sane language, is not permitted. It instead causes an error!!!


# Error Printing
For this parser, in some errors, the offending line is printed as one ahead of what we would expect.
> num1 = x + 5  
> num2 = y + 6;


In this code, we would expect the error to be on line one, as there is no semi-colon. However, the error would actually be on line two. Why? In the program's eyes, it is num2 which is the offending token. It expected a semi-colon where num2 was, thus resulting in the error being on that line. This is a feature of the parser as space does not matter for semi-colons. To highlight this, look at the following example


> num1 = x + 5  
> ;num2 = y + 6  
> ;


This is valid code. As long as the semi colon is following the end of the expression, it is considered valid. It is important to know this, otherwise, one may get confused as to why the parser is erroring the incorrect lines. An extreme example of this can be seen in the follwing:


> line1 x = y  
> line2  
> line3  
> line4  
> line5 $$


In here, the linex statements are just to show the lines. They are not part of the code. In this code, the error would be on line 5, as the program expected a semi-colon there but instead got a $$. 
