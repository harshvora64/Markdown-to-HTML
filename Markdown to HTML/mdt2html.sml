fun conv(fl) = 
    (String.explode(TextIO.inputAll ( TextIO.openIn (fl))) @ [#"\n"]);

fun chk(x,l) = 
    if l = [] then false
    else if x=hd(l) then true
    else chk(x,tl(l));

fun olsyn(s) =
 let
    val n = [#"0",#"1",#"2",#"3",#"4",#"5",#"6",#"7",#"8",#"9"]
    in
    if s = [] then (false,s)
    else if hd(s) = #"." then (true,tl(s))
    else if chk(hd(s),n) then olsyn(tl(s))
    else (false,s)
    end;

fun countangle(n,s) = 
    if s=[] then (n,s)
    else if hd(s) = #" " orelse hd(s) = #"\t" then countangle(n,tl(s))
    else if hd(s) = #">" then countangle(n+1,tl(s))
    else (n,s);

fun trim(s,t) = 
if s = [] then (s,t)
else if hd(s) = #"\n" then trim(tl(s),[])
else if hd(s) = #" " orelse hd(s) = #"\t" then trim(tl(s),t@[hd(s)])
else (s,t);

fun prtbq(f,n) = 
    if n = 0 then ["\n"]
    else if f = 0 then prtbq(f,n-1)@["<BLOCKQUOTE>\n"]
    else  prtbq(f,n-1)@["</BLOCKQUOTE>\n"];

fun countspaces(n,s) = 
    if s=[] then (n,s)
    else if hd(s) = #" " then countspaces(n+1,tl(s)) 
    else if hd(s) = #"\t" then countspaces(n+4,tl(s))
    else (n,s);

fun closeall(temp) = 
if temp=[] then ["\n</LI>\n"]
else if hd(temp) = 0 then ["</OL>"]@closeall(tl(temp))
else ["</UL>"]@closeall(tl(temp));

fun closelsts(x,temp) = 
    if temp=[] then ["\n"]
    else if x<hd(temp) then 
    if hd(tl(temp)) = 0 then ["</OL>"]@closelsts(x,tl(tl((temp))))
    else ["</UL>"]@closelsts(x,tl(tl(temp)))
    else ["\n"];

fun newtemp(x,temp) = 
if temp = [] then []
else if x < hd(temp) then newtemp(x,tl(tl(temp)))
else temp;

fun isLink(word) = 
    let
        val w = String.map Char.toUpper word
    in 
        String.isPrefix "HTTP" w
    end;

fun err(s) = 
    ([],[s],[]);

(**)
fun nl2(s,out,temp) =
    if s = [] then 
        (s,out@["\n</P>\n<P>\n"],[0])
    else if hd(s) = #"\n" then
        nl2(tl(s),out,temp)
    else if hd(s) = #" " then
        nl2(tl(s),out,temp)
    else if hd(s) = #"\t" then
        nl2(tl(s),out,temp)
    else
        (s,out@["\n</P>\n<P>\n"],[0]);

fun nl1(s,out,temp) = 
    if s=[] then
        (s,out,temp)
    else if hd(s) = #" " then
        nl1(tl(s),out,temp)
    else if hd(s) = #"\t" then
        nl1(tl(s),out,temp)
    else if hd(s) = #"\n" then nl2(s,out,temp)
    else 
        (s,out,[0]);

(**)
fun otherEscape(s,out,temp) = 
    if s = [] then (s,out@["\\ "],[])
    else if hd(s) = #"\\" then (tl(s),out@["\\"],[])
        else if hd(s) = #"\"" then (tl(s),out@["\""],[])
            else (tl(s),out@[Char.toString(hd(s))],[]);
(**)
fun otherLinkTag(s,out,temp) = 
    if s=[] then (s,out,[])
    else if hd(s) = #">" then if isLink(String.concat(temp)) then (tl(s),out @ ["<A HREF = \"",String.concat(temp),"\">",String.concat(temp),"</A>"],[]) else (tl(s),out @ ["<",String.concat(temp),">"],[])
        else otherLinkTag(tl(s),out,temp@[Char.toString(hd(s))]); 
(**)
fun otherLink3(s,out,temp) =
    if s=[] then 
        err("Link does not specify address 2")
    else
        if hd(s) = #")" then
            (tl(s),[],temp)
        else
            otherLink3(tl(s),[],temp@[Char.toString(hd(s))])

fun otherLink2(s,out,temp) = 
    if s=[] then 
        (s,out,[])
    else 
        if hd(s) = #"]" then 
            if hd(tl(s)) = #"(" then
                let 
                    val v = otherLink3(tl(tl(s)),[],[])  
                in 
                    (#1 v,out @ ["<A HREF = \"",String.concat(#3 v),"\">",String.concat(temp),"</A>"],[])
                end
            else
                (s,out@["["]@temp,[])
        else otherLink2(tl(s),out,temp@[Char.toString(hd(s))]); 
(**)
fun underline(s,out,temp) = 
    if s = [] then (s,out@["</U>"],[])
    else if hd(s) = #"_" then underline(tl(s),out@[" "]@temp,[])
    else if hd(s) = #" " then (String.explode(String.concat(temp))@s,out@["</U>"," "],[])
    else if hd(s) = #"*" then (String.explode(String.concat(temp))@s,out@["</U>"," "],[])
    else if hd(s) = #"\n" then (String.explode(String.concat(temp))@s,out@["</U>"," "],[0])
    else if hd(s) = #"<" then otherLinkTag(tl(s),out@[" "]@temp,[])
    else if hd(s) = #"[" then otherLink2(tl(s),out@[" "]@temp,[])
    else if hd(s) = #"\\" then  otherEscape(tl(s),out@[" "]@temp,[])
    else if hd(s) = #"\"" then  (tl(s),out@[" "]@temp@["\""],[])
    else underline((tl(s),out,temp@[Char.toString(hd(s))]))
(**)
fun otherThanIB(s,out,temp) = 
    if s=[] then (s,out,[])
    else if hd(s) = #"\t" then (tl(s),out,[])
    else if hd(s) = #"<" then otherLinkTag(tl(s),out,[])
        
        else if hd(s) = #"[" then otherLink2(tl(s),out,[])
                else if hd(s) = #"\\" then  otherEscape(tl(s),out,[])
                    else if hd(s) = #"\"" then  (tl(s),out@["\""],[])
                            else if hd(s) = #"\n" then nl1(tl(s),out@["\n"],[0])
                                else if hd(s) = #"_" then underline(tl(s),out@["<U>"],[])
                                    else (tl(s),out@[Char.toString(hd(s))],[]);


fun otherB2(s,out,temp) = 
    if s=[] then err("Bold didn't end 2.1")
    else if tl(s)=[] then err("Bold didn't end 2.2")
        else if hd(s) = #"*" then if hd(tl(s))= #"*" then (tl(tl(s)),out@["</B>"],[]) else err("bold not closed inside italics")
            else otherB2(otherThanIB(s,out,[]));

fun otherI(s,out,temp) = 
    if s=[] then err("Italics didn't end")
    else if hd(s) = #"*" then 
        if tl(s) = [] then (tl(s),out@["</I>"],[])
        else if hd(tl(s)) = #"*" then 
            otherI(otherB2(tl(tl(s)),out@["<B>"],[]))
        else (tl(s),out@["</I>"],[])


    else otherI(otherThanIB(s,out,[]));

fun otherI2(s,out,temp) = 
    if s=[] then err("Italics didn't end")
    else if hd(s) = #"*" then (tl(s),out@["</I>"],[])

    else otherI2(otherThanIB(s,out,[]));

fun otherB(s,out,temp) = 
    if s=[] then err("Bold didn't end 1")
    else if tl(s)=[] then err("Bold didn't end 2")
        else if hd(s) = #"*" then if hd(tl(s))= #"*" then (tl(tl(s)),out@["</B>"],[]) else otherB(otherI2(tl(s),out@["<I>"],[]))
            else otherB(otherThanIB(s,out,[]));

fun otherIB(s,out,temp) = 
    if s=[] then (s,out,[])
    else if hd(s) = #"*" then otherB(tl(s),out@["<B>"],[])
        else otherI(s,out@["<I>"],[]);
(**)
fun otherThanHeader(s,out,temp) =  
    if s=[] then (s,out,[])
    else if hd(s) = #"\t" then (tl(s),out,[])
    else if hd(s) = #"<" then otherLinkTag(tl(s),out,[])
        
        else if hd(s) = #"[" then otherLink2(tl(s),out,[])
            else if hd(s) = #"*" then otherIB(tl(s),out,[])
                else if hd(s) = #"\\" then  otherEscape(tl(s),out,[])
                    else if hd(s) = #"\"" then  (tl(s),out@["\""],[])
                            else if hd(s) = #"\n" then nl1(tl(s),out@["\n"],[0])
                                else if hd(s) = #"_" then underline(tl(s),out@["<U>"],[])
                                    else (tl(s),out@[Char.toString(hd(s))],[]);

fun header2(s,out,temp) = 
    if s=[] then (s,out@temp,[])
    else
        if hd(s) = #"\n" then (s,out@temp,[0])
        else 
            let val v = otherThanHeader(s,out,[])
            in header2(#1 v,#2 v,temp)
            end;

fun header1(s,out,temp) = 
    if s = [] then
        let
            val x=["H",Int.toString(hd(temp)),">"]
        in
            header2(s,out@["<"]@x,["</"]@x)
        end
    else
        if hd(s) = #"#" then
            if hd(temp) < 6 then
                header1(tl(s),out,[hd(temp)+1]@temp)
            else
                header2(s,out@["<H6>"],["</H6>"])
        else
            let
                val x=["H",Int.toString(hd(temp)),">"]
            in
                header2(s,out@["<"]@x,["</"]@x)
            end;
(**)
fun ul(s,out,temp) = 
    (tl(s),out@["-"],[])
(**)
fun hr2(s,out,temp) =
    if s = [] then 
        (s,out,[])
    else if hd(s) = #"-" then
        hr2(tl(s),out,temp)
    else
        (s,out,temp);


fun hr1(s,out,temp) =
    if s = [] then 
        (s,out,[])
    else 
        if tl(s) = [] then
            (s,out,[])
        else 
            if tl(tl(s))=[] then
                (s,out,[])
            else
                if hd(s) = #"-" then
                    if hd(tl(s)) = #"-" then
                        if hd(tl(tl(s))) = #"-" then
                            hr2(tl(tl(s)),out@["<HR>"],[])
                        else
                            (tl(tl(s)),out@["--"],[])
                    else
                        ul(s,out,[1])
                else
                    (s,out,[]);
(**)
fun other(s,out,temp) = 
    if s=[] then (s,out,[])
    else if hd(s) = #"\t" then (tl(s),out,temp)
    else if hd(s) = #" " then (tl(s),out@[" "],temp)
    else if hd(s) = #"\n" then nl1(tl(s),out@["\n"],[0])
    else if hd(s) = #"<" then otherLinkTag(tl(s),out,[])
        
        else if hd(s) = #"[" then otherLink2(tl(s),out,[])
            else if hd(s) = #"*" then otherIB(tl(s),out,[])
                else if hd(s) = #"\\" then  otherEscape(tl(s),out,[])
                    else if hd(s) = #"\"" then  (tl(s),out@["\""],[])
                        else if hd(s) = #"-" then hr1(s,out,[])
                                else if hd(s) = #"#" then header1(tl(s),out,[1])
                                    else if hd(s) = #"_" then underline(tl(s),out@["<U>"],[]) 
                                        else (tl(s),out@[Char.toString(hd(s))],[]);
(**)
fun table2(s,out,temp) = 
    if s=[] then (s,out,[])
    else if hd(s) = #"|" then table2(tl(s),out@["</TD><TD>"],[])
        else if hd(s) = #"\n" then 
                if tl(s)=[] then 
                    err("table ends before closing >> 1")
                else if hd(tl(s)) = #" " orelse hd(tl(s)) = #"\t" then table2([#"\n"]@tl(tl(s)),out,temp)
                else if hd(tl(s)) = #">" then 
                        if tl(tl(s))=[] then
                            err("table ends before closing >> 2") 
                        else
                            if hd(tl(tl(s))) = #">" then 
                                (tl(tl(tl(s))),out@["</TD></TR>\n</TABLE></CENTER>\n"],[0]) 
                            else err("only single > at end of table") 
                    else table2(tl(s),out@["</TD></TR>\n<TR><TD>"],[])
            else table2(other(s,out,[]));

fun table1(s,out,temp) = 
    if s=[] then (s,out,[])
    else if hd(s) = #"\n" then table2(tl(s),out@["\n<TR><TD>"],[])
        else err("Table doesn't have new line immediately following <<");
(**)
fun s1(s,out,temp) = 
    if s=[] then (s,out,[])
    else if hd(s) = #"<" then table1(tl(s),out@["<CENTER><TABLE border=\"1\">"],[])
        else otherLinkTag(s,out,[]);
(**)


fun bq(s,out,temp) = 
    if s=[] then (s, out@prtbq(1,hd(temp)),[0])
    else if hd(s) = #"\n" then
        let val v = countangle(0,tl(s))
        in
        if #2(v) = [] orelse #1(v) = 0 orelse (#1(v) = 1 andalso hd(#2(v)) = #"\n") then (#2(v), out@prtbq(1,hd(temp)),[0])
        else if hd(temp) <= #1(v) then bq(#2(v),out@prtbq(0,#1(v)-hd(temp)),[#1(v)]@temp)
        else bq(#2(v),out@prtbq(1,hd(temp)- #1(v)),[#1(v)]@temp)
        end
    else
    let val v = other(s,out,[])
    in bq(#1(v),#2(v),temp)
    end;

fun lst(s,out,temp) = 
if s = [] then (s,out@closeall(temp),[])
else if hd(s) = #"\n" then
    if tl(s) = [] then (tl(s),out@closeall(temp),[])
    else
    let val v = countspaces(0,#2(trim(s,[]))@ #1(trim(s,[])))
    in 
    if #1(v) = 0 then
        if hd(#2(v)) = #"-" then lst(tl(#2(v)),out@["</LI><LI>"],temp)
        else if (#1(olsyn(#2(v)))) then lst(#2(olsyn(#2(v))),out@["</LI><LI>"],temp)
        else (tl(s),out@closeall(temp),[])
    else if hd(temp) = #1(v) then 
        if hd(#2(v)) = #"-" then lst(tl(#2(v)),out@["</LI><LI>"],temp)
        else if (#1(olsyn(#2(v)))) then lst(#2(olsyn(#2(v))),out@["</LI><LI>"],temp)
        else 
        let
        val v = other(s,out,[])
    in lst(#1(v),#2(v),temp)
    end
    else if hd(temp) < #1(v) then
        if hd(#2(v)) = #"-" then lst(tl(#2(v)),out@["</LI><UL><LI>"],[#1(v),1]@temp)
        else if (#1(olsyn(#2(v)))) then lst(#2(olsyn(#2(v))),out@["</LI><OL><LI>"],[#1(v),0]@temp)
        else let
        val v = other(s,out,[])
    in lst(#1(v),#2(v),temp)
    end
    else
        let
            val t=newtemp(#1(v),temp)
            val st = closelsts(#1(v),temp)
            val w = ["</LI>"]
        in
            if hd(#2(v)) = #"-" then lst(tl(s),out@w@st@["<LI>"],t)
            else if (#1(olsyn(#2(v)))) then lst(#2(olsyn(#2(v))),out@w@st@["<LI>"],t)
            else let
        val v = other(s,out,[])
    in lst(#1(v),#2(v),temp)
    end
        end
    end
else
let val v = other(s,out,[])
    in lst(#1(v),#2(v),temp)
    end;


fun ul(s,out,temp) =  
if s=[] then (s,out@["</LI></UL>"],temp)
else if hd(s) = #"\n" then 
 if tl(s) = []  then (s,out@["</LI></UL>"],temp)
 else if hd(tl(s)) = #"-" then ul(tl(tl(s)),out@["</LI><LI>"],temp)
 else (tl(s),out@["</LI></UL>"],temp)
else ul(other(s,out,temp));

fun ol(s,out,temp) =  
if s=[] then (s,out@["</LI></OL>"],temp)
else if hd(s) = #"\n" then 
    if tl(s)=[] then (s,out@["</LI></OL>"],temp)
    
    else if (#1(olsyn(tl(tl((s)))))) then ol(#2(olsyn(tl(tl((s))))),out@["</LI><LI>"],temp)
    else if hd(tl(s)) = #" " orelse hd(tl(s)) = #"\t" then ol(other(tl(s),out,temp))
    else (tl(s),out@["</LI></OL>"],temp)
else ol(other(s,out,temp));

(**)
fun writeFile (filename,content) =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end;

fun start(s,out,temp) = 
    let
    val n = [#"0",#"1",#"2",#"3",#"4",#"5",#"6",#"7",#"8",#"9"]
    in
    if s=[] then (s,out,[])
    else if temp = [0] then 
        if hd(s) = #"<" then start(s1(tl(s),out,[]))
            else if hd(s) = #">" then start(bq([#"\n"]@s,out,[0]))
            else if hd(s) = #"-" then  (* assuming that first comment always starts directly on newline without spaces *)
                if tl(s) = [] orelse hd(tl(s)) = #"-" then
                    start(other(s,out,[]))
                else start(lst(tl(s),out@["<UL><LI>"],[0,1]))
            else if #1(olsyn(s)) then start(lst(#2(olsyn(s)) ,out@["<OL><LI>"],[0,0]))
        else start(other(s,out,[]))
    else start(other(s,out,[]))
    end;



fun main() =
    let
        val s=conv("mdtab-2023.mdt")
    in
        writeFile("mdtab-2023.html",String.concat(["<!DOCTYPE html>\n<HTML>\n<HEAD></HEAD>\n<BODY>\n<P>\n"]@ #2(start(s,[],[])) @ ["\n</P>\n</BODY>\n</HTML> "]))
    end;

