# Markdown-to-HTML
SML program to translate markdown text to corresponding HTML code.

I have made a DFA (flowchart) for the program, please view it at:
<https://miro.com/app/board/uXjVPsbtsy0=/?share_link_id=465739780267>

Provide the input in the file "mdtab-2023.mdt", the output will be written into the file "mdtab-2023.html".

Design decisions:
- lists can not be inside any tag
- b and i (\*, \*\*) if present, have to be present outside the underline (convention)
- header can not be inside b, i, u tags, but the reverse can be done
- direct links and links with address and name specified have been implemented, along with b, i, u, headings
- nested lists (both ordered and unordered) have been implemented
- nested blockquotes have been implemented
- escape sequences for special characters has been implemented
- nesting is based on amount of indentation provided in lists
- nesting in blockquotes is based on number of > characters, starting from newline only
- hardwrapped lines of list elements ms=ust be indented with atleast one space
- inline tags, and asterisks for b and i must end, otherwise error is produced.
- list elements may be separated by newline characters and spaces/tabs

> Since this is a markdown to HTML converter, if an error occurs, I have output only the error text as output in the HTML file
> as this will give the exact error description
Errors pinpointed are:
- Asterisks for bold/italics not closed
- inline tags not closed
- link angle bracket not closed
- incorrect syntax of \[a\](b) links
- (b) missing in \[a\](b) links
- table syntax incorrect.
