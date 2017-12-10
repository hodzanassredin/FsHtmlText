namespace FsHtmlText

module FsHtmlText = 
    open System
    open System.Linq
    open System.Collections.Generic
    //import htmlentitydefs
    //from crawler import bytes_to_text
    let CHARS_LIMIT = 10000
    type State = TAG_NAME=0 | TAG_PARAMS=1 | TAG_END=2 | TEXT = 3 | SKIP = 4 | SYMBOL = 5 | COMMENT = 6
    
    let chars = HashSet(".?!,")
    let chars_and_sp = chars.UnionWith(" ")
    let self_closing_tags = HashSet(["areabase";"br";"col";"command";"embed";"hr";"img";"input";"keygen";"link";"meta";"param";"source";"track";"wbr"])
    let bad_tags = HashSet(["script";"style"; "time"; "label"])
    let e2c = dict((u'%s'%k,eval("u'\\u%04x'"%v)) for k, v in htmlentitydefs.name2codepoint.items())

    let isalnum (str:String) = str.All(fun c -> Char.IsLetterOrDigit(c))

    for k in e2c.Keys do
        if not <| isalnum(e2c.[k])
        then e2c.[k] <- " "
        


    let skip_tags = HashSet(["nav"; "header"; "footer"; "aside"; "code"; "samp"; "form"; "button"; "fieldset"; "nav"; "time"; "noscript"; "var"; "select"; "option"])
    let good_tags = HashSet(["body"; "article"])

    let bad_names = ["banner";"nav";"tabloid";"sidebar";"date";"time";"tags";"comment";"signin";"rating";"count"; "select"]
    let attrs = dict(
        "id", bad_names;
        "class", bad_names;
        "style" , ["none"; "hidden"; "absolute"]
    )
    let is_skip_tag(tag, tag_attrs) =
        if tag in skip_tags
            return True
        if tag in good_tags:
            return False
        for attr in [x for x in attrs if x in tag_attrs]:
            attr_val = tag_attrs[attr]
            for bad in attrs[attr]:
                if bad in attr_val:
                    return True
        return False

    def add_char(lst, chr):
        if chr == "" or chr == u"\uFFFD":
            return False
        is_chr_or_sp = chr in chars_and_sp
        last = lst[-1]
        if is_chr_or_sp and last in chars:
            return False
        else:
            if is_chr_or_sp and last == " ":
                lst[-1] = chr
            else:
                lst.append(chr)

            return True

    def is_escaped(str, str_mark_idx):
         count = 0
         while True:
            str_mark_idx -= 1
            is_esc = str[str_mark_idx] == "\\"
            if str_mark_idx < 0 or not is_esc:
                return count > 0 and count / 2 == 0
            if is_esc:
                count += 1

    def parse(html):
        if isinstance(html, str):
            html = bytes_to_text(html)
        html = html.lower()
        read_str = [""]
        skip_str = [""]
        skip_read = False
        skip_level = 0
        i = 0
        state = TEXT
        skip_idx = 0
        symbol_code = ""
        tag = ""
        tag_attr = ""
        tag_attr_vals = {}
        append = ""
        added_chrs = 0
        html_len = min(200000, len(html))
        level = 0
        
        while i < html_len:

            i_next = i + 1
            current = html[i] 
            if current == "\n" or current == "\t" or current == "\r":
                current = " "

            #try:
            #    print "%6d\t%s\t%2d\t%10s\t%d\t%25s||%-25s" % (i, current, skip_idx, tag, state, html[i-25:i+1].replace("\n"," "), html[i+1:i+25].replace("\n"," "))
            #except:
            #    print "err"    
            #skip strings 
            if (state == TAG_PARAMS or state == SKIP) and current in "\"\'":
                #handle escapes in strings
                if not (tag_attr == "" and state == TAG_PARAMS):
                    while True:
                        i = html.find(current, i + 1)
                        if i == -1:
                            #print "not found end of string in position = %d" % i_next
                            i = i_next # probably better to fail
                            break 
                        if not is_escaped(html,i):
                            if state == TAG_PARAMS:
                                tag_attr_vals[tag_attr] = html[i_next:i]
                                tag_attr = ""
                            break
                else:
                    #bad string in args trying to skip them all
                    i = html.find(">", i + 1)
            #stop skipping only when closing tag found
            elif state == SKIP:
                if current == tag[skip_idx]:
                    if skip_idx == tag_len:
                        state = TEXT
                        skip_idx = 0
                    else:
                        skip_idx = skip_idx + 1
                else:
                    skip_idx = 0
                    tmp = html[i-1] + current
                    if tmp == "//" and tag == "</script>":
                        i = html.find("\n", i + 1)
                        if i == -1:
                            #print "not found end of comment // position = %d %s" % (i_next, html[i_next-10:i_next+20])
                            i = i_next # probably better to fail
                    elif tmp == "/*":
                        i = html.find("*/", i + 1) + 1
                        if i == 0:
                            #print "not found end of comment /* position = %d %s" % (i_next, html[i_next-10:i_next+20])
                            i = i_next # probably better to fail
                    elif current == "/" and tag == "</script>":
                        while True:
                            i = html.find(current, i + 1)
                            if i == -1:
                                #print "not found end of string in position = %d %s" % (i_next, html[i_next-10:i_next+20])
                                i = i_next # probably better to fail
                                break 
                            if not is_escaped(html,i):
                                break
            elif current == "<":
                state = TAG_NAME
                tag = ""
            elif current == "/" and tag == "" and state == TAG_NAME:
                tag = "/"
            elif current == ">" and (state == TAG_NAME or state == TAG_PARAMS):
                #print "".join([" " for igg in range(0, level)]) + tag, "skip" if skip_read else ""
                if tag == "meta" and "name" in tag_attr_vals and "content" in tag_attr_vals and tag_attr_vals["name"] == "description":
                    append = parse(tag_attr_vals["content"]).strip()
                    #print "found meta descr", append
                if tag in bad_tags:
                    state = SKIP
                    tag = "</%s>" % tag
                    tag_len = len(tag)-1
                else:
                    if not tag[0] == "/":
                        not_self_closing = not tag in self_closing_tags
                        if not_self_closing:
                            if not skip_read:    
                                skip_read = is_skip_tag(tag, tag_attr_vals)
                                if skip_read:
                                    skip_level = level
                            level += 1
                    else:
                        level -= 1
                        if skip_read and level == skip_level:
                            skip_read = False
                    state = TEXT
                    if "/" in tag: 
                        append = " " if "span" in tag else "."
                        
                tag_attr_vals = {}
                tag_attr = ""
            elif state == TAG_PARAMS and not (current == " " or current == "="):
                tag_attr += current
            elif state == SYMBOL:
                if current == ";":
                    state = TEXT
                    if symbol_code in e2c:
                        append = e2c[symbol_code]
                    else:
                        try:
                            append = unichr(int(symbol_code[2:], 16))
                        except:
                            append = " "
                else:
                    symbol_code = symbol_code + current
            elif state == TEXT:
                if current == "&":
                    state = SYMBOL
                    symbol_code = ""
                else:
                    append = current
            elif state == TAG_NAME:
                if current == " " or current == "/":
                    if not tag == "":
                        state = TAG_PARAMS
                        tag_attr = ""
                        tag_attrs = {}
                else:
                    tag = tag + current
                    if tag == "!--":
                        i = html.find("-->", i + 1) + 2
                        if i == 1:
                            #print "not found end of comment <|-- position = %d %s" % (i_next, html[i_next-10:i_next+20])
                            i = i_next # probably better to fail
                        #state = TEXT
            
            
            if not skip_read:
                for chr in append:
                    if add_char(read_str, chr):
                        added_chrs += 1
            else:
                for chr in append:
                    if add_char(skip_str, chr):
                        added_chrs += 1
            if added_chrs > CHARS_LIMIT and chr == ".":
                break

            i = i + 1
            append = ""
        rstr = "".join(read_str)
        if len(rstr) > 400:
            return rstr
        return rstr + "".join(skip_str)
