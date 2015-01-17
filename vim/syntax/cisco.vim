" Notes {{{
" Vim syntax file
" Language: cisco configuration files
" Version: .622
" 
" Inception: 29-jan-2006
" Inspiration:  Harry Schroeder's original cisco vim syntax file, and a discussion
"               on reddit.com in the vim subreddit on highlighting ip addresses in
"               a syntax file in 2005.  Grown from there.
" Notes:    
"           This does not follow the conventional notion in vim of separate
"           language and color definition files.  That's because cisco
"           configuration syntax, in spite of the misnomer 'code' used when
"           referring to cisco config files, is not a language.  It has no
"           branch logic, data structures, or flow control.  Moreover, in
"           terms purely of syntax, is doesn't have a consistent structure and
"           form.  Configuration lines do not express logic, but rather
"           command parameters.  The chief structure are subcommands, and
"           depending on the command chosen, there are a variable number of
"           nested subcommand mixed with parameters.  Also, I wanted to
"           underline major mode commands like "interface" and underlining
"           isn't really available in all the syntax mode.  Pandoc does it,
"           but depending on the color scheme it's not always there and in
"           general it's confusing to highlight a command or subcomand using
"           lexical tags meant for programming languages.  What's really
"           needed are lexical tags for the cisco command structure.
"
"           Defining switchport configurations, for example, will have a
"           different form depending on whether one is configuring a Dot1q
"           trunk or access port, and if an access port, whether or not a
"           private vlan is to be used.  Also, in cisco configurations,
"           there is only really one major class of syntax elements to be
"           highlighted, keywords, and so highlighting everything the same way
"           loses the chief advantage highlighting confers which is to make it
"           easier to visually separate elements in text i.e. parameters and
"           keywords and other command elements.  So varied colors are used
"           for subcommands and their nested elements to make it easier to
"           visually parse things.
"
"           So rather than define new two files, one for colors of highlighted
"           elements and one for determing which configuration words are
"           catagorized as which highlighting elements, all is in one file, to
"           simplify the process of adding new command/configuration line
"           elements.  To add highlighting for a new configuration line, a new
"           section is added to this file. However, in time a consistent
"           logical heirarchy may emerge as a result of this effort, and a
"           workable list of configuration syntax elements consistent and
"           common across all cisco gear and configurations may come into
"           existance.  At that point it will make sense to split this file
"           out into color theme and syntax definition files.
"
"           Origianly, the purpose was to highlight IP addresses and subnet
"           masks, as those are arguably central to configuring cisco
"           equipment.  Later interfaces were added and accumulated as new
"           ones arose.  While there are a large collection of keywords as in
"           Schroeder's work, a smaller set was chosen and highlighting
"           changed so later keywords in a config line would receive different
"           highlighting.
"
"           The most significant change was added when the Conqueterm plugin
"           became available, and a terminal session could be opened in a vim
"           buffer.  That allowed live terminal sessions to be highlighted
"           using a vim syntax file.  Chiefly, error conditions were given
"           initial attention, mostly in the output of 'show interface'
"           command.  From there other selected elements in command output
"           such as versions, interface status, and other items of interest.
"
"           The foreground colors will as such look the same in any coor
"           scheme, so this will look good on some schemes like solarized, and
"           not as good in others.  In essence, about all switching color
"           schemes will do is change the background color and the color of
"           the default text.
"
"           A quick note about versioning.  Currently, three digits are used
"           past the decimal point, i.e. .621 where .622 indicates minor
"           tweaks to highlighting in the third digit.  Whereas .63 would
"           indicate a new subcommand.  This is not a particularily good
"           versioning system right now, so this will likely change in the
"           near future.
"
" }}}
" Setup {{{
syn clear
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"syn case ignore
setlocal iskeyword+=/
setlocal iskeyword+=:
setlocal iskeyword+=.
setlocal iskeyword+=-

" Define the default hightlighting.

"}}}
" detect and mark gui .vs. term mode set light .vs. dark background color pallets {{{
" gui and 256 terminals will end up with a much richer set of colors.  In the
" 16 color terminal world, colors will that don't exist will just get
" duplicated with the next nearest color - i.e. light red will just be red.
if (has("gui_running"))
    let s:vmode       = " gui"
    if &background == "light"
        let s:white         =   "#ffffff"  " 256 term color 15
        let s:gray          =   "#808080"  " 256 term color 244
        let s:black         =   "#444444"  " 256 term color 238
        let s:parameter     =   "#303030"  " 256 term colot 236
        let s:red           =   "#cd0000"  " 256 term color 1
        let s:orange        =   "#af5f00"  " 256 term color 130
        let s:brown         =   "#875f00"  " 256 term color 94
        let s:yellow        =   "#af8700"  " 256 term color 136
        let s:blue          =   "#0000ff"  " 256 term color 21
        let s:bluegreen     =   "#005f5f"  " 256 term color 23
        let s:cyan          =   "#008787"  " 256 term color 30
        let s:green         =   "#005f00"  " 256 term color 22
        let s:magenta       =   "#5f005f"  " 256 term color 53
        let s:lightmagenta  =   "#ff00ff"  " 256 term color 201
        let s:purple        =   "#5f0087"  " 256 term color 54
        let s:pink          =   "#af005f"  " 256 term color 125
        let s:lime          =   "#5f8700"  " 256 term color 64
        let s:emph_bg       =   "#ffffff"  " 256 term color 15
        let s:lightblue     =   "#005fff"  " 256 term color 27
    else " assume dark background
        let s:white         =   "#eeeeee"  " 256 term color 255
        let s:gray          =   "#8a8a8a"  " 256 term color 246
        let s:black         =   "#303030"  " 256 term color 236
        let s:parameter     =   "#e5e5e5"  " 256 term color 7
        let s:red           =   "#ff0000"  " 256 term color 196
        let s:orange        =   "#ff8700"  " 256 term color 208
        let s:brown         =   "#af8700"  " 256 term color 136
        let s:yellow        =   "#ffff00"  " 256 term color 226
        let s:blue          =   "#0087ff"  " 256 term color 33
        let s:bluegreen     =   "#008787"  " 256 term color 30
        let s:cyan          =   "#00ffff"  " 256 term color 51
        let s:green         =   "#00ff00"  " 256 term color 46
        let s:magenta       =   "#ff00ff"  " 256 term color 201
        let s:lightmagenta  =   "#ff87ff"  " 256 term color 213
        let s:purple        =   "#af5fff"  " 256 term color 135
        let s:pink          =   "#ff5fd7"  " 256 term color 206
        let s:lime          =   "#5fff00"  " 256 term color 82
        let s:emph_bg       =   "#000000"  " 256 term color 0
        let s:lightblue     =   "#00afff"  " 256 term color 39
    endif
else " assume terminal mode
    let s:vmode = " cterm"
    if &t_Co == 256
        if &background == "light"
            let s:white         =   "15"
            let s:gray          =   "244"
            let s:black         =   "238"
            let s:parameter     =   "236"
            let s:red           =   "1"
            let s:orange        =   "130"
            let s:brown         =   "94"
            let s:yellow        =   "136"
            let s:blue          =   "21"
            let s:bluegreen     =   "23"
            let s:cyan          =   "30"
            let s:green         =   "22"
            let s:magenta       =   "53"
            let s:lightmagenta  =   "201"
            let s:purple        =   "54"
            let s:pink          =   "125"
            let s:lime          =   "64"
            let s:emph_bg       =   "15"
            let s:lightblue     =   "27"
        else " assume dark background
            let s:white         =   "255"
            let s:gray          =   "246"
            let s:black         =   "236"
            let s:parameter     =   "7"  
            let s:red           =   "196"
            let s:orange        =   "208"
            let s:brown         =   "136"
            let s:yellow        =   "226"
            let s:blue          =   "33"
            let s:bluegreen     =   "30"
            let s:cyan          =   "51"
            let s:green         =   "46"
            let s:magenta       =   "201"
            let s:lightmagenta  =   "213"
            let s:purple        =   "135"
            let s:pink          =   "206"
            let s:lime          =   "82"
            let s:emph_bg       =   "0"
            let s:lightblue     =   "39"
        endif
    else
        if &background == "light"
            let s:white         =   "15"
            let s:gray          =   "8"
            let s:black         =   "0"
            let s:parameter     =   "0"
            let s:red           =   "1"
            let s:orange        =   "9"
            let s:brown         =   "1"
            let s:yellow        =   "3"
            let s:blue          =   "4"
            let s:bluegreen     =   "6"
            let s:cyan          =   "6"
            let s:green         =   "2"
            let s:magenta       =   "5"
            let s:lightmagenta  =   "5 term=bold"
            let s:purple        =   "12 term=bold"
            let s:pink          =   "5 term=bold"
            let s:lime          =   "2"
            let s:emph_bg       =   "15"
            let s:lightblue     =   "12 term=bold"
        else " assume dark background
            let s:white         =   "15"
            let s:gray          =   "8"
            let s:black         =   "0"
            let s:parameter     =   "15"  
            let s:red           =   "9"
            let s:orange        =   "9 term=bold"
            let s:brown         =   "1 term=bold"
            let s:yellow        =   "11"
            let s:blue          =   "12"
            let s:bluegreen     =   "14"
            let s:cyan          =   "14"
            let s:green         =   "10"
            let s:magenta       =   "13"
            let s:lightmagenta  =   "13 term=bold"
            let s:purple        =   "12 term=bold"
            let s:pink          =   "13 term=bold"
            let s:lime          =   "10"
            let s:emph_bg       =   "0 term=bold"
            let s:lightblue     =   "12 term=bold"
        endif
    endif
endif
" }}}
" Define formatting {{{
let s:none          = s:vmode."=NONE"
let s:ul            = s:vmode."=underline"
let s:underline     = s:vmode."=underline"
let s:rev           = s:vmode."=reverse"
let s:standout      = s:vmode."=standout"
let s:bold          = s:vmode."=bold"
if (has("gui_running"))
    let s:italic        = s:vmode."=italic"
    let s:ul_italic     = s:vmode."=underline,italic"
    let s:italic_rev    = s:vmode."=italic,reverse"
    let s:italic_stand  = s:vmode."=italic,standout"
    let s:bold_italic   = s:vmode."=bold,italic"
else
    let s:italic        = s:vmode."=none"
    let s:ul_italic     = s:vmode."=underline"
    let s:italic_rev    = s:vmode."=reverse"
    let s:italic_stand  = s:vmode."=standout"
    let s:bold_italic   = s:vmode."=bold"
endif
let s:ul_bold       = s:vmode."=underline,bold"
let s:ul_rev        = s:vmode."=underline,reverse"
let s:ul_stand      = s:vmode."=underline,standout"
let s:bold_rev      = s:vmode."=bold,reverse"
let s:bold_stand    = s:vmode."=bold,standout"
" }}}
" background and foreground variables {{{

let s:fgparameter       =   s:vmode . "fg=" . s:parameter       . " "
let s:fgwhite           =   s:vmode . "fg=" . s:white           . " "
let s:fggray            =   s:vmode . "fg=" . s:gray            . " "
let s:fgblack           =   s:vmode . "fg=" . s:black           . " "
let s:fgred             =   s:vmode . "fg=" . s:red             . " "
let s:fgorange          =   s:vmode . "fg=" . s:orange          . " "
let s:fgbrown           =   s:vmode . "fg=" . s:brown           . " "
let s:fgyellow          =   s:vmode . "fg=" . s:yellow          . " "
let s:fgblue            =   s:vmode . "fg=" . s:blue            . " "
let s:fglightblue       =   s:vmode . "fg=" . s:lightblue       . " "
let s:fgbluegreen       =   s:vmode . "fg=" . s:bluegreen       . " "
let s:fgcyan            =   s:vmode . "fg=" . s:cyan            . " "
let s:fggreen           =   s:vmode . "fg=" . s:green           . " "
let s:fgmagenta         =   s:vmode . "fg=" . s:magenta         . " "
let s:fglightmagenta    =   s:vmode . "fg=" . s:lightmagenta    . " "
let s:fgpurple          =   s:vmode . "fg=" . s:purple          . " "
let s:fgpink            =   s:vmode . "fg=" . s:pink            . " "
let s:fglime            =   s:vmode . "fg=" . s:lime            . " "
let s:fgkeyword         =   s:vmode . "fg=" . s:lightblue       . " "

let s:bgparameter       =   s:vmode . "bg=" . s:parameter       . " "
let s:bgwhite           =   s:vmode . "bg=" . s:white           . " "
let s:bggray            =   s:vmode . "bg=" . s:gray            . " "
let s:bgblack           =   s:vmode . "bg=" . s:black           . " "
let s:bgred             =   s:vmode . "bg=" . s:red             . " "
let s:bgorange          =   s:vmode . "bg=" . s:orange          . " "
let s:bgbrown           =   s:vmode . "bg=" . s:brown           . " "
let s:bgyellow          =   s:vmode . "bg=" . s:yellow          . " "
let s:bgblue            =   s:vmode . "bg=" . s:blue            . " "
let s:bglightblue       =   s:vmode . "bg=" . s:lightblue       . " "
let s:bgbluegreen       =   s:vmode . "bg=" . s:bluegreen       . " "
let s:bgcyan            =   s:vmode . "bg=" . s:cyan            . " "
let s:bggreen           =   s:vmode . "bg=" . s:green           . " "
let s:bgmagenta         =   s:vmode . "bg=" . s:magenta         . " "
let s:bglightmagenta    =   s:vmode . "bg=" . s:lightmagenta    . " "
let s:bgpurple          =   s:vmode . "bg=" . s:purple          . " "
let s:bgpink            =   s:vmode . "bg=" . s:pink            . " "
let s:bglime            =   s:vmode . "bg=" . s:lime            . " "
let s:bgemph            =   s:vmode . "bg=" . s:emph_bg         . " "
let s:bgprompt          =   s:vmode . "bg=" . s:lightblue       . " "

" }}}
" Highlighting commands {{{
let s:h = "hi! "
" this may not be in use - check
exe "let s:interface_type = ' ".s:ul." ".s:vmode."fg=".s:cyan." ".s:vmode."bg=".s:none."'"
" }}}
" show interface error conditions {{{
syn match int_errors /[1-9][0-9]* collision[s]\{0,1}/ contained 
syn match int_errors /[1-9][0-9]* runts/ contained
syn match int_errors /[1-9][0-9]* giants/ contained
syn match int_errors /[1-9][0-9]* throttles/ contained
syn match int_errors /[1-9][0-9]* input errors/ contained
syn match int_errors /[1-9][0-9]* CRC/ contained
syn match int_errors /[1-9][0-9]* frame/ contained
syn match int_errors /[1-9][0-9]* overrun/ contained
syn match int_errors /[1-9][0-9]* ignored/ contained
syn match int_errors /[1-9][0-9]* watchdog/ contained
syn match int_errors /[1-9][0-9]* input packets with dribble condition detected/ contained
syn match int_errors /[1-9][0-9]* input discard/ contained
syn match int_errors /[1-9][0-9]* output error[s]\{0,1}/ contained
syn match int_errors /[1-9][0-9]* unknown protocol drops/ contained
syn match int_errors /[1-9][0-9]* babble[s]\{0,1}/ contained
syn match int_errors /[1-9][0-9]* late collision/ contained
syn match int_errors /[1-9][0-9]* deferred/ contained
syn match int_errors /[1-9][0-9]* lost carrier/ contained
syn match int_errors /[1-9][0-9]* no carrier/ contained
syn match int_errors /[1-9][0-9]* no buffer/ contained
syn match int_errors /[1-9][0-9]* input errors*/ contained
syn match int_errors /[1-9][0-9]* short frame/ contained
syn match int_errors /[1-9][0-9]* bad etype drop/ contained
syn match int_errors /[1-9][0-9]* bad proto drop/ contained
syn match int_errors /[1-9][0-9]* if down drop/ contained
syn match int_errors /[1-9][0-9]* input with dribble/ contained
syn match int_errors /[1-9][0-9]* output buffer failures/ contained
syn match int_errors /[1-9][0-9]* underrun/ contained
syn match int_errors /[1-9][0-9]* ignored/ contained
syn match int_errors /[1-9][0-9]* storm suppression/ contained
syn match int_errors /[1-9][0-9]* abort/ contained
syn match int_errors /[eE]rr[Dd]isable[d ]/ contained
exe s:h . " int_errors " . s:bold . s:fgwhite . s:bgred

"}}}
" other show interface info of interest {{{
syn match channel_members /Members in this channel:/ contained keepend
exe s:h . " channel_members " . s:fgmagenta

syn match half_duplex /[Hh]alf-duplex/ contained keepend
exe s:h . " half_duplex " . s:fgwhite . s:bgred

syn match speed /[0-9]\{2,15}Mb\/s/ contained keepend
syn match speed /[0-9]\{2,15} Mb\/s/ contained keepend
syn match speed /[0-9]\{2,15}Gb\/s/ contained keepend
syn match speed /[0-9]\{2,15} Gb\/s/ contained keepend
syn match speed /[0-9]\{2,15} bits\/sec/ contained keepend
exe s:h . " speed " . s:fggreen

syn region media_type excludenl start=/media type is /hs=e+1 end=/$/ keepend contained
exe s:h . " media_type " . s:fgpurple

syn match output_drops /output drops: [1-9][0-9]*/ contained keepend
exe s:h . " output_drops " . s:bold . s:fgwhite . s:bgred

syn match input_output_rate /[0-9]\+ bits\/sec/ contained keepend
exe s:h . " input_output_rate " . s:fgpurple

syn match tx_rx_load /[rt]xload [0-9]\{1,3}\/[0-9]\{1,3}/ contained keepend
exe s:h . " tx_rx_load " . s:fgblue

syn match is_down /is down/ 
exe s:h . " is_down " . s:fgred

syn match is_up /is up/
exe s:h . " is_up " . s:fggreen

syn match int_resets /[1-9][0-9]* interface resets/ contained keepend
exe "hi int_resets " . s:standout . s:fgred

syn cluster show_interface_highlights contains=channel_members,half_duplex,speed,media_type,media_type_desc,output_drops,input_output_rate,tx_rx_load,is_down,is_up,int_resets,ciscodescription

syn match rxtx_pause /[1-9][0-9]* [RT]x pause/ contained keepend containedin=show_interface_output
exe s:h . " rxtx_pause " . s:fgred . s:bgemph

syn match lastclearing /Last clearing of "show interface" counters/ contained containedin=show_interface_output nextgroup=lastclearing_time skipwhite keepend
exe s:h . " lastclearing " . s:italic

syn match lastclearing_time /.*$/ contained keepend
exe s:h . "lastclearing_time" . s:fgorange

syn match output_buffers_swapped_out /[1-9][0-9]* output buffers swapped out/ contained containedin=show_interface_output keepend
exe s:h . "output_buffers_swapped_out" . s:bold . s:fgred

"}}}
" Interface names {{{
syn match ciscointerfacetype excludenl /[A-Za-z\-]\{2,} \{0,1}/               nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ee]th \{0,1}/                        nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ee]thernet \{0,1}/                   nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ff]a \{0,1}/                         nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ff]as \{0,1}/                        nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ff]ast[Ee]thernet \{0,1}/            nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Gg]i \{0,1}/                         nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Gg]ig \{0,1}/                        nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Gg]igabit[Ee]thernet \{0,1}/         nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Tt]e \{0,1}/                         nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Tt]en \{0,1}/                        nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Tt]en[Gg]i \{0,1}/                   nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Tt]en[Gg]igabit[Ee]thernet \{0,1}/   nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Dd]ot11[Rr]adio \{0,1}/              nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ss]er \{0,1}/                        nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ss]erial \{0,1}/                     nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ll]o \{0,1}/                         nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Ll]oopback \{0,1}/                   nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Tt]un \{0,1}/                        nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Tt]unnel \{0,1}/                     nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Pp]o \{0,1}/                         nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Pp]ort.[cC]hannel \{0,1}/            nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Vv]l \{0,1}/                         nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Vv]lan \{0,1}/                       nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /[Pp][Vv]lan \{0,1}/                   nextgroup=ciscointerfacenumber skipwhite contained keepend
syn match ciscointerfacetype excludenl /Null0/ contained keepend
exe s:h . "ciscointerfacetype" . s:ul . s:fgcyan

syn match ciscointerfacenumber excludenl /\d\{1,4}/ contained nextgroup=interfacenumberafterslash skipwhite keepend
exe s:h . "ciscointerfacenumber" . s:ul . s:fgyellow

syn match interfacenumberafterslash excludenl /\/\d\{1,2}\/\{0,1}\d\{0,2}/ contained nextgroup=ciscosubinterface skipwhite keepend
exe s:h . "interfacenumberafterslash"  . s:ul . s:fgyellow

syn match ciscosubinterface excludenl /[:.]\{0,1}\d\{0,3}/ contained keepend
exe s:h . "ciscosubinterface" . s:fgorange

syn region ciscointregion excludenl start="\v[eE]th {0,1}\d{-1,2}[^0-9a-zA-Z]"               end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[eE]thernet {0,1}\d{-1,2}"                      end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[fF]as{0,1} {0,1}\d{-1,2}[^0-9a-zA-Z]"          end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[fF]ast[eE]thernet {0,1}\d{-1,2}"               end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[gG]ig{0,1} {0,1}\d{-1,2}[^0-9a-zA-Z]"          end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[gG]igabit[eE]thernet {0,1}\d{-1,2}"            end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[tT]en{0,1} {0,1}\d{-1,2}[^0-9a-zA-Z]"          end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[tT]en[gG]i {0,1}\d{-1,2}"                      end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[tT]en[gG]igabit[eE]thernet {0,1}\d{-1,2}"      end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[Dd]ot11[Rr]adio {0,1}\d{-1,2}"                 end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[pP]o {0,1}\d{-1,4}[^0-9a-zA-Z]"                end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[pP]ort.[Cc]hannel {0,1}\d{-1,4}"               end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[vV]lan {0,1}\d{-1,4}"                          end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[vV]l {0,1}\d{-1,4}"                            end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[pP][vV]lan {0,1}\d{-1,2}"                      end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[lL]oopback {0,1}\d{-1,2}"                      end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[tT]unnel {0,1}\d{-1,2}"                        end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[sS]erial {0,1}\d{-1,2}"                        end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[lL]o {0,1}\d{1,4}[^0-9a-zA-Z]"                 end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[tT]un {0,1}\d{1,4}[^0-9a-zA-Z]"                end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="\v[sS]er {0,1}\d{1,4}[^0-9a-zA-Z]"                end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype
syn region ciscointregion excludenl start="Null0"                                            end=/$/ end="[,-:]\|\s" transparent contains=ciscointregion,ciscointerfacetype

syn region dont_highlight excludenl start=/\v[a-zA-Z0-9][Tt]e {0,1}\d{-1,2}/                 end=/$/ end="[,:]\|\s" contains=nohighlight
syn region dont_highlight excludenl start=/\v[a-zA-Z0-9][Gg]i {0,1}\d{-1,2}/                 end=/$/ end="[,:]\|\s" contains=nohighlight
syn match nohighlight /.*/ contained keepend

"}}}
" show interface region {{{

syn region show_interface_output start="^[^ ]* is up" start="^[^ ]* is down" end=/[\r]\[^ ]{-1,32}#/ end=/[\r]\[^ ]{-1,32}/ end=/^[^ ]\{-1,32}#/ end=/^[^ ]\{-1,32}/  extend transparent contains=ciscoip,int_errors,@show_interface_highlights,vrf,hash_prompt,ciscointregion,ciscodisable,connected,notconnect,dont_highlight,ethernet_address_colon_dash,ethernet_address,ipaddress

"}}}
" interface config highlighting region {{{

syn match ciscointerface /^int / contained nextgroup=ciscointerfacetype skipwhite keepend
syn match ciscointerface /^interface / contained nextgroup=ciscointerfacetype skipwhite keepend
syn match ciscointerface /^Interface:/ contained nextgroup=ciscointerfacetype skipwhite keepend
exe s:h . "ciscointerface" . s:ul . s:fgblue

syn region interfaceregion  excludenl start="^int[e]\{,1}[r]\{,1}[f]\{,1}[a]\{,1}[c]\{,1}[e]\{,1}" end=".$" transparent keepend contains=ciscointerface

"}}}
" show cdp neighbor  {{{

syn match DeviceID_text /[^ ]/ contained keepend
exe s:h . "DeviceID_text" . s:fgcyan

"syn match DeviceID_Serial /(.*)/

syn match DeviceID_kw /Device ID:/
exe s:h . "DeviceID_kw" . s:fgblue

syn region DeviceID excludenl start="Device ID:" end=".$" contains=DeviceID_kw,DeviceID_Serial,DeviceID_text keepend transparent excludenl

syn match SystemName_text /[^ ]/ contained keepend
exe s:h . "SystemName_text" . s:fgcyan

syn match SystemName_KW /System Name:/ contained keepend
exe s:h . "SystemName_KW" . s:fgblue

syn region SystemName excludenl start="System Name:" end=".$" contains=SystemName_KW,SystemName_text keepend transparent excludenl

"}}}
" Misc Global Keywords {{{
"syntax match ciscono /no .*/

syn match connected /connected/ 
exe s:h . "connected" . s:fggreen

syn match notconnect /not *connect/
syn match notconnect /not *connec[t]\{,1}[e]\{,1}[d]\{,1}/
syn match notconnect /secViolEr/
syn match notconnect /errDisable/
exe s:h . "notconnect" . s:rev . s:fgred

syn match ciscodisable /disable[d]/
exe s:h . "ciscodisable" . s:bold_rev . s:fgorange

syn region vlan_list_reg start=/^vlan [0-9]\{-1,4},/ end="$" contains=vlan_kw,vlan_number transparent keepend
syn match vlan_kw /vlan/ contained
exe s:h . "vlan_kw" . s:fgcyan

syn match vlan_number /\d\{1,4}/ contained
exe s:h . "vlan_number" . s:fgparameter

syn match hostname_keyword /hostname / nextgroup=hostname
exe s:h . "hostname_keyword" . s:fgblue

syn match hostname /.*/ contained
exe s:h . "hostname" . s:bold . s:fgorange

syn match name_keyword / name / nextgroup=name_text
syn match name_keyword /^name / nextgroup=name_text
exe s:h . "name_keyword" . s:fgblue

syn match name_text /.*/ contained
exe s:h . "name_text" . s:fgorange

syn match version_keyword /[vV]ersion/ nextgroup=version_number
exe s:h . "version_keyword" . s:fgblue

syn match version_number /.\+/ contained
exe s:h . "version_number" . s:fgorange 

syn match feature_keyword /feature / nextgroup=feature
exe s:h . "feature_keyword" . s:fgblue

syn match feature /.*/ contained
exe s:h . "feature" . s:fgorange 

syn match permit_statement /permit/
exe s:h . "permit_statement" . s:fggreen 

syn match deny_statement /deny/
exe s:h . "deny_statement" . s:fgred 

syn match no_shutdown /no shut/
syn match no_shutdown /no shutdown/
exe s:h . "no_shutdown" . s:bold . s:fggreen 

syn match match_any_keyword /match-any/ nextgroup=match_any_text
exe s:h . "match_any_keyword" . s:fgblue 

syn match match_any_text /.*/ contained
exe s:h . "match_any_text" . s:fgorange 

syn match keyword2 / location/ nextgroup=name_text skipwhite
syn match keyword2 /contact/ nextgroup=name_text skipwhite
exe s:h . "keyword2" . s:fgblue 

syn match boot_system_flash_phrase /boot system flash / nextgroup=boot_image_name skipwhite
exe s:h . "boot_system_flash_phrase" . s:fgblue 

syn match boot_image_name /.*/ contained
exe s:h . "boot_image_name" . s:fgorange 


"}}}
" show vlan region {{{
"syntax match vlannumber /^[0-9]\{1,4}/ contained nextgroup=vlanname
"HiLink    vlannumber       Keyword
"syntax match vlanname /[a-zA-Z]\{1,32}/ contained
"HiLink    vlanname         Repeat  
"syntax region showvlan start="sh.*vl" end="^[^ ]\{1,63}#" end=/[\r]\{1,63}\#/ contains=vlannumber,ciscointerfacetype,more,ciscointregion,hash_prompt
"}}}
" Interface Description and comment highlighting {{{

syn match ciscodescription /[dD]escription[:]\{0,1}/ nextgroup=ciscodescriptiontext skipwhite
exe s:h . "ciscodescription" . s:fgblue 

syn match ciscodescriptiontext /.*$/ contained keepend
exe s:h . "ciscodescriptiontext" . s:italic . s:fgbrown

syn match commenttext /.*$/ contained keepend
exe s:h . "commenttext"  . s:italic . s:fggray 

syn region comment excludenl start=/\!/ end=/$/ contains=commenttext keepend transparent

"}}}
" route-map highlighting {{{
syn match routemap_match_WORD /.*/ contained
hi routemap_match_WORD ctermfg=darkyellow guifg=orange

syn match routemap_match_kw1 /address/                contained 
syn match routemap_match_kw1 /access-group/           contained 
syn match routemap_match_kw1 /name/                   contained skipwhite nextgroup=routemap_match_WORD
syn match routemap_match_kw1 /next-hop/               contained 
syn match routemap_match_kw1 /route-source/           contained 
syn match routemap_match_kw1 /prefix-list/            contained 
syn match routemap_match_kw1 /redistribution-source/  contained 
syn match routemap_match_kw1 /multicast/              contained 
syn match routemap_match_kw1 /unicast/                contained 
syn match routemap_match_kw1 /level-1/                contained 
syn match routemap_match_kw1 /level-2/                contained 
syn match routemap_match_kw1 /local/                  contained 
syn match routemap_match_kw1 /nssa-external/          contained 
syn match routemap_match_kw1 /external/               contained 
syn match routemap_match_kw1 /internal/               contained 
syn match routemap_match_kw1 /bgp/                    contained 
syn match routemap_match_kw1 /connected/              contained 
syn match routemap_match_kw1 /eigrp/                  contained 
syn match routemap_match_kw1 /isis/                   contained 
syn match routemap_match_kw1 /mobile/                 contained 
syn match routemap_match_kw1 /ospf/                   contained 
syn match routemap_match_kw1 /rip /                   contained 
syn match routemap_match_kw1 /static/                 contained 
syn match routemap_match_kw1 /as-path/                contained 
syn match routemap_match_kw1 /cln /                   contained 
syn match routemap_match_kw1 /community/              contained 
syn match routemap_match_kw1 /extcommunity/           contained 
syn match routemap_match_kw1 /interface/              contained 
syn match routemap_match_kw1 /ip /                    contained 
syn match routemap_match_kw1 /ipv6/                   contained 
syn match routemap_match_kw1 /length/                 contained 
syn match routemap_match_kw1 /local-preference/       contained 
syn match routemap_match_kw1 /metric/                 contained 
syn match routemap_match_kw1 /mpls-label/             contained 
syn match routemap_match_kw1 /nlri/                   contained 
syn match routemap_match_kw1 /policy-list/            contained 
syn match routemap_match_kw1 /route-type/             contained 
syn match routemap_match_kw1 /source-protocol/        contained 
syn match routemap_match_kw1 /tag /                   contained 
exe s:h . "routemap_match_kw1" . s:fgblue

syn match routemap_match_kw /match / contained
exe s:h . "routemap_match_kw" . s:fgkeyword

syn region routemap_match_region matchgroup=routemap_match_kw start=/[ ]*[ ]\+match / end=/$/ transparent contains=routemap_match_kw1

"}}}
" Cisco vrf highlighting {{{
syn match vrf_kw /vrf/ contained nextgroup=vrf_keyword skipwhite
exe s:h . "vrf_kw" . s:fgkeyword

syn match vrf_keyword /context/     contained nextgroup=vrf_name skipwhite
syn match vrf_keyword /member/      contained nextgroup=vrf_name skipwhite
syn match vrf_keyword /forwarding/  contained nextgroup=vrf_name skipwhite
exe s:h . "vrf_keyword" . s:fgpurple

syn match vrf_name /.*/ contained
exe s:h . "vrf_name" . s:bold . s:fgred

syn region vrf_region start="vrf context" start="vrf forwarding" start="vrf member" end="$" keepend transparent contains=vrf_kw, vrf_keyword

"}}}
" switchport command {{{
" This is a good example of why cisco highlighting can't really be done well
" in the context of highlighting a conventional programming language.  One
" could simply re-use keyword, preproc, repeat, and so forth, but it's more
" straightforward to just say "purple", blue, green, and so forth.  That said,
" the highlighting variable "s:fgkeyword" is used in places, but not to the
" extent of "s:fgkeyword1,2,3,4" and so on.  That's one approach that would
" make it easier to have custom color schemes, but that also doesn't fit the
" paradigm of highlighting a conventional programming language.
"
" TODO  make these local to each subgroup, even at the expense of defining
"       the same pattern with different names
syn match switchport_kw_err "\v[^ ]+" contained
exe s:h . "switchport_kw_err" . s:rev . s:fgred

syn match encapsulation_tag /\v[0-9]{1,4}/ contained
syn match encapsulation_tag /ethertype/ contained
exe s:h . "encapsulation_tag" . s:fglightmagenta

syn match switchport_keyword /switchport/
exe s:h . "switchport_keyword" . s:fgkeyword

" the base set following the root.
" TODO  each should get its own subregion
syn match switchport_base_kwds /access/         contained
syn match switchport_base_kwds /autostate/      contained skipwhite nextgroup=switchport_autostate_kw,switchport_kw_err
syn match switchport_base_kwds /backup/         contained skipwhite nextgroup=switchport_backup_kw,switchport_kw_err
syn match switchport_base_kwds /block/          contained skipwhite nextgroup=switchport_block_kw,switchport_kw_err
syn match switchport_base_kwds /capture/        contained
syn match switchport_base_kwds /dot1q/          contained
syn match switchport_base_kwds /host/           contained
syn match switchport_base_kwds /mode/           contained
syn match switchport_base_kwds /monitor/        contained
syn match switchport_base_kwds /trunk/          contained
syn match switchport_base_kwds /port-security/  contained
syn match switchport_base_kwds /private-vlan/   contained
syn match switchport_base_kwds /block/          contained
syn match switchport_base_kwds /priority/       contained
exe s:h . "switchport_base_kwds" . s:fgpurple

"syn region switchport_conf_command matchgroup=switchport_keyword start=/switchport/rs=e+1 end=/$/ contains=@switchport_regions,@switchport_kwds skipwhite keepend excludenl transparent
syn region switchport_command matchgroup=switchport_keyword start=/switchport/rs=e+1 end=/$/ contains=switchport_base_kwds,switchport_kw_err,ethernet_address skipwhite keepend excludenl transparent

" switchport command nextgroups {{{2

syn match switchport_autostate_kw /exclude/ contained containedin=switchport_base_kwds
exe s:h . "switchport_autostate_kw" . s:fgred 

syn match switchport_backup_kw /interface/ contained containedin=switchport_base_kwds skipwhite nextgroup=ciscointerfacetype
exe s:h . "switchport_backup_kw" . s:fgred 

syn match switchport_block_kw /unicast/ contained
syn match switchport_block_kw /multicast/ contained
exe s:h . "switchport_block_kw" . s:fgred 
hi switchport_block_kw ctermfg=red guifg=darkorange

"}}}
" switchport dot1q ethertype {{{2

syn match switchport_dot1q_kw /[dD]ot1[qQ]/ contained containedin=switchport_dot1q_ethertype_region
exe s:h . "switchport_dot1q_kw" . s:fgorange 

syn match switchport_dot1q_ethertype_kw /ethertype/ contained containedin=switchport_dot1q_ethertype_region skipwhite nextgroup=switchport_dot1q_ethertype_value
exe s:h . "switchport_dot1q_ethertype_kw" . s:fgmagenta 

syn match switchport_dot1q_ethertype_value /0x[6-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]/ contained containedin=switchport_dot1q_ethertype_kw
exe s:h . "switchport_dot1q_ethertype_value" . s:fgred 

syn region switchport_dot1q_ethertype_region start=/[dD]ot1[qQ] ethertype/ end=/$/ contains=switchport_kw_err transparent keepend contained containedin=switchport_command
"}}}
" switchport capture allowed vlan {{{2

syn match switchport_capture_allowed_vlan /add/     contained containedin=switchport_capture_allowed_vlan_region skipwhite nextgroup=switchport_capture_allowed_vlan_list
syn match switchport_capture_allowed_vlan /except/  contained containedin=switchport_capture_allowed_vlan_region skipwhite nextgroup=switchport_capture_allowed_vlan_list
syn match switchport_capture_allowed_vlan /remove/  contained containedin=switchport_capture_allowed_vlan_region skipwhite nextgroup=switchport_capture_allowed_vlan_list
syn match switchport_capture_allowed_vlan /all/     contained containedin=switchport_capture_allowed_vlan_region
exe s:h . "switchport_capture_allowed_vlan" . s:fgorange 

syn match switchport_capture_allowed_vlan_kw /vlan/ contained containedin=switchport_capture_allowed_vlan_region
exe s:h . "switchport_capture_allowed_vlan_kw" . s:fgmagenta

syn match switchport_capture_allowed_kw /allowed/ contained containedin=switchport_capture_allowed_vlan_region
exe s:h . "switchport_capture_allowed_kw" . s:fgkeyword 

syn match switchport_capture_allowed_vlan_list /[0-9,-]\+/ contained containedin=switchport_capture_allowed_vlan
exe s:h . "switchport_capture_allowed_vlan_list" . s:fgparameter

syn region switchport_capture_allowed_vlan_region matchgroup=switchport_base_kwds start=/capture allowed vlan/rs=e-13 end=/$/ contains=switchport_kw_err skipwhite transparent keepend contained containedin=switchport_command
"}}}
" switchport access {{{2

syn match switchport_conf_access_vlan_kw /vlan/ skipwhite contained containedin=switchport_conf_access nextgroup=switchport_conf_access_kw_WORDS
exe s:h . "switchport_conf_access_vlan_kw" . s:fgorange 

syn match switchport_conf_access_kw_WORDS /[0-9]\+/ contained 
exe s:h . "switchport_conf_access_kw_WORDS" . s:fgparameter 

syn region switchport_conf_access matchgroup=switchport_base_kwds start=/access/rs=e end=/$/ contains=switchport_kw_err skipwhite keepend transparent contained containedin=switchport_command

"}}}
" switchport trunk {{{2

syn match switchport_trunk_kwds /encapsulation/ contained skipwhite containedin=switchport_trunk nextgroup=switchport_trunk_encap_kwds,switchport_kw_err
syn match switchport_trunk_kwds /ethertype/     contained skipwhite containedin=switchport_trunk nextgroup=switchport_trunk_ethertype_value
syn match switchport_trunk_kwds /pruning/       contained skipwhite containedin=switchport_trunk nextgroup=switchport_trunk_pruning_vlan_list
exe s:h . "switchport_trunk_kwds" . s:fgorange

syn match switchport_trunk_encap_kwds /[Dd]ot1[qQ]/ contained containedin=switchport_trunk_kwds
syn match switchport_trunk_encap_kwds /[iI][sS][lL]/ contained containedin=switchport_trunk_kwds
syn match switchport_trunk_encap_kwds /negotiate/ contained containedin=switchport_trunk_kwds
exe s:h . "switchport_trunk_encap_kwds" . s:fgparameter 

syn match switchport_trunk_native_vlan_ID /\d\{-1,4}/ contained containedin=switchport_trunk_kwds
syn match switchport_trunk_native_vlan_ID /tag/       contained containedin=switchport_trunk_kwds
exe s:h . "switchport_trunk_native_vlan_ID" . s:fgparameter 

syn match switchport_trunk_ethertype_value /0x[05-9a-fA-F][e-fE-F]\{0,1}[fF]\{0,1}[fF]\{0,1}/ contained containedin=switchport_trunk_kwds
exe s:h . "switchport_trunk_ethertype_value" . s:fgred 

syn match switchport_trunk_pruning_vlan_list /[0-9,-]\+/ contained containedin=switchport_trunk_kwds
exe s:h . "switchport_trunk_pruning_vlan_list" . s:fgparameter

syn region switchport_trunk matchgroup=switchport_base_kwds start=/trunk/rs=e+1 end=/$/ contains=switchport_kw_err skipwhite keepend transparent contained containedin=switchport_command
"}}}
" switchport trunk allowed vlan {{{2
syn match switchport_trunk_allowed_vlan_kwds /add/      skipwhite contained containedin=switchport_trunk_allowed_vlan_region nextgroup=switchport_trunk_allowed_vlan_list
syn match switchport_trunk_allowed_vlan_kwds /all/      skipwhite contained containedin=switchport_trunk_allowed_vlan_region
syn match switchport_trunk_allowed_vlan_kwds /except/   skipwhite contained containedin=switchport_trunk_allowed_vlan_region nextgroup=switchport_trunk_allowed_vlan_list
syn match switchport_trunk_allowed_vlan_kwds /remove/   skipwhite contained containedin=switchport_trunk_allowed_vlan_region nextgroup=switchport_trunk_allowed_vlan_list
syn match switchport_trunk_allowed_vlan_kwds /none/     contained
exe s:h . "switchport_trunk_allowed_vlan_kwds" . s:fgmagenta

syn match switchport_trunk_allowed_vlan_list /\v[0-9,-]+/ contained containedin=switchport_trunk_allowed_vlan_kwds,switchport_trunk_allowed_vlan_region
exe s:h . "switchport_trunk_allowed_vlan_list" . s:fgparameter

syn match switchport_trunk_allowed_kw /allowed/ contained containedin=switchport_trunk_allowed_vlan_region
exe s:h . "switchport_trunk_allowed_kw" . s:fgkeyword

syn match switchport_trunk_allowed_vlan_kw /vlan/ contained containedin=switchport_trunk_allowed_vlan_region
exe s:h . "switchport_trunk_allowed_vlan_kw" . s:fgorange
hi switchport_trunk_allowed_vlan_kw ctermfg=darkyellow guifg=orange

syn region switchport_trunk_allowed_vlan_region start=/allowed/ end=/$/ contains=switchport_kw_err skipwhite transparent keepend contained containedin=switchport_trunk,switchport_command

"}}}
" switchport trunk native vlan {{{2

syn match switchport_trunk_native_vlan_list /\v[0-9,-]+/ contained containedin=switchport_trunk_native_vlan_kwds,switchport_trunk_native_vlan_region
exe s:h . "switchport_trunk_native_vlan_list" . s:fgparameter

syn match switchport_trunk_native_kw /native/ contained containedin=switchport_trunk_native_vlan_region
exe s:h . "switchport_trunk_native_kw" . s:fgkeyword

syn match switchport_trunk_native_vlan_kw /vlan/ contained containedin=switchport_trunk_native_vlan_region
exe s:h . "switchport_trunk_native_vlan_kw" . s:fgorange

syn region switchport_trunk_native_vlan_region start=/native/ end=/$/ contains=switchport_kw_err skipwhite transparent keepend contained containedin=switchport_trunk,switchport_command

"}}}
" switchport mode {{{2
syn match switchport_mode_kwds /access/             contained containedin=switchport_kw_err
syn match switchport_mode_kwds /dot1q\-tunnel/      contained containedin=switchport_kw_err
syn match switchport_mode_kwds /fex\-fabric/        contained containedin=switchport_kw_err
syn match switchport_mode_kwds /trunk/              contained containedin=switchport_kw_err
syn match switchport_mode_kwds /dynamic/            contained containedin=switchport_kw_err
syn match switchport_mode_kwds /dot1[qQ]\-tunnel/   contained containedin=switchport_kw_err
syn match switchport_mode_kwds /private-vlan/       contained containedin=switchport_kw_err
exe s:h . "switchport_mode_kwds" . s:fgmagenta

syn region switchport_mode matchgroup=switchport_base_kwds start=/mode/rs=e end=/$/ contains=switchport_kw_err skipwhite keepend transparent contained containedin=switchport_command
" }}}
" switchport mode private-vlan {{{2

syn match switchport_mode_privatevlan_kwds /host/               contained containedin=switchport_privatevlan
syn match switchport_mode_privatevlan_kwds /promiscuous/        contained containedin=switchport_privatevlan
syn match switchport_mode_privatevlan_kwds /mapping/            contained skipwhite containedin=switchport_privatevlan nextgroup=switchport_privatevlan_1
syn match switchport_mode_privatevlan_kwds /host\-association/  contained skipwhite containedin=switchport_privatevlan nextgroup=switchport_privatevlan_1
syn match switchport_mode_privatevlan_kwds /native/  contained skipwhite containedin=switchport_privatevlan nextgroup=switchport_privatevlan_1
syn match switchport_mode_privatevlan_kwds /allowed/  contained skipwhite containedin=switchport_privatevlan nextgroup=switchport_privatevlan_1
syn match switchport_mode_privatevlan_kwds /vlan/  contained skipwhite containedin=switchport_privatevlan nextgroup=switchport_privatevlan_1
syn match switchport_mode_privatevlan_kwds /trunk/  contained skipwhite containedin=switchport_privatevlan nextgroup=switchport_privatevlan_1
exe s:h . "switchport_mode_privatevlan_kwds" . s:fggreen
hi switchport_mode_privatevlan_kwds ctermfg=darkgreen guifg=green

syn match switchport_privatevlan_1 /\v[0-9-]+/ contained containedin=switchport_mode_privatevlan_kwds skipwhite nextgroup=switchport_privatevlan_2
exe s:h . "switchport_privatevlan_1" . s:fgorange

syn match switchport_privatevlan_2 /\v[0-9-]+/ contained
exe s:h . "switchport_privatevlan_2" . s:fgred
hi switchport_privatevlan_2 ctermfg=red guifg=red gui=italic

syn region switchport_privatevlan matchgroup=switchport_base_kwds start=/private-vlan/rs=e+1 end=/$/ contains=switchport_kw_err skipwhite transparent keepend contained containedin=switchport_command,switchport_mode

" }}}
" switchport block {{{2
syn match switchport_block_kw /unicast/     contained containedin=switchport_block
syn match switchport_block_kw /multicast/   contained containedin=switchport_block
exe s:h . "switchport_block_kw" . s:fgorange
hi switchport_block_kw ctermfg=darkyellow

syn region switchport_block matchgroup=switchport_base_kwds start=/block/rs=e end=/$/ contains=switchport_kw_err skipwhite transparent keepend contained containedin=switchport_command
"}}}
"switchport priority {{{2

syn match switchport_priority_extend_kw /extend/    contained containedin=switchport_priority nextgroup=switchport_priority_extend_words
exe s:h . "switchport_priority_extend_kw" . s:fgorange

syn match switchport_priority_extend_words /trust/  contained containedin=switchport_priority 
syn match switchport_priority_extend_words /[0-7]/  contained containedin=switchport_priority
syn match switchport_priority_extend_words /cos/    contained  containedin=switchport_priority nextgroup=switchport_extent_cos_values
exe s:h . "switchport_priority_extend_words" . s:fgred

syn match switchport_extent_cos_values /[0-9]\{-1,3}/ contained containedin=switchport_priority_extend_words
exe s:h . "switchport_extent_cos_values" . s:fgparameter

syn region switchport_priority matchgroup=switchport_base_kwds start=/priority/rs=e+1 end=/$/ contains=switchport_kw_err contained transparent keepend skipwhite containedin=switchport_command

"}}}
" switchport port-security {{{2

syn match switchport_port_security_kw /port-security/ skipwhite contained containedin=switchport_port_security
exe s:h . "switchport_port_security_kw" . s:fgpurple

syn match port_security_mac_addr /mac-address/ skipwhite contained containedin=switchport_port_security contains=ethernet_address
exe s:h . "port_security_mac_addr" . s:fgorange

syn match port_security_max /maximum/ skipwhite contained containedin=switchport_port_security
exe s:h . "port_security_max" . s:fgorange

syn match port_security_violation /violation/ skipwhite contained containedin=switchport_port_security
exe s:h . "port_security_violation" . s:fgorange

syn match port_security_aging /aging/ skipwhite contained containedin=switchport_port_security
exe s:h . "port_security_aging" . s:fgorange

syn match port_security_mac_addr_sticky /sticky/ skipwhite contained containedin=port_security_mac_addr,switchport_port_security
exe s:h . "port_security_mac_addr_sticky" . s:fgparameter

syn match port_security_violation_setting /shutdown/ skipwhite contained containedin=port_security_violation,switchport_port_security
syn match port_security_violation_setting /protect/ skipwhite contained containedin=port_security_violation,switchport_port_security
syn match port_security_violation_setting /restrict/ skipwhite contained containedin=port_security_violation,switchport_port_security
exe s:h . "port_security_violation_setting" . s:ul . s:fgparameter
"hi port_security_violation_setting ctermfg=white cterm=underline guifg=white gui=bold,italic,underline

syn match port_security_maximum_setting /\v\d{-1,4}/ skipwhite contained containedin=port_security_max,switchport_port_security
exe s:h . "port_security_maximum_setting" . s:fgparameter

syn match port_security_maximum_vlan_kw /vlan/ skipwhite contained containedin=port_security_max,switchport_port_security nextgroup=port_security_maximum_vlans
exe s:h . "port_security_maximum_vlan_kw" . s:fgorange

syn match port_security_maximum_vlans /\v[0-9,-]+/ skipwhite contained containedin=port_security_max,switchport_port_security
exe s:h . "port_security_maximum_vlans" . s:italic . s:fgparameter


syn region switchport_port_security start=/port-security/ end=/$/ contains=ethernet_address,switchport_kw_err skipwhite transparent keepend contained containedin=switchport_command

" }}}
"}}}
" encapsulation command {{{
"

syn match encapsulation_error /\v[^ ]+/ contained
exe s:h . "encapsulation_error" . s:rev . s:fgred

syn match encapsulation_kw /encapsulation/ contained containedin=encapsulation_command skipwhite nextgroup=encapsulation_type
exe s:h . "encapsulation_kw" . s:fgblue

" encapsulation_tag was defined in switchport command - re-using it in this region
syn match encapsulation_type /dot1[Qq]/ contained skipwhite nextgroup=encapsulation_tag
exe s:h . "encapsulation_type" . s:fgmagenta

"syn region encapsulation_command excludenl start=/encapsulation / end=/$/ contains=encapsulation_error keepend transparent
syn region encapsulation_command excludenl start=/encapsulation / end=/$/ keepend transparent

" }}}
" ip {{{
syn region ip excludenl start=/^ip / end=/$/ keepend transparent oneline

syn match ip_route /route/ skipwhite contained containedin=ip nextgroup=ip_route_vrf
exe s:h . "ip_route" . s:fgbrown

syn match ip_route_vrf /vrf/ skipwhite contained containedin=ip_route nextgroup=ip_route_vrf_name skipwhite
exe s:h . "ip_route_vrf" . s:bold . s:fgcyan

syn match ip_route_vrf_name / [^ ]\+/ms=s+1 contained containedin=ip_route_vrf skipwhite
exe s:h . "ip_route_vrf_name" . s:none . s:fgbluegreen

syn cluster follows_ip_route contains=ipaddr,ip_route_vrf

syn match route_name_kw /name / skipwhite contained containedin=ip nextgroup=route_name_text
exe s:h . "route_name_kw" . s:fgbrown

syn match route_name_text /.*/ skipwhite contained
exe s:h . "route_name_text" . s:fgbluegreen

syn match ip_address /address/ skipwhite contained containedin=ip
exe s:h . "ip_address" . s:fgparameter

" }}}
" prefix-list {{{
syn match prefix_name / [^ ]\+/ skipwhite contained containedin=prefix_list_kw
exe s:h . "prefix_name" . s:fgbluegreen

syn match prefix_list_kw /prefix-list/ contained containedin=ip nextgroup=prefix_name skipwhite
exe s:h . "prefix_list_kw" . s:fgkeyword

syntax cluster prefix_list contains=prefix_name,prefix_list_kw

syn match seq /seq/ skipwhite contained transparent containedin=ip nextgroup=seqnum
exe s:h . "seq" . s:fgblue

syn match seqnum /\v\d{1,5}/ skipwhite contained containedin=seq
exe s:h . "seqnum" . s:fggreen

"}}}
" Ethernet address {{{

syn match ethernet_address /\v[0-9A-Fa-f]{4}\.[0-9A-Fa-f]{4}\.[0-9A-Fa-f]{4}/
syn match ethernet_address /\v[0-9A-Fa-f]{2}[-:][0-9A-Fa-f]{2}[-:][0-9A-Fa-f]{2}[-:][0-9A-Fa-f]{2}[-:][0-9A-Fa-f]{2}[-:][0-9A-Fa-f]{2}/
exe s:h . "ethernet_address" . s:bold . s:fgred

"}}}
" IP Address highlighting {{{
" The error catching isn't really vetted out here.  It should highlight *any*
" bad IP address or subnet mask in a way such to show it's wrong.  Doesn't
" quite do that yet.
"
" experimental ip address octet by octet matching and error catch {{{2
" ends up looking kind of trippy, but gives a good impression of how match
" progression works.
"
"syn match anyerror /[^0-9]*/ contained
"syn match anyerror /[0-9][^0-9]\+/ contained
"HiLink    anyerror ErrorMsg
"
"syn match ipaddr_octet_1 /\v(25[0-4]|2[0-4]\d|1\d{,2}|\d{1,2})\./ nextgroup=ipaddr_octet_2,anyerror contained containedin=ipaddr_region
"HiLink ipaddr_octet_1 gitcommitDiscardedType
"
"syn match ipaddr_octet_2 contained /\v(25[0-4]|2[0-4]\d|1\d{,2}|\d{1,2})\./ nextgroup=ipaddr_octet_3,anyerror
"HiLink ipaddr_octet_2 pandocSubscript
"
"syn match ipaddr_octet_3 contained /\v(25[0-4]|2[0-4]\d|1\d{,2}|\d{1,2})\./ nextgroup=ipaddr_octet_4,anyerror
"HiLink ipaddr_octet_3 javaScript
"
"syn match ipaddr_octet_4 contained /\v(25[0-4]|2[0-4]\d|1\d{,2}|\d{1,2})/ nextgroup=cidr
"HiLink ipaddr_octet_4 helpNote

"2}}}
syn match zeros /\s0\.0\.0\.0/ nextgroup=ipaddr,ipaddr_cidr,subnetmask,wildcard skipwhite
exe s:h . "zeros" . s:bold . s:fgpink

"syn match ipaddress /(1\d\d|2[0-4]\d|25[0-5]|[1-9]\d|\d)\.(1\d\d|2[0-4]\d|25[0-5]|[1-9]\d|\d)\.(1\d\d|2[0-4]\d|25[0-5]|[1-9]\d|\d)\.(1\d\d|2[0-4]\d|25[0-5]|[1-9]\d|\d)/

syn match ipaddress /\v\s(25[0-4]|2[0-4]\d|1\d{1,2}|[1-9]\d|[1-9])\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2})\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2})\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2})/ nextgroup=ipaddr,ipaddr_cidr,subnetmask,wildcard skipwhite 
exe s:h . "ipaddress" . s:fgpink


syn match badmask /\v (12[0-79]|19[013-9]|1[013-8]\d|22[0-35-9]|24[13-9]|25[0136-9]|0\d{1,})\.
					   \(12[0-79]|19[013-9]|1[013-8]\d|22[0-35-9]|24[13-9]|25[0136-9]|0\d{1,})\.
					   \(12[0-79]|19[013-9]|1[013-8]\d|22[0-35-9]|24[13-9]|25[0136-9]|0\d{1,})\.
					   \(12[0-79]|19[013-9]|1[013-8]\d|22[0-35-9]|24[13-9]|25[0136-9]|0\d{1,})/ contained
exe s:h . "badmask" . s:rev . s:fgred

" BadIPaddr match {{{2
syn match BadIPaddr /\v(25[6-9]|2[6-9]\d|[3-9]\d\d)\.\d{1,3}\.\d{1,3}\.\d{1,3}/       contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.(2[5][6-9]|2[6-9]\d|[3-9]\d\d)\.\d{1,3}\.\d{1,3}/     contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{0,3}\.(2[5][6-9]|2[6-9]\d|[3-9]\d\d)\.\d{1,3}/     contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{1,3}\.\d{1,3}\.(2[5][6-9]|2[6-9]\d|[3-9]\d\d)/     contained containedin=ipaddr_region
syn match BadIPaddr /\v(25[6-9]|2[6-9]\d|[3-9]\d\d)\.
                        \(25[6-9]|2[6-9]\d|[3-9]\d\d)\.
                        \(25[6-9]|2[6-9]\d|[3-9]\d\d)\.
                        \(25[6-9]|2[6-9]\d|[3-9]\d\d)/                                contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{1,3}\.\d{4,}\.\d{1,3}/                             contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{4,}\.\d{1,3}\.\d{1,3}/                             contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{4,}\.\d{1,3}\.\d{1,3}\.\d{1,3}/                             contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{4,}\./                           contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{1,3}\.\d{4,}\.\d{1,3}\./                           contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{4,}\.\d{1,3}\.\d{1,3}\./                           contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{4,}\.\d{1,3}\.\d{1,3}\.\d{1,3}\./                           contained containedin=ipaddr_region
syn match BadIPaddr /\v\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{4,}/                             contained containedin=ipaddr_region
exe s:h . "BadIPaddr" . s:standout . s:fgred
"2}}}


syn match subnetmask contained /\v (0|192|224|240|248|252|254|255)\.(0|128|192|224|240|248|252|254|255)\.(0|128|192|224|240|248|252|254|255)\.(0|128|192|224|240|248|252|254|255)/he=e+1
exe s:h . "subnetmask" . s:italic . s:fgkeyword

syn match wildcard contained /\v (63|31|15|7|3|1|0)\.(255|63|31|15|7|3|1|0)\.(255|63|31|15|7|3|1|0)\.(255|63|31|15|7|3|1|0)/he=e+1
exe s:h . "wildcard" . s:italic . s:fgblue

syn match ipaddr_kw /ip address/ contained
exe s:h . "ipaddr_kw" . s:fgkeyword

syn match badmaskoctect /\v( 12[0-79]|19[013-9]|1[013-8]\d|22[0-35-9]|24[13-9]|25[0136-9]|0\d{1,})/ contained
exe s:h . "badmaskoctect" . s:standout . s:fgred

" This doesn't match *everything* because inspite of being contained, it
" somehow matches some unix prompts, so it just covers most errant characters
syn match ipaddr_anyerror "\a\|\d\|[.()!#^&*\-_=+{};'",./<>?]\+" contained containedin=ipaddr_region
exe s:h . "ipaddr_anyerror" . s:rev . s:fgred

syn match ipaddr_cidr contained /\v[/]\d{1,3}/
exe s:h . "ipaddr_cidr" . s:italic . s:fgkeyword

syn match ipaddr /\v(25[0-4]|2[0-4]\d|1\d{1,2}|[1-9]\d|[1-9])\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2})\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2})\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2})/ nextgroup=ipaddr_cidr,subnetmask,wildcard,skipwhite contained
exe s:h . "ipaddr" . s:fgpink

syn region ipaddr_subnetmask_in_ipaddr matchgroup=subnetmask start=/\v(0|192|224|240|248|252|254|255)\.(0|128|192|240|224|248|252|254|255)\.(0|128|192|224|240|248|252|254|255)\.(0|128|192|224|240|248|252|254|255)/ end=/$/ keepend skipwhite transparent excludenl contains=subnetmask contained

syn region ipaddr_in_ciscoipaddr matchgroup=ipaddr start=/\v(25[0-4]|2[0-4]\d|1\d\d|\d{1,2})\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2}|0)\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2}|0)\.(25[0-5]|2[0-4]\d|1\d\d|\d{1,2})/ end=/$/ keepend skipwhite excludenl transparent contains=ipaddr_cidr,ipaddr,ipaddr_subnetmask_in_ipaddr,ipaddr_anyerror contained

syn region ipaddr_region matchgroup=ipaddr_kw start=/ip addr[e]\{,1}[s]\{,1}[s]\{,1}/rs=e+1 end=/$/ contains=ipaddr_in_ciscoipaddr keepend excludenl transparent skipwhite

"}}}
" log messages {{{

syntax match timestamp excludenl / \d\d:\d\d:\d\d[ .]/ keepend nextgroup=subseconds
exe s:h . "timestamp" . s:fgbluegreen

syntax match subseconds /\.\d\+/ contained keepend skipwhite
exe s:h . "subseconds" . s:italic . s:fggray

syntax match ciscoerror excludenl /%[^ ]\{-}:/ keepend skipwhite
exe s:h . "ciscoerror" . s:bold . s:fgred

"syntax region message excludenl start=/\s \d\d/ end=/.\$/ contains=devicedaystamp,ciscoerror
"exe s:h . "region" . s:fgorange

"syntax match logtimestamp excludenl /\v\d\d:\d\d:\d\d\.+/ contained nextgroup=ciscodevice,subseconds skipwhite
"exe s:h . "match" . s:fgbluegreen

"syntax match logdaystamp /^\u\w\+\s\+\d\+\s/ nextgroup=logtimestamp skipwhite
"exe s:h . "match" . s:fgorange

"syntax match devicetimestamp /\d\d:\d\d:\d\d/ contained keepend
"exe s:h . "match" . s:fgbluegreen

"syntax match devicedaystamp / \u\w\+\s\+\d\+\s / nextgroup=devicetimestamp skipwhite keepend 
"exe s:h . "match" . s:fgorange

"}}}
" Prompts {{{
"syn match config_prompt /^[^ ]\{-1,63}([a-zA-Z\-]*)#/ contained
"exe s:h . "config_prompt" . s:fgwhite . s:bgbrown
"hi config_prompt ctermfg=white ctermbg=darkred guibg=firebrick guifg=white

syn match config_prompt_hostname /^[^ ]\{-1,63}(/ contained nextgroup=config_word
exe s:h . "config_prompt_hostname" . s:fgwhite . s:bgbluegreen
"hi config_prompt_hostname ctermfg=white ctermbg=darkred guibg=firebrick guifg=white

syn match config_word /config/ contained
exe s:h . "config_word" . s:fgwhite . s:bgred
"hi config_word ctermbg=red ctermfg=white guibg=red guifg=white

syn match config_mode /-[^ )]\{1,32}/ contained
exe s:h . "config_mode" . s:fgwhite . s:bgorange
"hi config_mode ctermbg=darkyellow ctermfg=white guibg=darkorange guifg=white

syn match config_prompt_end /)#/ contained
exe s:h . "config_prompt_end" . s:fgwhite . s:bgbluegreen
"hi config_prompt_end ctermfg=white ctermbg=darkred guibg=firebrick guifg=white

syn match hash_prompt /^[^ ]\{-1,63}\#/ excludenl 
exe s:h . "hash_prompt" . s:bold . s:fgwhite . s:bgbluegreen
"hi hash_prompt cterm=none ctermfg=white ctermbg=darkblue gui=bold guifg=white guibg=brown
syn region config_prompt_reg excludenl keepend start=/^[a-zA-Z0-9]\{-1,63}([a-zA-Z\-]*)#/ end=" " transparent contains=config_prompt_hostname,config_word,config_mode,config_prompt_end


"}}}

let b:current_syntax = "cisco"
