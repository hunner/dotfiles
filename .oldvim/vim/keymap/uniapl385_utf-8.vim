" Mapping the unified APL keyboard to emit the apl385 unicode characters
" Original Author:	Peter Keller <psilord@cs.wisc.edu>
" Maintainer:		Peter Keller <psilord@cs.wisc.edu>
" Last Changed:		2008 Oct 23
" This file is under the BSD license.

let b:keymap_name = "uniapl385"

loadkeymap
" row 1 of keyboard, lowercase and uppercase
<M-`>	<char-0x22c4>	" {diamond}
<M-~>	<char-0x235e>	" {quotequad}
<M-1>	<char-0x00a8>	" {each}
<M-!>	<char-0x2336>	" {ibeam}
<M-2>	<char-0x00af>	" {negative}
<M-@>	<char-0x236b>	" {deltilde}
<M-3>	<char-0x003c>	" {lessthan}
<M-#>	<char-0x2352>	" {gradedown}
<M-4>	<char-0x2264>	" {lessthanequal}
<M-$>	<char-0x234b>	" {gradeup}
<M-5>	<char-0x003d>	" {equal}
<M-%>	<char-0x233d>	" {reverse}
<M-6>	<char-0x2265>	" {greaterorequal}
<M-^>	<char-0x2349>	" {transpose}
<M-7>	<char-0x003e>	" {morethan}
<M-&>	<char-0x2296>	" {rotatefirstaxis}
<M-8>	<char-0x2260>	" {notequalto}
<M-*>	<char-0x235f>	" {log}
<M-9>	<char-0x2228>	" {logicalor}
<M-(>	<char-0x2371>	" {aplnor}
<M-0>	<char-0x005e>	" {logicaland}
<M-)>	<char-0x2372>	" {aplnand}
<M-->	<char-0x00d7>	" {multiply}
<M-_>	<char-0x0021>	" {factorial}
<M-+>	<char-0x2339>	" {domino}
<M-=>	<char-0x00f7>	" {divide}

" row 2 of keyboard, lowercase and uppercase
<M-q>	<char-0x003f>	" {random}
<M-Q>	<char-0x0051>	" letter Q
<M-w>	<char-0x2375>	" {omega}
<M-W>	<char-0x2364>	" {hoot}
<M-e>	<char-0x220a>	" {membership}
<M-E>	<char-0x2377>	" {find}
<M-r>	<char-0x2374>	" {rho}
<M-R>	<char-0x0052>	" letter R
<M-t>	<char-0x007e>	" {tilde}
<M-T>	<char-0x0054>	" letter T
<M-y>	<char-0x2191>	" {take}
<M-Y>	<char-0x2350>	" {quadup}
<M-u>	<char-0x2193>	" {drop}
<M-U>	<char-0x2357>	" {quaddown}
<M-i>	<char-0x2373>	" {iota}
<M-I>	<char-0x2378>	" {iotaunderbar}
<M-o>	<char-0x25cb>	" {circle}
<M-O>	<char-0x2337>	" {squad}
<M-p>	<char-0x002a>	" {power}
<M-P>	<char-0x0050>	" letter P
<M-[>	<char-0x2190>	" {assign}
<M-{>	<char-0x2347>	" {quadleft}
<M-]>	<char-0x2192>	" {goto}
<M-}>	<char-0x2348>	" {quadright}
<M-\>	<char-0x22a2>	" {right tack}
<M-|>	<char-0x22a3>	" {left tack}

" row 3 of keyboard, lowercase and uppercase
<M-a>	<char-0x237a>	" {alpha}
<M-A>	<char-0x0041>	" letter A
<M-s>	<char-0x2308>	" {max}
<M-S>	<char-0x0053>	" letter S
<M-d>	<char-0x230A>	" {min}
<M-D>	<char-0x0044>	" letter D
<M-f>	<char-0x005f>	" {underbar}
<M-F>	<char-0x0046>	" letter F
<M-g>	<char-0x2207>	" {del}
<M-G>	<char-0x0047>	" letter G
<M-h>	<char-0x2206>	" {delta}
<M-H>	<char-0x2359>	" {deltaunderbar}
<M-j>	<char-0x2218>	" {jot}
<M-J>	<char-0x233b>	" {quadjot}
<M-k>	<char-0x0027>	" {quote}
<M-K>	<char-0x2337>	" {squad}
<M-l>	<char-0x25af>	" {quad}
<M-L>	<char-0x2342>	" {sandwich}
<M-;>	<char-0x234e>	" {execute}
<M-:>	<char-0x2261>	" {match}
<M-'>	<char-0x2355>	" {format}
<M-">	<char-0x2262>	" {notmatch}

" row 4 of keyboard, lowercase and uppercase
<M-z>	<char-0x2282>	" {enclose}
<M-Z>	<char-0x005a>	" letter Z
<M-x>	<char-0x2283>	" {disclose}
<M-X>	<char-0x0058>	" letter X
<M-c>	<char-0x2229>	" {intersect}
<M-C>	<char-0x0043>	" letter C
<M-v>	<char-0x222a>	" {union}
<M-V>	<char-0x0056>	" letter V
<M-b>	<char-0x22a5>	" {decode}
<M-B>	<char-0x0042>	" letter B
<M-n>	<char-0x22a4>	" {encode}
<M-N>	<char-0x004e>	" letter N
<M-m>	<char-0x007c>	" {remainder}
<M-M>	<char-0x004d>	" letter M
<M-,>	<char-0x235d>	" {lamp}
<M-<>	<char-0x236a>	" {commabar}
<M-.>	<char-0x2340>	" {slopebar}
<M->>	<char-0x2235>	" {paw}
<M-/>	<char-0x233f>	" {slashbar}
<M-?>	<char-0x003f>	" {random}
