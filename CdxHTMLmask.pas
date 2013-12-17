unit CdxHTMLmask;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxHTMLMask
Version:    1.1
Purpose:    Mask special chars to HTML entities and vice versa
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   02.09.2008    Initial version written in D7 (Non-Unicode)
1.1   14.12.2013    Rework as FPC/Lazarus version (Unicode)
                    Moved entities into a constant array

------------------------------------------------------------------------
Additional Technical Information:
------------------------------------------------------------------------

Character entity references in HTML
Source: http://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references

}

//enable entitity masking
{$DEFINE MASK}
//enable entitity unmasking
{$DEFINE UNMASK}
//enable test string
{$DEFINE TESTSTRING}

interface

uses
  SysUtils, LazUTF8;

type
  TEntity = record
    Unicode:  UnicodeString;
    HTML:     UnicodeString;
  end;

type
  TEntityList = array[0..2] of TEntity;

function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
{$IFDEF MASK}
function MaskUnicodeToHTML(Text: UnicodeString): UnicodeString;
function MaskUTF8ToHTML(Text: UTF8String): UTF8String;
function MaskAnsiToHTML(Text: AnsiString): AnsiString;
{$ENDIF}
{$IFDEF UNMASK}
function UnmaskHTMLToUnicode(Text: UnicodeString): UnicodeString;
function UnmaskHTMLToUTF8(Text: UTF8String): UTF8String;
function UnmaskHTMLToAnsi(Text: AnsiString): AnsiString;
{$ENDIF}

const
  EntityList: array[0..253] of TEntity =
    (
      (Unicode: #$0026; HTML: '&amp;'),       // & ampersand (must be first char to be replaced!)
      (Unicode: #$0022; HTML: '&quot;'),      // " quotation mark (APL quote)
      (Unicode: #$0027; HTML: '&apos;'),      // ' apostrophe (apostrophe-quote)
      (Unicode: #$003C; HTML: '&lt;'),        // < less-than sign
      (Unicode: #$003E; HTML: '&gt;'),        // > greater-than sign
      (Unicode: #$00A0; HTML: '&nbsp;'),      //   no-break space (non-breaking space)
      (Unicode: #$00A1; HTML: '&iexcl;'),     // ¡ inverted exclamation mark
      (Unicode: #$00A2; HTML: '&cent;'),      // ¢ cent sign
      (Unicode: #$00A3; HTML: '&pound;'),     // £ pound sign
      (Unicode: #$00A4; HTML: '&curren;'),    // ¤ currency sign
      (Unicode: #$00A5; HTML: '&yen;'),       // ¥ yen sign (yuan sign)
      (Unicode: #$00A6; HTML: '&brvbar;'),    // ¦ broken bar (broken vertical bar)
      (Unicode: #$00A7; HTML: '&sect;'),      // § section sign
      (Unicode: #$00A8; HTML: '&uml;'),       // ¨ diaeresis (spacing diaeresis); see Germanic umlaut
      (Unicode: #$00A9; HTML: '&copy;'),      // © copyright symbol
      (Unicode: #$00AA; HTML: '&ordf;'),      // ª feminine ordinal indicator
      (Unicode: #$00AB; HTML: '&laquo;'),     // « left-pointing double angle quotation mark (left pointing guillemet)
      (Unicode: #$00AC; HTML: '&not;'),       // ¬ not sign
      (Unicode: #$00AD; HTML: '&shy;'),       // ­ soft hyphen (discretionary hyphen)
      (Unicode: #$00AE; HTML: '&reg;'),       // ® registered sign (registered trademark symbol)
      (Unicode: #$00AF; HTML: '&macr;'),      // ¯ macron (spacing macron, overline, APL overbar)
      (Unicode: #$00B0; HTML: '&deg;'),       // ° degree symbol
      (Unicode: #$00B1; HTML: '&plusmn;'),    // ± plus-minus sign (plus-or-minus sign)
      (Unicode: #$00B2; HTML: '&sup2;'),      // ² superscript two (superscript digit two, squared)
      (Unicode: #$00B3; HTML: '&sup3;'),      // ³ superscript three (superscript digit three, cubed)
      (Unicode: #$00B4; HTML: '&acute;'),     // ´ acute accent (spacing acute)
      (Unicode: #$00B5; HTML: '&micro;'),     // µ micro sign
      (Unicode: #$00B6; HTML: '&para;'),      // ¶ pilcrow sign (paragraph sign)
      (Unicode: #$00B7; HTML: '&middot;'),    // · middle dot (Georgian comma, Greek middle dot)
      (Unicode: #$00B8; HTML: '&cedil;'),     // ¸ cedilla (spacing cedilla)
      (Unicode: #$00B9; HTML: '&sup1;'),      // ¹ superscript one (superscript digit one)
      (Unicode: #$00BA; HTML: '&ordm;'),      // º masculine ordinal indicator
      (Unicode: #$00BB; HTML: '&raquo;'),     // » right-pointing double angle quotation mark (right pointing guillemet)
      (Unicode: #$00BC; HTML: '&frac14;'),    // ¼ vulgar fraction one quarter (fraction one quarter)
      (Unicode: #$00BD; HTML: '&frac12;'),    // ½ vulgar fraction one half (fraction one half)
      (Unicode: #$00BE; HTML: '&frac34;'),    // ¾ vulgar fraction three quarters (fraction three quarters)
      (Unicode: #$00BF; HTML: '&iquest;'),    // ¿ inverted question mark (turned question mark)
      (Unicode: #$00C0; HTML: '&Agrave;'),    // À Latin capital letter A with grave accent (Latin capital letter A grave)
      (Unicode: #$00C1; HTML: '&Aacute;'),    // Á Latin capital letter A with acute accent
      (Unicode: #$00C2; HTML: '&Acirc;'),     // Â Latin capital letter A with circumflex
      (Unicode: #$00C3; HTML: '&Atilde;'),    // Ã Latin capital letter A with tilde
      (Unicode: #$00C4; HTML: '&Auml;'),      // Ä Latin capital letter A with diaeresis
      (Unicode: #$00C5; HTML: '&Aring;'),     // Å Latin capital letter A with ring above (Latin capital letter A ring)
      (Unicode: #$00C6; HTML: '&AElig;'),     // Æ Latin capital letter AE (Latin capital ligature AE)
      (Unicode: #$00C7; HTML: '&Ccedil;'),    // Ç Latin capital letter C with cedilla
      (Unicode: #$00C8; HTML: '&Egrave;'),    // È Latin capital letter E with grave accent
      (Unicode: #$00C9; HTML: '&Eacute;'),    // É Latin capital letter E with acute accent
      (Unicode: #$00CA; HTML: '&Ecirc;'),     // Ê Latin capital letter E with circumflex
      (Unicode: #$00CB; HTML: '&Euml;'),      // Ë Latin capital letter E with diaeresis
      (Unicode: #$00CC; HTML: '&Igrave;'),    // Ì Latin capital letter I with grave accent
      (Unicode: #$00CD; HTML: '&Iacute;'),    // Í Latin capital letter I with acute accent
      (Unicode: #$00CE; HTML: '&Icirc;'),     // Î Latin capital letter I with circumflex
      (Unicode: #$00CF; HTML: '&Iuml;'),      // Ï Latin capital letter I with diaeresis
      (Unicode: #$00D0; HTML: '&ETH;'),       // Ð Latin capital letter Eth
      (Unicode: #$00D1; HTML: '&Ntilde;'),    // Ñ Latin capital letter N with tilde
      (Unicode: #$00D2; HTML: '&Ograve;'),    // Ò Latin capital letter O with grave accent
      (Unicode: #$00D3; HTML: '&Oacute;'),    // Ó Latin capital letter O with acute accent
      (Unicode: #$00D4; HTML: '&Ocirc;'),     // Ô Latin capital letter O with circumflex
      (Unicode: #$00D5; HTML: '&Otilde;'),    // Õ Latin capital letter O with tilde
      (Unicode: #$00D6; HTML: '&Ouml;'),      // Ö Latin capital letter O with diaeresis
      (Unicode: #$00D7; HTML: '&times;'),     // × multiplication sign
      (Unicode: #$00D8; HTML: '&Oslash;'),    // Ø Latin capital letter O with stroke (Latin capital letter O slash)
      (Unicode: #$00D9; HTML: '&Ugrave;'),    // Ù Latin capital letter U with grave accent
      (Unicode: #$00DA; HTML: '&Uacute;'),    // Ú Latin capital letter U with acute accent
      (Unicode: #$00DB; HTML: '&Ucirc;'),     // Û Latin capital letter U with circumflex
      (Unicode: #$00DC; HTML: '&Uuml;'),      // Ü Latin capital letter U with diaeresis
      (Unicode: #$00DD; HTML: '&Yacute;'),    // Ý Latin capital letter Y with acute accent
      (Unicode: #$00DE; HTML: '&THORN;'),     // Þ Latin capital letter THORN
      (Unicode: #$00DF; HTML: '&szlig;'),     // ß Latin small letter sharp s (ess-zed); see German Eszett
      (Unicode: #$00E0; HTML: '&agrave;'),    // à Latin small letter a with grave accent
      (Unicode: #$00E1; HTML: '&aacute;'),    // á Latin small letter a with acute accent
      (Unicode: #$00E2; HTML: '&acirc;'),     // â Latin small letter a with circumflex
      (Unicode: #$00E3; HTML: '&atilde;'),    // ã Latin small letter a with tilde
      (Unicode: #$00E4; HTML: '&auml;'),      // ä Latin small letter a with diaeresis
      (Unicode: #$00E5; HTML: '&aring;'),     // å Latin small letter a with ring above
      (Unicode: #$00E6; HTML: '&aelig;'),     // æ Latin small letter ae (Latin small ligature ae)
      (Unicode: #$00E7; HTML: '&ccedil;'),    // ç Latin small letter c with cedilla
      (Unicode: #$00E8; HTML: '&egrave;'),    // è Latin small letter e with grave accent
      (Unicode: #$00E9; HTML: '&eacute;'),    // é Latin small letter e with acute accent
      (Unicode: #$00EA; HTML: '&ecirc;'),     // ê Latin small letter e with circumflex
      (Unicode: #$00EB; HTML: '&euml;'),      // ë Latin small letter e with diaeresis
      (Unicode: #$00EC; HTML: '&igrave;'),    // ì Latin small letter i with grave accent
      (Unicode: #$00ED; HTML: '&iacute;'),    // í Latin small letter i with acute accent
      (Unicode: #$00EE; HTML: '&icirc;'),     // î Latin small letter i with circumflex
      (Unicode: #$00EF; HTML: '&iuml;'),      // ï Latin small letter i with diaeresis
      (Unicode: #$00F0; HTML: '&eth;'),       // ð Latin small letter eth
      (Unicode: #$00F1; HTML: '&ntilde;'),    // ñ Latin small letter n with tilde
      (Unicode: #$00F2; HTML: '&ograve;'),    // ò Latin small letter o with grave accent
      (Unicode: #$00F3; HTML: '&oacute;'),    // ó Latin small letter o with acute accent
      (Unicode: #$00F4; HTML: '&ocirc;'),     // ô Latin small letter o with circumflex
      (Unicode: #$00F5; HTML: '&otilde;'),    // õ Latin small letter o with tilde
      (Unicode: #$00F6; HTML: '&ouml;'),      // ö Latin small letter o with diaeresis
      (Unicode: #$00F7; HTML: '&divide;'),    // ÷ division sign (obelus)
      (Unicode: #$00F8; HTML: '&oslash;'),    // ø Latin small letter o with stroke (Latin small letter o slash)
      (Unicode: #$00F9; HTML: '&ugrave;'),    // ù Latin small letter u with grave accent
      (Unicode: #$00FA; HTML: '&uacute;'),    // ú Latin small letter u with acute accent
      (Unicode: #$00FB; HTML: '&ucirc;'),     // û Latin small letter u with circumflex
      (Unicode: #$00FC; HTML: '&uuml;'),      // ü Latin small letter u with diaeresis
      (Unicode: #$00FD; HTML: '&yacute;'),    // ý Latin small letter y with acute accent
      (Unicode: #$00FE; HTML: '&thorn;'),     // þ Latin small letter thorn
      (Unicode: #$00FF; HTML: '&yuml;'),      // ÿ Latin small letter y with diaeresis
      (Unicode: #$0152; HTML: '&OElig;'),     // Œ Latin capital ligature oe
      (Unicode: #$0153; HTML: '&oelig;'),     // œ Latin small ligature oe
      (Unicode: #$0160; HTML: '&Scaron;'),    // Š Latin capital letter s with caron
      (Unicode: #$0161; HTML: '&scaron;'),    // š Latin small letter s with caron
      (Unicode: #$0178; HTML: '&Yuml;'),      // Ÿ Latin capital letter y with diaeresis
      (Unicode: #$0192; HTML: '&fnof;'),      // ƒ Latin small letter f with hook (function, florin)
      (Unicode: #$02C6; HTML: '&circ;'),      // ˆ modifier letter circumflex accent
      (Unicode: #$02DC; HTML: '&tilde;'),     // ˜  small tilde
      (Unicode: #$0391; HTML: '&Alpha;'),     //   Greek capital letter Alpha
      (Unicode: #$0392; HTML: '&Beta;'),      //   Greek capital letter Beta
      (Unicode: #$0393; HTML: '&Gamma;'),     //   Greek capital letter Gamma
      (Unicode: #$0394; HTML: '&Delta;'),     //   Greek capital letter Delta
      (Unicode: #$0395; HTML: '&Epsilon;'),   //   Greek capital letter Epsilon
      (Unicode: #$0396; HTML: '&Zeta;'),      //   Greek capital letter Zeta
      (Unicode: #$0397; HTML: '&Eta;'),       //   Greek capital letter Eta
      (Unicode: #$0398; HTML: '&Theta;'),     //   Greek capital letter Theta
      (Unicode: #$0399; HTML: '&Iota;'),      //   Greek capital letter Iota
      (Unicode: #$039A; HTML: '&Kappa;'),     //   Greek capital letter Kappa
      (Unicode: #$039B; HTML: '&Lambda;'),    //   Greek capital letter Lambda
      (Unicode: #$039C; HTML: '&Mu;'),        //   Greek capital letter Mu
      (Unicode: #$039D; HTML: '&Nu;'),        //   Greek capital letter Nu
      (Unicode: #$039E; HTML: '&Xi;'),        //   Greek capital letter Xi
      (Unicode: #$039F; HTML: '&Omicron;'),   //   Greek capital letter Omicron
      (Unicode: #$03A0; HTML: '&Pi;'),        //   Greek capital letter Pi
      (Unicode: #$03A1; HTML: '&Rho;'),       //   Greek capital letter Rho
      (Unicode: #$03A3; HTML: '&Sigma;'),     //   Greek capital letter Sigma
      (Unicode: #$03A4; HTML: '&Tau;'),       //   Greek capital letter Tau
      (Unicode: #$03A5; HTML: '&Upsilon;'),   //   Greek capital letter Upsilon
      (Unicode: #$03A6; HTML: '&Phi;'),       //   Greek capital letter Phi
      (Unicode: #$03A7; HTML: '&Chi;'),       //   Greek capital letter Chi
      (Unicode: #$03A8; HTML: '&Psi;'),       //   Greek capital letter Psi
      (Unicode: #$03A9; HTML: '&Omega;'),     //   Greek capital letter Omega
      (Unicode: #$03B1; HTML: '&alpha;'),     //   Greek small letter alpha
      (Unicode: #$03B2; HTML: '&beta;'),      //   Greek small letter beta
      (Unicode: #$03B3; HTML: '&gamma;'),     //   Greek small letter gamma
      (Unicode: #$03B4; HTML: '&delta;'),     //   Greek small letter delta
      (Unicode: #$03B5; HTML: '&epsilon;'),   //   Greek small letter epsilon
      (Unicode: #$03B6; HTML: '&zeta;'),      //   Greek small letter zeta
      (Unicode: #$03B7; HTML: '&eta;'),       //   Greek small letter eta
      (Unicode: #$03B8; HTML: '&theta;'),     //   Greek small letter theta
      (Unicode: #$03B9; HTML: '&iota;'),      //   Greek small letter iota
      (Unicode: #$03BA; HTML: '&kappa;'),     //   Greek small letter kappa
      (Unicode: #$03BB; HTML: '&lambda;'),    //   Greek small letter lambda
      (Unicode: #$03BC; HTML: '&mu;'),        //   Greek small letter mu
      (Unicode: #$03BD; HTML: '&nu;'),        //   Greek small letter nu
      (Unicode: #$03BE; HTML: '&xi;'),        //   Greek small letter xi
      (Unicode: #$03BF; HTML: '&omicron;'),   //   Greek small letter omicron
      (Unicode: #$03C0; HTML: '&pi;'),        //   Greek small letter pi
      (Unicode: #$03C1; HTML: '&rho;'),       //   Greek small letter rho
      (Unicode: #$03C2; HTML: '&sigmaf;'),    //   Greek small letter final sigma
      (Unicode: #$03C3; HTML: '&sigma;'),     //   Greek small letter sigma
      (Unicode: #$03C4; HTML: '&tau;'),       //   Greek small letter tau
      (Unicode: #$03C5; HTML: '&upsilon;'),   //   Greek small letter upsilon
      (Unicode: #$03C6; HTML: '&phi;'),       //   Greek small letter phi
      (Unicode: #$03C7; HTML: '&chi;'),       //   Greek small letter chi
      (Unicode: #$03C8; HTML: '&psi;'),       //   Greek small letter psi
      (Unicode: #$03C9; HTML: '&omega;'),     //   Greek small letter omega
      (Unicode: #$03D1; HTML: '&thetasym;'),  //   Greek theta symbol
      (Unicode: #$03D2; HTML: '&upsih;'),     //   Greek Upsilon with hook symbol
      (Unicode: #$03D6; HTML: '&piv;'),       //   Greek pi symbol
      (Unicode: #$2002; HTML: '&ensp;'),      //   en space
      (Unicode: #$2003; HTML: '&emsp;'),      //   em space
      (Unicode: #$2009; HTML: '&thinsp;'),    //   thin space
      (Unicode: #$200C; HTML: '&zwnj;'),      //   zero-width non-joiner
      (Unicode: #$200D; HTML: '&zwj;'),       //   zero-width joiner
      (Unicode: #$200E; HTML: '&lrm;'),       //   left-to-right mark
      (Unicode: #$200F; HTML: '&rlm;'),       //   right-to-left mark
      (Unicode: #$2013; HTML: '&ndash;'),     // – en dash
      (Unicode: #$2014; HTML: '&mdash;'),     // — em dash
      (Unicode: #$2018; HTML: '&lsquo;'),     // ‘ left single quotation mark
      (Unicode: #$2019; HTML: '&rsquo;'),     // ’ right single quotation mark
      (Unicode: #$201A; HTML: '&sbquo;'),     // ‚ single low-9 quotation mark
      (Unicode: #$201C; HTML: '&ldquo;'),     // “ left double quotation mark
      (Unicode: #$201D; HTML: '&rdquo;'),     // ” right double quotation mark
      (Unicode: #$201E; HTML: '&bdquo;'),     // „ double low-9 quotation mark
      (Unicode: #$2020; HTML: '&dagger;'),    // † dagger, obelisk
      (Unicode: #$2021; HTML: '&Dagger;'),    // ‡ double dagger, double obelisk
      (Unicode: #$2022; HTML: '&bull;'),      // • bullet (black small circle)
      (Unicode: #$2026; HTML: '&hellip;'),    // … horizontal ellipsis (three dot leader)
      (Unicode: #$2030; HTML: '&permil;'),    // ‰ per mille sign
      (Unicode: #$2032; HTML: '&prime;'),     //   prime (minutes, feet)
      (Unicode: #$2033; HTML: '&Prime;'),     //   double prime (seconds, inches)
      (Unicode: #$2039; HTML: '&lsaquo;'),    // ‹ single left-pointing angle quotation mark
      (Unicode: #$203A; HTML: '&rsaquo;'),    // › single right-pointing angle quotation mark
      (Unicode: #$203E; HTML: '&oline;'),     //   overline (spacing overscore)
      (Unicode: #$2044; HTML: '&frasl;'),     //   fraction slash (solidus)
      (Unicode: #$20AC; HTML: '&euro;'),      // € euro sign
      (Unicode: #$2111; HTML: '&image;'),     //   black-letter capital I (imaginary part)
      (Unicode: #$2118; HTML: '&weierp;'),    //   script capital P (power set, Weierstrass p)
      (Unicode: #$211C; HTML: '&real;'),      //   black-letter capital R (real part symbol)
      (Unicode: #$2122; HTML: '&trade;'),     // ™ trademark symbol
      (Unicode: #$2135; HTML: '&alefsym;'),   //   alef symbol (first transfinite cardinal)
      (Unicode: #$2190; HTML: '&larr;'),      //   leftwards arrow
      (Unicode: #$2191; HTML: '&uarr;'),      //   upwards arrow
      (Unicode: #$2192; HTML: '&rarr;'),      //   rightwards arrow
      (Unicode: #$2193; HTML: '&darr;'),      //   downwards arrow
      (Unicode: #$2194; HTML: '&harr;'),      //   left right arrow
      (Unicode: #$21B5; HTML: '&crarr;'),     //   downwards arrow with corner leftwards (carriage return)
      (Unicode: #$21D0; HTML: '&lArr;'),      //   leftwards double arrow
      (Unicode: #$21D1; HTML: '&uArr;'),      //   upwards double arrow
      (Unicode: #$21D2; HTML: '&rArr;'),      //   rightwards double arrow
      (Unicode: #$21D3; HTML: '&dArr;'),      //   downwards double arrow
      (Unicode: #$21D4; HTML: '&hArr;'),      //   left right double arrow
      (Unicode: #$2200; HTML: '&forall;'),    //   for all
      (Unicode: #$2202; HTML: '&part;'),      //   partial differential
      (Unicode: #$2203; HTML: '&exist;'),     //   there exists
      (Unicode: #$2205; HTML: '&empty;'),     //   empty set (null set, diameter)
      (Unicode: #$2207; HTML: '&nabla;'),     //   nabla (backward difference)
      (Unicode: #$2208; HTML: '&isin;'),      //   element of
      (Unicode: #$2209; HTML: '&notin;'),     //   not an element of
      (Unicode: #$220B; HTML: '&ni;'),        //   contains as member
      (Unicode: #$220F; HTML: '&prod;'),      //   n-ary product (product sign)
      (Unicode: #$2211; HTML: '&sum;'),       //   n-ary summation
      (Unicode: #$2212; HTML: '&minus;'),     //   minus sign
      (Unicode: #$2217; HTML: '&lowast;'),    //   asterisk operator
      (Unicode: #$221A; HTML: '&radic;'),     //   square root (radical sign)
      (Unicode: #$221D; HTML: '&prop;'),      //   proportional to
      (Unicode: #$221E; HTML: '&infin;'),     //   infinity
      (Unicode: #$2220; HTML: '&ang;'),       //   angle
      (Unicode: #$2227; HTML: '&and;'),       //   logical and (wedge)
      (Unicode: #$2228; HTML: '&or;'),        //   logical or (vee)
      (Unicode: #$2229; HTML: '&cap;'),       //   intersection (cap)
      (Unicode: #$222A; HTML: '&cup;'),       //   union (cup)
      (Unicode: #$222B; HTML: '&int;'),       //   integral
      (Unicode: #$2234; HTML: '&there4;'),    //   therefore sign
      (Unicode: #$223C; HTML: '&sim;'),       //   tilde operator (varies with, similar to)
      (Unicode: #$2245; HTML: '&cong;'),      //   congruent to
      (Unicode: #$2248; HTML: '&asymp;'),     //   almost equal to (asymptotic to)
      (Unicode: #$2260; HTML: '&ne;'),        //   not equal to
      (Unicode: #$2261; HTML: '&equiv;'),     //   identical to; sometimes used for equivalent to
      (Unicode: #$2264; HTML: '&le;'),        //   less-than or equal to
      (Unicode: #$2265; HTML: '&ge;'),        //   greater-than or equal to
      (Unicode: #$2282; HTML: '&sub;'),       //   subset of
      (Unicode: #$2283; HTML: '&sup;'),       //   superset of[n]
      (Unicode: #$2284; HTML: '&nsub;'),      //   not a subset of
      (Unicode: #$2286; HTML: '&sube;'),      //   subset of or equal to
      (Unicode: #$2287; HTML: '&supe;'),      //   superset of or equal to
      (Unicode: #$2295; HTML: '&oplus;'),     //   circled plus (direct sum)
      (Unicode: #$2297; HTML: '&otimes;'),    //   circled times (vector product)
      (Unicode: #$22A5; HTML: '&perp;'),      //   up tack (orthogonal to, perpendicular)
      (Unicode: #$22C5; HTML: '&sdot;'),      //   dot operator[p]
      (Unicode: #$22EE; HTML: '&vellip;'),    //   vertical ellipsis
      (Unicode: #$2308; HTML: '&lceil;'),     //   left ceiling (APL upstile)
      (Unicode: #$2309; HTML: '&rceil;'),     //   right ceiling
      (Unicode: #$230A; HTML: '&lfloor;'),    //   left floor (APL downstile)
      (Unicode: #$230B; HTML: '&rfloor;'),    //   right floor
      (Unicode: #$2329; HTML: '&lang;'),      //   left-pointing angle bracket (bra)
      (Unicode: #$232A; HTML: '&rang;'),      //   right-pointing angle bracket (ket)
      (Unicode: #$25CA; HTML: '&loz;'),       //   lozenge
      (Unicode: #$2660; HTML: '&spades;'),    //   black spade suit
      (Unicode: #$2663; HTML: '&clubs;'),     //   black club suit (shamrock)
      (Unicode: #$2665; HTML: '&hearts;'),    //   black heart suit (valentine)
      (Unicode: #$2666; HTML: '&diams;')      //   black diamond suit
    );

  {$IFDEF TESTSTRING}
  TestString: UnicodeString =  #$0022+#$0026+#$0027+#$003C+#$003E+#$00A0+#$00A1+#$00A2+#$00A3+#$00A4
                              +#$00A5+#$00A6+#$00A7+#$00A8+#$00A9+#$00AA+#$00AB+#$00AC+#$00AD+#$00AE
                              +#$00AF+#$00B0+#$00B1+#$00B2+#$00B3+#$00B4+#$00B5+#$00B6+#$00B7+#$00B8
                              +#$00B9+#$00BA+#$00BB+#$00BC+#$00BD+#$00BE+#$00BF+#$00C0+#$00C1+#$00C2
                              +#$00C3+#$00C4+#$00C5+#$00C6+#$00C7+#$00C8+#$00C9+#$00CA+#$00CB+#$00CC
                              +#$00CD+#$00CE+#$00CF+#$00D0+#$00D1+#$00D2+#$00D3+#$00D4+#$00D5+#$00D6
                              +#$00D7+#$00D8+#$00D9+#$00DA+#$00DB+#$00DC+#$00DD+#$00DE+#$00DF+#$00E0
                              +#$00E1+#$00E2+#$00E3+#$00E4+#$00E5+#$00E6+#$00E7+#$00E8+#$00E9+#$00EA
                              +#$00EB+#$00EC+#$00ED+#$00EE+#$00EF+#$00F0+#$00F1+#$00F2+#$00F3+#$00F4
                              +#$00F5+#$00F6+#$00F7+#$00F8+#$00F9+#$00FA+#$00FB+#$00FC+#$00FD+#$00FE
                              +#$00FF+#$0152+#$0153+#$0160+#$0161+#$0178+#$0192+#$02C6+#$02DC+#$0391
                              +#$0392+#$0393+#$0394+#$0395+#$0396+#$0397+#$0398+#$0399+#$039A+#$039B
                              +#$039C+#$039D+#$039E+#$039F+#$03A0+#$03A1+#$03A3+#$03A4+#$03A5+#$03A6
                              +#$03A7+#$03A8+#$03A9+#$03B1+#$03B2+#$03B3+#$03B4+#$03B5+#$03B6+#$03B7
                              +#$03B8+#$03B9+#$03BA+#$03BB+#$03BC+#$03BD+#$03BE+#$03BF+#$03C0+#$03C1
                              +#$03C2+#$03C3+#$03C4+#$03C5+#$03C6+#$03C7+#$03C8+#$03C9+#$03D1+#$03D2
                              +#$03D6+#$2002+#$2003+#$2009+#$200C+#$200D+#$200E+#$200F+#$2013+#$2014
                              +#$2018+#$2019+#$201A+#$201C+#$201D+#$201E+#$2020+#$2021+#$2022+#$2026
                              +#$2030+#$2032+#$2033+#$2039+#$203A+#$203E+#$2044+#$20AC+#$2111+#$2118
                              +#$211C+#$2122+#$2135+#$2190+#$2191+#$2192+#$2193+#$2194+#$21B5+#$21D0
                              +#$21D1+#$21D2+#$21D3+#$21D4+#$2200+#$2202+#$2203+#$2205+#$2207+#$2208
                              +#$2209+#$220B+#$220F+#$2211+#$2212+#$2217+#$221A+#$221D+#$221E+#$2220
                              +#$2227+#$2228+#$2229+#$222A+#$222B+#$2234+#$223C+#$2245+#$2248+#$2260
                              +#$2261+#$2264+#$2265+#$2282+#$2283+#$2284+#$2286+#$2287+#$2295+#$2297
                              +#$22A5+#$22C5+#$22EE+#$2308+#$2309+#$230A+#$230B+#$2329+#$232A+#$25CA
                              +#$2660+#$2663+#$2665+#$2666
                              ;
  {$ENDIF}

implementation

function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
//Unicode StringReplace() variant based on original SysUtil function
var
  Srch,OldP,RemS: UnicodeString; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=WideUpperCase(Srch);
    OldP:=WideUpperCase(OldP);
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;

{$IFDEF MASK}
function MaskUnicodeToHTML(Text: UnicodeString): UnicodeString;
//convert special chars to HTML entity mask
var
  Index: Integer;

begin
  for Index:=0 to Length(EntityList)-1 do
    Text:=UnicodeStringReplace(Text,EntityList[Index].Unicode,EntityList[Index].HTML,[rfReplaceAll]);
  result:=Text;
end;

function MaskUTF8ToHTML(Text: UTF8String): UTF8String;
//convert special chars to HTML entity mask (only UTF8 type)
begin
  result:=UTF16toUTF8(MaskUnicodeToHTML(UTF8toUTF16(Text)));
end;

function MaskAnsiToHTML(Text: AnsiString): AnsiString;
//convert special chars to HTML entity mask (only String type)
begin
  result:=UTF8toSys(MaskUTF8ToHTML(SysToUTF8(Text)));
end;
{$ENDIF}

{$IFDEF UNMASK}
function UnmaskHTMLToUnicode(Text: UnicodeString): UnicodeString;
//remove all HTML entity masks
var
  Index: Integer;

begin
  for Index:=0 to Length(EntityList)-1 do
    Text:=UnicodeStringReplace(Text,EntityList[Index].HTML,EntityList[Index].Unicode,[rfReplaceAll]);
  result:=Text;
end;

function UnmaskHTMLToUTF8(Text: UTF8String): UTF8String;
//remove all HTML entity masks (only UTF8 type)
begin
  result:=UTF16toUTF8(UnmaskHTMLToUnicode(UTF8toUTF16(Text)));
end;

function UnmaskHTMLToAnsi(Text: AnsiString): AnsiString;
//remove all HTML entity masks (only AnsiString type)
begin
  result:=UTF8toSys(UnmaskHTMLToUTF8(ANSItoUTF8(Text)));
end;
{$ENDIF}

end.
