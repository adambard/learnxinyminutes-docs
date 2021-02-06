---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
translators:
    - ["Rasendran Kirushan", "https://github.com/kirushanr"]
filename: learncss-ta.css
lang: in-ta
---


இணையத்தின்    ஆரம்ப காலத்தில்  முழுமையாக உரைகளை மட்டுமே கொண்டிருந்தன. 
ஆனால் உலாவிகளில் கொண்டு வரப்பட்ட மாற்றங்களில் முழுமையான காட்சிபடுத்தல்களுடன்
கூடிய இணையதளங்கள் உருவாகின.


CSS ஆனது HTML மற்றும் அதன் அழகுபடுத்கூடிய காரணிகளையும் வேறுபடுத்த உதவியது.

ஒரு html இல் உள்ள உறுப்புகளை(elements) வெவ்வேறு வகையான காட்சி பண்புகளை வழங்க உதவுகிறது.

இந்த வழிகாட்டி CSS2 உக்கு எழுதப்பட்டுள்ளது, இருப்பினும் தற்போது CSS 3 வேகமாக பிரபல்யமாகி வருகிறது.

**குறிப்பு:**
CSS ஆனது முற்று முழுதாக visual(காட்சி)  மாற்றங்களை தருவதால் அதை நீங்கள் முயற்சிக்க
இதை    உபயோகபடுத்தலாம்  [dabblet](http://dabblet.com/).
இந்த வழிகாட்டியின் பிரதான நோக்கம் CSS இன்  syntax மற்றும் மேலும் சில வழிமுறைகளை
உங்களுக்கு கற்று தருவதாகும்

```css
/* css இல் குறிப்புகளை இப்படி இடலாம் */

/* ####################
   ## SELECTORS
   #################### */

/* ஒரு HTML பக்கத்தில் இருக்கும் உறுப்பை நாம் selector மூலம் தெரிவு செய்யலாம்
selector { property: value; /* more properties...*/ }

/*
கிழே ஒரு உதாரணம் காட்டப்பட்டுள்ளது:

<div class='class1 class2' id='anID' attr='value' otherAttr='en-us foo bar' />
*/

/* நீங்கள் அந்த உறுப்பை அதன் CSS class மூலம் தெரியலாம் */
.class1 { }

/* அல்லது இவ்வாறு  இரண்டு  class மூலம் தெரியலாம்! */
.class1.class2 { }

/* அல்லது  அதன்  பெயரை பாவித்து தெரியலாம் */
div { }

/* அல்லது  அதன் id ஐ  பயன்படுத்தி  தெரியலாம்*/
#anID { }

/* அல்லது ஒரு  உறுப்பின்   பண்பு ஒன்றின்  மூலம்! */
[attr] { font-size:smaller; }

/* அல்லது அந்த  பண்பு ஒரு  குறிப்பிட்ட  பெறுமானத்தை கொண்டு இருப்பின் */
[attr='value'] { font-size:smaller; }

/* ஒரு  பெறுமதியுடன் ஆரம்பமாகும் போது (CSS 3) */
[attr^='val'] { font-size:smaller; }

/* அல்லது  ஒரு பெறுமதியுடன் முடிவடையும் போது  (CSS 3) */
[attr$='ue'] { font-size:smaller; }

/*  அல்லது  காற்புள்ளியால் பிரிக்கப்பட்ட  பெறுமானங்களை கொண்டு இருப்பின் */
[otherAttr~='foo'] { }
[otherAttr~='bar'] { }

/* அல்லது  `-` பிரிக்கப்பட்ட  பெறுமானங்களை கொண்டு இருப்பின், உ.ம்:-, "-" (U+002D) */
[otherAttr|='en'] { font-size:smaller; }


/* நாம் இரண்டு selectors ஐ ஒன்றாக உபயோகித்தும் ஒரு உறுப்பை அணுக முடியும்  , 
அவற்றுக்கு இடயே இடைவெளி காணப்படகூடாது
 */
div.some-class[attr$='ue'] { }

/*அல்லது ஒரு உறுப்பினுள் இருக்கும் இன்னொரு உறுப்பை (child element) அணுக */
div.some-parent > .class-name { }

/* ஒரு  ஒரு  பிரதான உறுப்பில் உள்ள உப உறுப்புகளை அணுக*/
div.some-parent .class-name { }

/* மேலே  குறிபிட்ட அணுகுமுறையில் இடைவெளி காணப்படாது விடின் 
	அந்த selector வேலை செய்யாது
 */
div.some-parent.class-name { }

/* அல்லது ஒரு உறுப்புக்கு அடுத்துள்ள  */
.i-am-just-before + .this-element { }

/* or அல்லது அதற்கு முந்தய உறுப்பின்  மூலம் */
.i-am-any-element-before ~ .this-element { }

/* 
	சில selectors ஐ pseudo class மூலம் அணுக முடியும் , எப்போது எனில் அவை
	குறித்த ஒரு நிலையில் இருக்கும் போது ஆகும்
   */

/* உதாரணமாக நாம் ஒரு  உறுப்பின் மீதாக cursor ஐ நகர்த்தும் போது */
selector:hover { }

/* அல்லது ஒரு
பார்வையிட்ட இணைப்பு */
selector:visited { }

/* அல்லது ஒரு  பார்வையிடபடாத இணைப்பு */   
selected:link { }

/* அல்லது  ஒரு element ஐ  focus செய்யும் போது */
selected:focus { }

/* 
	எல்லா elementகளையும் ஒரே நேரத்தில் அணுக `*`
*/
* { } /* all elements */
.parent * { } /* all descendants */
.parent > * { } /* all children */

/* ####################
   ## பண்புகள்
   #################### */

selector {
    
    /*  நீளத்தின் அலகுகள் absolute அல்லது relative ஆக இருக்கலாம். */
    
    /* Relative units */
    width: 50%;       /* percentage of parent element width */
    font-size: 2em;   /* multiples of element's original font-size */
    font-size: 2rem;  /* or the root element's font-size */
    font-size: 2vw;   /* multiples of 1% of the viewport's width (CSS 3) */
    font-size: 2vh;   /* or its height */
    font-size: 2vmin; /* whichever of a vh or a vw is smaller */
    font-size: 2vmax; /* or greater */
    
    /* Absolute units */
    width: 200px;     /* pixels */
    font-size: 20pt;  /* points */
    width: 5cm;       /* centimeters */
    min-width: 50mm;  /* millimeters */
    max-width: 5in;   /* inches */
    
	
    /* Colors */
    color: #F6E;                 /* short hex format */
    color: #FF66EE;              /* long hex format */
    color: tomato;               /* a named color */
    color: rgb(255, 255, 255);   /* as rgb values */
    color: rgb(10%, 20%, 50%);   /* as rgb percentages */
    color: rgba(255, 0, 0, 0.3); /* as rgba values (CSS 3) Note: 0 < a < 1 */
    color: transparent;          /* equivalent to setting the alpha to 0 */
    color: hsl(0, 100%, 50%);    /* as hsl percentages (CSS 3) */
    color: hsla(0, 100%, 50%, 0.3); /* as hsla percentages with alpha */
    
    /* Images as backgrounds of elements */
    background-image: url(/img-path/img.jpg); /* quotes inside url() optional */
    
    /* Fonts */
    font-family: Arial;
    /* if the font family name has a space, it must be quoted */
    font-family: "Courier New";
    /* if the first one is not found, the browser uses the next, and so on */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}
```

## Usage

ஒரு css file ஐ  save செய்ய `.css`.

```xml
<!-- உங்கள் css file ஐ  <head>. உள் குறிப்பிட வேண்டும் 
     சரியான முறையை பார்க்க  http://stackoverflow.com/questions/8284365 -->
<link rel='stylesheet' type='text/css' href='path/to/style.css' />

<!-- நீங்கள் css ஐ html உள்ளும் எழுத முடியும் -->
<style>
   a { color: purple; }
</style>

<!-- அல்லது css ஐ நேரடியாக அந்த element இல் எழுத முடியும் -->
<div style="border: 1px solid red;">
</div>
```

## Precedence அல்லது Cascade

ஒரு element ஆனது ஒன்றுக்கு மேற்பட்ட selectors மூலம் அணுகபடலாம் ,இவ்வாறான சந்தர்பங்களில் 
ஒரு குறிபிட்ட விதிமுறையை பின்பற்றுகிறது இது cascading என அழைக்கபடுகிறது, அதனால் தன
இது  Cascading Style Sheets என அழைக்கபடுகிறது.


கிழே தரப்பட்டுள்ள css இன் படி:

```css
/* A */
p.class1[attr='value']

/* B */
p.class1 { }

/* C */
p.class2 { }

/* D */
p { }

/* E */
p { property: value !important; }
```

அத்துடன் கிழே தரப்பட்டுள்ள கட்டமைப்பின்படியும்:

```xml
<p style='/*F*/ property:value;' class='class1 class2' attr='value' />
```


css முன்னுரிமை பின்வருமாறு 
* `E` இதுவே அதிக முக்கியத்துவம் வாய்ந்தது காரணம் இது `!important` பயன்படுத்துகிறது. இதை பயன்படுத்துவதை தவிர்க்கவும்
* `F` இது இரண்டாவது காரணம் இது inline style.
* `A` இது  மூன்றவதாக வருகிறது, காரணம் இது மூன்று காரணிகளை குறிக்கிறது : element(உறுப்பு) பெயர் `p`, அதன் class `class1`, an அதன் பண்பு(attribute) `attr='value'`.
* `C` இது அடுத்த நிலையில் உள்ளது கடைசி.
* `B` இது அடுத்தது.
* `D` இதுவே கடைசி .

## Media Queries [மீடியா குரிஸ்]

CSS மீடியா குரிஸ் CSS 3 அம்சங்கள். பயன்படுத்தும் கணினி, கைபேசி அல்லது சாதனத்தின் பிஸேல் டென்சிட்டிக்கு ஏற்றவாறு மீடியா குரிஸ் விதிகளை பயன்படுத்தலாம்.

```css
/* அனைத்து டேவிஸ்களுக்கும் பொதுவான விதி */
h1 {
  font-size: 2em;
  color: white;
  background-color: black;
}

/* பிரிண்ட் செய்யும்போது h1 கலர் மாற்ற */
@media print {
  h1 {
    color: black;
    background-color: white;
  }
}

/* 480 பிஸேல்ளுக்கு மேல் சிகிரீன் அளவு உள்ள சாதனத்தில் எழுத்து அளவு மிகை படுத்த   */
@media screen and (min-width: 480px) {
  h1 {
    font-size: 3em;
    font-weight: normal;
  }
}
```

மீடியா குரிஸ் வழங்கும் அம்சங்கள் :
`width`, `height`, `device-width`, `device-height`, `orientation`, `aspect-ratio`, `device-aspect-ratio`, `color`, `color-index`, `monochrome`, `resolution`, `scan`, `grid`. இவையுள் பெரும்பான்மை `min-` அல்லது `max-` வுடன் பயன்படுத்தலாம் .

`resolution` பழைய சாதனங்களில் பயன்படாது, எனவே `device-pixel-ratio` பயன்படுத்தவும்.

பல கைபேசி மற்றும் கணினிகள், வீடு கணினி திரை அளவு காட்ட முற்படும். எனவே `viewport` மெட்டா டேக் பயன்படுத்தவும்.

```html
<head>
  <meta name="viewport" content="width=device-width; initial-scale=1.0">
</head>
```

## css அம்சங்களின் பொருந்தகூடிய தன்மை

பெரும்பாலான css 2 வின் அம்சங்கள் எல்லா உலாவிகளிலும் , கருவிகளிலும் உள்ளன. ஆனால் முன்கூட்டியே அந்த அம்சங்களை பரிசோதிப்பது நல்லது.

## வளங்கள்

* To run a quick compatibility check, [CanIUse](http://caniuse.com).
* CSS Playground [Dabblet](http://dabblet.com/).
* [Mozilla Developer Network's CSS documentation](https://developer.mozilla.org/en-US/docs/Web/CSS)
* [Codrops' CSS Reference](http://tympanus.net/codrops/css_reference/)

## மேலும் வாசிக்க

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [Selecting elements using attributes](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) and [LESS](http://lesscss.org/) for CSS pre-processing
* [CSS-Tricks](https://css-tricks.com)
