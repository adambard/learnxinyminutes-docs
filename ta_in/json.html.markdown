---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
translators:
  - ["Rasendran Kirushan", "https://github.com/kirushanr"]
lang: ta-in
---

ஜேசன் ஒரு ஒரு மிக எளிய தரவு உள்மாற்றீட்டு வடிவம் ஆகும்.
Learn X in Y Minutes இதுவே மிகவும் இலகுவான பகுதியாக  அமைய போகிறது.


ஜேசன்  இன் எளிமையான கட்டமைப்பில்  குறிப்புக்கள் (Comments) இல்லை , எனினும் 
பெரும்பாலான  பாகுபடுத்திகளில் C - style  முறையிலான (`//`, `/* */`) குறிப்புகளை இட முடியும்.
சில பாகுபடுத்திகள்(interpreter) குறிப்புகளுக்கு (comments)தொடர்ச்சியாக வரும்
 காற்புள்ளியை  அனுமதிக்கின்றன (உதாரணமாக ஒரு அணியை (array) அடுத்துவரும் காற்புள்ளி
 அல்லது ஒரு பொருளில் (object)உள்ள கடைசி உறுப்பை/சொத்தை(  last property) அடுத்து வரும் காற்புள்ளி )
எனினும் சகல இடங்களிலும் ஜேசன் பயன்படுத்த பட வேண்டும் எனில் மேற்கூறிய குறிப்புகளை தவிர்த்தல் நல்லது .\


ஜேசன் 100% மிக சரியாக அமைவது மட்டும் இன்றி 
இலகுவாக புரியக் கூடிய எளிய தரவு உள்மாற்றீட்டு வடிவம் ஆகும்.


ஜேசன் அனுமதிக்கும் தரவு வகைகள் : சரம் (string),முழு (int),பூலியன் (தர்க ரீதியில் ஆன கட்டமைப்பு),
அணி (array ),கழி (null ),பொருள் (object).

ஜேசன் அனுமதிக்கும் அல்லது பாவனைக்கு உட்படுத்த கூடிய உலாவிகள் (browsers): 
Firefox(Mozilla) 3.5, Internet Explorer 8, Chrome, Opera 10, Safari 4.

ஜேசனின் கோப்புவகை(filetype)  ".json " ஆகும் .

ஜேசன் உரைக்கான MIME வகை   "application/json" ஆகும். 
ஜேசன் இல் காணப்படும் பிரதான பின்னடைவு தரவு இனம் இதுவென்று வரையறுக்க 
படாமை ஆகும் .

ஒரு ஜேசன் இன் எளிய கட்டமைப்பு கீழே காட்டப்பட்டுள்ளது 

```json
{
  "key": "ஒரு சாவிக்கு ஒரு பெறுமதி உள்ளது ",

  "keys": "சாவிகள் , மற்றும் பெறுமானங்கள் மேற்கோள் குறிக்குள் இடல் வேண்டும்",
  "numbers": 0,
  "strings": "Hellø, wørld. எல்லாவகையான  unicode உம் அனுமதிக்கப்படும், அத்துடன் \"escaping\".",
  "has bools?": true,
  "nothingness": null,

  "big number": 1.2e+100,

  "objects": {
    "comment": "பெரும்பாலான கட்டமைப்புகள் objects இல் இருந்தே வருகின்றன",

    "array": [0, 1, 2, 3, "array யானது எல்லாவகையான பெறுமானங்களையும் கொண்டிருக்கும்", 5],

    "another object": {
      "comment": "இவை ஒன்றுக்குள் இன்னொன்றை எழுத முடியும்"
    }
  },

  "silliness": [
    {
      "sources of potassium": ["வாழைபழம்"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "alternative style": {
    "comment": "இதை பார்க்கவும்"
  , "comma position": "doesn't matter - as long as it's before the value, then it's valid"
  , "another comment": "how nice"
  },

  "that was short": "நீங்கள் ஜேசன் பற்றி யாவற்றையும் கற்றுள்ளீர்கள்"
}
```

