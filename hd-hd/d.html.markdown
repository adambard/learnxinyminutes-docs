---
language: D
filename: learnd-hd.d
contributors:
    - ["Nick Papanastasiou", "www.nickpapanastasiou.github.io"]
lang: hd
---

```c
//क्या आ रहा है पता है ...
module hello;

import std.stdio;

void main(string[] args) {
    writeln("Hello, World!");
}
```

अगर आप मेरे जैसे हैं और इंटरनेट पर समय बहुत अधिक समय खर्च करते हैं, तो आप बाधाओं के बारे में सुना है
के बारे में [डी ] ( http://dlang.org/ )। डी प्रोग्रामिंग भाषा में एक आधुनिक, सामान्य प्रयोजन है ,
सब कुछ के लिए समर्थन कम स्तर की सुविधाओं से करने के साथ बहु - प्रतिमान भाषा
अर्थपूर्ण उच्च स्तरीय चीजें ।

D सक्रिय रूप से सुपर स्मार्ट लोगों का एक बड़ा समूह द्वारा विकसित की है और नेतृत्व द्वारा किया जाता है
[ वाल्टर ब्राइट ] ( https://en.wikipedia.org/wiki/Walter_Bright ) और
[ आंद्रेई Alexandrescu ] ( https://en.wikipedia.org/wiki/Andrei_Alexandrescu )।
जिस तरह की है कि सभी के साथ बाहर, चलो कुछ उदाहरणों पर गौर करते हैं!


```c
import std.stdio;

void main() {

    for(int i = 0; i < 10000; i++) {
        writeln(i);
    }

    // 'auto' can be used for inferring types.
    auto n = 1;

    // संख्यात्मक literals स्पष्टता के लिए एक अंकों विभाजक के रूप में '_' का उपयोग कर सकते हैं।
    while(n < 10_000) {
        n += n;
    }

    do {
        n -= (n / 2);
    } while(n > 0);
    // लिए और जब तक अच्छा कर रहे हैं, लेकिन D में हम 'foreach' छोरों पसंद करते हैं।
    // '..' पहला मान सहित एक सतत श्रृंखला बनाता है,
    // लेकिन पिछले छोड़कर।
    foreach(i; 1..1_000_000) {
        if(n % 2 == 0)
            writeln(i);
    }

    // वहाँ भी 'foreach_reverse' आप पीछे की ओर पाश करना चाहते हैं।
    foreach_reverse(i; 1..int.max) {
        if(n % 2 == 1) {
            writeln(i);
        } else {
            writeln("No!");
        }
    }
}
```

हम ' struct`, `class`,` union`, और `` enum` साथ नए प्रकार परिभाषित कर सकते हैं। Structs और unions
मूल्य से कार्य करने के लिए पारित कर रहे हैं (यानी नकल) और वर्गों के संदर्भ द्वारा पारित कर रहे हैं। इसके अलावा,
हम प्रकारों और मानों दोनों पर करने के लिए टेम्पलेट का उपयोग कर सकते हैं!

```c
// इधर, 'T' एक प्रकार पैरामीटर है। लगता है कि '&lt;+T&gt;' C++ / C/ Java से।
struct LinkedList(T) {
    T data = null;

    // '!'का प्रयोग करें , एक पैरामिट्रीकृत प्रकार इन्स्तांत । फिर, '<T >' लगता है।
    LinkedList!(T)* next;
}

class BinTree(T) {
    T data = null;

// केवल एक टेम्पलेट पैरामीटर नहीं है, तो  , हम कोष्ठकों छोड़ सकते हैं।
    BinTree!T left;
    BinTree!T right;
}

enum Day {
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
}

// उपयोग उर्फ प्रकार (alias) के लिए संक्षिप्त बनाने के लिए।
alias IntList = LinkedList!int;
alias NumTree = BinTree!double;

//हम के रूप में अच्छी तरह से कार्य टेम्पलेट्स बना सकते हैं!
T max(T)(T a, T b) {
    if(a < b)
        return b;

    return a;
}

// संदर्भ द्वारा पारित सुनिश्चित करने के लिए रेफरी कीवर्ड का प्रयोग करें । यही कारण है कि यहां तक ​​कि 'A' और 'B' , तो है
//मान प्रकार वे हमेशा ' swap()' के संदर्भ द्वारा पारित हो जाएगा  हैं ।
void swap(T)(ref T a, ref T b) {
    auto temp = a;

    a = b;
    b = temp;
}

// टेम्पलेट्स के साथ, हम भी मूल्यों पर परमेटेराइज़  कर सकते हैं , न सिर्फ types.With टेम्पलेट्स, हम भी नहीं है, बस प्रकार , मूल्यों पर parameterize कर सकते हैं।
class Matrix(uint m, uint n, T = int) {
    T[m] rows;
    T[n] columns;
}

auto mat = new Matrix!(3, 3);

```

Classes की बात हो रही है , एक दूसरे के लिए गुणों के बारे में बात करते हैं। एक संपत्ति
एक value की तरह कार्य कर सकते हैं कि एक समारोह में मोटे तौर पर है, इसलिए हम कर सकते हैं
के शब्दों के साथ पॉड संरचनाओं की वाक्य रचना (` structure.x = 7` ) है
मनुष्य और सेटर तरीकों ( ` object.setX (7) `) !

```c
// Consider a class parameterized on types 'T' & 'U'.
class MyClass(T, U) {
    T _data;
    U _other;
}

// And "getter" and "setter" methods like so:
class MyClass(T, U) {
    T _data;
    U _other;

    // भवन निर्माताओं हमेशा नामित कर रहे हैं 'this'.
    this(T t, U u) {
        //यह नीचे सेटर तरीकों से मुलाकात करेंगे।
        data = t;
        other = u;
    }

    // getters
    @property T data() {
        return _data;
    }

    @property U other() {
        return _other;
    }

    // setters
    @property void data(T t) {
        _data = t;
    }

    @property void other(U u) {
        _other = u;
    }
}

//और हम इस तरह से उन का उपयोग करें :
void main() {
    auto mc = new MyClass!(int, string)(7, "seven");

   करने के लिए लिखने के लिए मानक पुस्तकालय से
   // आयात ' stdio ' मॉड्यूल
    // सांत्वना (आयात एक गुंजाइश के लिए स्थानीय हो सकता है) ।
    import std.stdio;

    // Call the getters to fetch the values.
    writefln("Earlier: data = %d, str = %s", mc.data, mc.other);

    // Call the setters to assign new values.
    mc.data = 8;
    mc.other = "eight";

    // Call the getters again to fetch the new values.
    writefln("Later: data = %d, str = %s", mc.data, mc.other);
}
```

गुणों के साथ, हम तर्क की किसी भी राशि को जोड़ सकते हैं
हमारे मनुष्य और सेटर तरीकों, और की साफ वाक्य रचना रखना
सीधे सदस्यों तक पहुँचने !

हमारे निपटान पर अन्य वस्तु उन्मुख उपहार
` interface`s , ` सार class`es शामिल
और ` तरीकों override`ing । डी सिर्फ जावा की तरह विरासत करता है:
आप कृपया के रूप में कई इंटरफेस को लागू करने, एक वर्ग का विस्तार ।

हम डी एस OOP सुविधाओं देखा , लेकिन स्विच गियर छोड़ दिया । डी प्रस्तावों
प्रथम श्रेणी के कार्यों के साथ कार्यात्मक प्रोग्रामिंग, ` pure`
काम करता है, और अपरिवर्तनीय डेटा । इसके अलावा, अपने पसंदीदा के सभी
कार्यात्मक एल्गोरिदम ( नक्शा, फिल्टर , कम करने और मित्र हो सकते हैं)
अद्भुत ` std.algorithm` मॉड्यूल में पाया!

```c
import std.algorithm : map, filter, reduce;
import std.range : iota; // builds an end-exclusive range

void main() {
    // हम भी ints के वर्गों की एक सूची का योग मुद्रित करना चाहते हैं
    // 1 से 100 के लिए आसान करने के लिए!

    // बस टेम्पलेट पैरामीटर के रूप में लैम्ब्डा भाव के पास!
    // आप आप की तरह किसी भी पुराने समारोह पारित कर सकते हैं , लेकिन lambdas यहाँ सुविधाजनक हैं।
    auto num = iota(1, 101).filter!(x => x % 2 == 0)
                           .map!(y => y ^^ 2)
                           .reduce!((a, b) => a + b);

    writeln(num);
}
```

हम NUM गणना करने के लिए एक अच्छा Haskellian पाइपलाइन का निर्माण करने के लिए मिला सूचना कैसे ?
यही कारण है कि एक डी नवाचार करने के लिए धन्यवाद वर्दी समारोह कॉल सिंटेक्स के रूप में जानते हैं।
UFCS के साथ, हम एक विधि के रूप में एक समारोह कॉल लिखने के लिए चुन सकते हैं
या मुफ्त समारोह कॉल ! वाल्टर इस पर एक अच्छा लेख लिखा था
[यहाँ ।] ( http://www.drdobbs.com/cpp/uniform-function-call-syntax/232700394 )
संक्षेप में, आप जिनकी पहली पैरामीटर कार्यों कॉल कर सकते हैं
एक विधि के रूप में ग्रुप ए की किसी भी अभिव्यक्ति पर कुछ प्रकार एक की है ।

मैं समानता चाहते । समानता की तरह कोई और? ज़रूर तुम करना। चलो कुछ करते हैं!
```c
import std.stdio;
import std.parallelism : parallel;
import std.math : sqrt;

void main() {
 // हम हमारे सरणी में वर्गमूल हर नंबर ले जाना चाहता हूँ ,
 // हम उपलब्ध है के रूप में और के रूप में कई कोर का लाभ ले।
    auto arr = new double[1_000_000];

   // संदर्भ के द्वारा एक सूचकांक , और एक सरणी तत्व का प्रयोग
    // और सिर्फ सरणी पर समानांतर फोन!
    foreach(i, ref elem; parallel(arr)) {
        ref = sqrt(i + 1.0);
    }
}


```
