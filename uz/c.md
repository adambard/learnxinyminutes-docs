---
contributors:
    - ["Adam Bard", "http://adambard.com/"]
    - ["Árpád Goretity", "http://twitter.com/H2CO3_iOS"]
    - ["Jakub Trzebiatowski", "http://cbs.stgn.pl"]
    - ["Marco Scannadinari", "https://marcoms.github.io"]
    - ["Zachary Ferguson", "https://github.io/zfergus2"]
    - ["himanshu", "https://github.com/himanshu81494"]
    - ["Joshua Li", "https://github.com/JoshuaRLi"]
    - ["Dragos B. Chirila", "https://github.com/dchirila"]
    - ["Heitor P. de Bittencourt", "https://github.com/heitorPB/"]
translators:
    - ["GitHub Copilot", "https://github.com/features/copilot"]
---

Ah, C. Hali ham **zamonaviy** yuqori samarali dasturlashning tili.

C - ko'pchilik dasturchilar ishlatadigan eng past darajadagi til, lekin
u o'zining tez ishlashi bilan buni to'liq qoplaydi. Faqat uning qo'lda
xotira boshqaruvidan xabardor bo'ling va C sizni kerak bo'lgan joyga olib boradi.

```c
// Bir qatorli izohlar // bilan boshlanadi - faqat C99 va undan keyingi versiyalarda.

/*
Ko'p qatorli izohlar shunday ko'rinadi. Ular C89 da ham ishlaydi.
*/

/*
Ko'p qatorli izohlar ichma-ich joylashmaydi /* Ehtiyot bo'ling */ // izoh shu qatorda tugaydi...
*/ // ...bu yerda emas!

// Konstantalar: #define <kalit so'z>
// Konstantalar odatda bosh harflar bilan yoziladi, bu shart emas, an'ana
#define YIL_KUNLARI 365

// Sanash konstantalari ham konstantalarni e'lon qilish usuli.
// Barcha operatorlar nuqta-vergul bilan tugashi kerak
enum kunlar {YAK, DUSH, SESH, CHOR, PAY, JUM, SHAN};
// YAK 0 ni oladi, DUSH 1 ni oladi, SESH 2 ni oladi, va hokazo.

// Sanash qiymatlari ham belgilanishi mumkin
enum kunlar {YAK = 1, DUSH, SESH, CHOR = 99, PAY, JUM, SHAN};
// DUSH avtomatik ravishda 2 ni oladi, SESH 3 ni oladi, va hokazo.
// CHOR 99 ni oladi, PAY 100 ni oladi, JUM 101 ni oladi, va hokazo.

// Sarlavha fayllarini #include bilan import qiling
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// <burchak qavslar> orasidagi fayl nomlari kompilyatorga tizim
// kutubxonalaringizdan sarlavhalarni qidirishni aytadi.
// O'zingizning sarlavhalaringiz uchun burchak qavslar o'rniga qo'sh tirnoq ishlatiladi:
#include "mening_sarlavham.h"        // mahalliy fayl
#include "../my_lib/my_lib_header.h" // nisbiy yo'l

// Funktsiya imzolarini oldindan .h faylida yoki .c faylingizning tepasida e'lon qiling.
void funktsiya_1();
int funktsiya_2(void);

// Eng kamida, har qanday funktsiyada ishlatishdan oldin 'funktsiya prototipi' ni e'lon qilishingiz kerak.
int ikkita_int_qosh(int x1, int x2); // funktsiya prototipi

// Dasturingizning kirish nuqtasi "main" deb ataladigan funktsiya. Qaytish turi
// har qanday bo'lishi mumkin, ammo ko'pgina operatsion tizimlar xato kodi qayta
// ishlash uchun `int` qaytish turini kutadi.
int main(void) {
  // sizning dasturingiz
}

// Dasturingizni ishga tushirish uchun ishlatilgan buyruq qatori argumentlari ham main ga uzatiladi
// argc - argumentlar soni - dastur nomi 1 sifatida hisoblanadi
// argv - belgilar massivlari massivi - argumentlarning o'zi
// argv[0] = dasturingiz nomi, argv[1] = birinchi argument, va hokazo.
int main (int argc, char** argv)
{
  // printf yordamida chiqarish, "formatted print" uchun
  // %d butun son, \n yangi qator
  printf("%d\n", 0); // => 0 ni chop etadi

  // scanf yordamida kirish
  // '&' kiritish qiymatini saqlashni xohlagan
  // joyni belgilash uchun ishlatiladi
  int kirish;
  scanf("%d", &kirish);

  ///////////////////////////////////////
  // Turlar
  ///////////////////////////////////////

  // C99 ga mos kelmaydigan kompilyatorlar o'zgaruvchilarning joriy blok
  // doirasining tepasida e'lon qilinishini TALAB qiladi.
  // C99 ga mos kelgan kompilyatorlar qiymat ishlatilgan joyga yaqin
  // e'lonlarga ruxsat beradi.

  // int lar odatda 4 bayt (tekshirish uchun `sizeof` operatoridan foydalaning)
  int x_int = 0;

  // short lar odatda 2 bayt
  short x_short = 0;

  // char lar protsessor uchun eng kichik manzilli birlik sifatida belgilanadi.
  // Bu odatda 1 bayt, lekin ba'zi tizimlar uchun ko'proq bo'lishi mumkin.
  char x_char = 0;
  char y_char = 'y'; // Char literallari '' bilan olinadi

  // long lar ko'pincha 4 dan 8 baytgacha; long long lar kamida 8 bayt kafolatlangan
  long x_long = 0;
  long long x_long_long = 0;

  // float lar odatda 32-bitli suzuvchi nuqta sonlari
  float x_float = 0.0f; // 'f' qo'shimchasi bu yerda suzuvchi nuqta literalini belgilaydi

  // double lar odatda 64-bitli suzuvchi nuqta sonlari
  double x_double = 0.0; // haqiqiy sonlar hech qanday qo'shimchasiz double hisoblanadi

  // butun son turlari ishoresiz bo'lishi mumkin (noldan katta yoki teng)
  unsigned short ux_short;
  unsigned int ux_int;
  unsigned long long ux_long_long;

  // bitta tirnoq ichidagi charlar mashina belgilar to'plamidagi butun sonlar.
  '0'; // => ASCII belgilar to'plamida 48.
  'A'; // => ASCII belgilar to'plamida 65.

  // sizeof(T) sizga T turi bilan o'zgaruvchining baytlardagi hajmini beradi
  printf("%zu\n", sizeof(int)); // => 4 (4-baytli so'zlar bilan ko'pgina mashinalarda)

  // Massivlar aniq hajmi bilan ishga tushirilishi kerak.
  char mening_char_massivim[20]; // Bu massiv 1 * 20 = 20 bayt egallaydi
  int mening_int_massivim[20]; // Bu massiv 4 * 20 = 80 bayt egallaydi

  // Yigirma intni hammasi 0 ga teng bo'lgan massivni shunday ishga tushirishingiz mumkin:
  int mening_massivim[20] = {0};

  // Massivni indekslash boshqa tillar kabi
  mening_massivim[0]; // => 0

  // Massivlar o'zgaruvchan; bu shunchaki xotira!
  mening_massivim[1] = 2;
  printf("%d\n", mening_massivim[1]); // => 2

  ///////////////////////////////////////
  // Operatorlar
  ///////////////////////////////////////

  // Qisqartma arifmetika
  int i = 0;
  i++; // i ni 1 ga oshirish. i = i + 1; ga teng
  ++i; // i ni 1 ga oshirish, yangi qiymatni qaytaradi
  i--; // i ni 1 ga kamaytirish. i = i - 1; ga teng
  --i; // i ni 1 ga kamaytirish, yangi qiymatni qaytaradi

  // Arifmetik operatorlar
  int j = 1 + 1; // => 2
  j = 2 - 1; // => 1
  j = 2 * 1; // => 2
  j = 1 / 2; // => 0 (0.5 emas chunki ikkala operand ham int)
  j = 3 % 2; // => 1

  // Taqqoslash operatorlari ehtimol tanish
  3 == 2; // => 0 (yolg'on)
  3 != 2; // => 1 (rost)
  3 > 2; // => 1
  3 < 2; // => 0
  2 <= 2; // => 1
  2 >= 2; // => 1

  // Mantiqiy operatorlar butun sonlar ustida ishlaydi
  !3; // => 0 (mantiqiy EMAS)
  !0; // => 1
  1 && 1; // => 1 (mantiqiy VA)
  0 && 1; // => 0
  0 || 1; // => 1 (mantiqiy YOKI)
  0 || 0; // => 0

  ///////////////////////////////////////
  // Boshqaruv tuzilmalari
  ///////////////////////////////////////

  // if shartli operator
  if (0) {
    printf("Men hech qachon chop etilmayman\n");
  } else if (0) {
    printf("Men ham hech qachon chop etilmayman\n");
  } else {
    printf("Men chop etilaman\n");
  }

  // While tsikli
  int ii = 0;
  while (ii < 10) {
      printf("%d, ", ii++); // ii++ i ni oshiradi, eski qiymatni qaytaradi
  } // => chop etadi "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  // For tsikli ham mavjud
  int jj;
  for (jj=0; jj < 10; jj++) {
      printf("%d, ", jj);
  } // => chop etadi "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  ///////////////////////////////////////
  // Funktsiyalar
  ///////////////////////////////////////

  // Funktsiya e'loni sintaksisi:
  // <qaytish turi> <funktsiya nomi>(<parametrlar>)

  int mening_funktsiyam(); // funktsiya e'loni

  // 0 qaytariladi
  return 0;
}

// Funktsiyalar argumentlarni qiymat bo'yicha oladi
void mening_funktsiyam() {
    printf("Men funktsiyaman!\n");
}

/*
C da satrlar faqat char massivlari bo'lib, null bayt (\0)
bilan tugaydi, odatda ASCII jadvalidagi 0-raqami.
*/

// Belgilar uchun escape ketma-ketliklari:
'\\'; // teskari slash
'\n'; // yangi qator
'\t'; // tab
'\v'; // vertikal tab
'\f'; // yangi sahifa
'\r'; // qaytarish
'\b'; // orqaga qaytish
'\0'; // null belgi
'\"'; // qo'sh tirnoq

```

## Qo'shimcha ma'lumotlar

O'zingizga [K&R, ya'ni "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language) nusxasini toping. Bu C haqidagi kitob bo'lib, C yaratuvchisi Dennis Ritchie va Brian Kernighan tomonidan yozilgan.

Agar savolingiz bo'lsa, [compl.lang.c Frequently Asked Questions](http://c-faq.com) ni o'qing.

To'g'ri bo'shliq, chekinish va umuman kodlash uslubingizda izchil bo'lish juda muhim.
O'qilishi mumkin bo'lgan kod aqlli va tez koddan yaxshiroqdir.